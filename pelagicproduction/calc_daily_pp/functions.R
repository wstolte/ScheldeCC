# functions.R — real columns + exact Eilers–Peeters formulation (clean)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(mgcv)
  library(zoo)
  library(tidymodels)
  library(modeltime)
  library(timetk)
  library(iml)
  library(patchwork)
})

# Small helper
`%||%` <- function(a,b) if (!is.null(a)) a else b


# ================================
# 0) Reduceer naar unieke datums (dagwaarden)
# ================================
collapse_daily_ts <- function(d) {
  cols <- intersect(c("PBmax","alpha","Eopt","Kd","chl_surface","bottomdepth"), names(d))
  
  out <- d %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cols),
        ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  if ("compartment" %in% names(d)) {
    out <- dplyr::mutate(out, compartment = dplyr::first(d$compartment))
  }
  out
}


# ================================
# 1) PAR uit KNMI en pp_wide (uniek per datum)
# ================================
compute_incident_PAR_knmi <- function(knmi_df, f_par = 0.45, umol_per_J = 4.57){
  req <- c('datum','STN','parameter','value','unit')
  stopifnot(all(req %in% names(knmi_df)))
  
  df <- knmi_df %>%
    mutate(date = as_date(datum)) %>%
    filter(parameter == 'Q') %>%
    group_by(date, unit) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  is_cm2 <- grepl('cm2', df$unit, fixed = TRUE)
  E_J_m2_day <- ifelse(is_cm2, df$value * 1e4, df$value)
  
  PAR_mol_m2_day <- f_par * E_J_m2_day * (umol_per_J * 1e-6)
  PAR_mean <- PAR_mol_m2_day * 1e6 / 86400
  
  tibble(date = df$date, PAR = PAR_mean) %>%
    group_by(date) %>%
    summarise(PAR = mean(PAR, na.rm = TRUE), .groups = "drop")
}

compute_incident_PAR_from_pp <- function(pp_df, f_par = 0.45, umol_per_J = 4.57){
  if (!'irradiance_J_cm2' %in% names(pp_df)) return(tibble(date = as_date(NA))[0,])
  
  pp_df %>%
    transmute(date = as_date(date),
              PAR_pp = f_par * irradiance_J_cm2 * 1e4 * (umol_per_J * 1e-6) * 1e6 / 86400) %>%
    group_by(date) %>%
    summarise(PAR_pp = mean(PAR_pp, na.rm = TRUE), .groups = "drop")
}

prefer_knmi_PAR <- function(knmi_par, pp_par){
  knmi1 <- knmi_par %>%
    group_by(date) %>%
    summarise(PAR = mean(PAR, na.rm = TRUE), .groups = "drop")
  
  pp1 <- pp_par %>%
    group_by(date) %>%
    summarise(PAR_pp = mean(PAR_pp, na.rm = TRUE), .groups = "drop")
  
  knmi1 %>%
    full_join(pp1, by = 'date') %>%
    mutate(PAR = coalesce(PAR, PAR_pp)) %>%
    select(date, PAR) %>%
    arrange(date)
}


# ================================
# 2) Eilers–Peeters (exacte 3‑parameter vorm)
# ================================
# PBmax: mg C (mg chla)^-1 h^-1
# alpha: mg C (mg chla)^-1 h^-1 per (umol m^-2 s^-1)
# Eopt : umol m^-2 s^-1
# E    : umol m^-2 s^-1
PP_EP_rate <- function(PBmax, alpha, Eopt, E){
  denom <- (E^2)/(alpha * Eopt^2) + (E/PBmax) - (2*E)/(alpha*Eopt) + (1/alpha)
  out <- E / denom
  out[out < 0] <- 0
  out
}

trapzR <- function(x, y){
  idx <- 2:length(x)
  sum((x[idx]-x[idx-1]) * (y[idx]+y[idx-1]) / 2)
}

compute_daily_PP_realcols <- function(date, PBmax, alpha, Eopt, Kd, chl_surface, PAR_surface,
                                      bottomdepth = 10, dz = 0.25, hours = 24){
  if (is.na(bottomdepth)) bottomdepth <- 10
  zs <- seq(0, bottomdepth, by = dz)
  E_z <- PAR_surface * exp(-Kd * zs)
  rate_h <- PP_EP_rate(PBmax, alpha, Eopt, E_z)
  chl_mg_m3 <- chl_surface
  vol_rate <- rate_h * chl_mg_m3
  areal_rate_day <- trapzR(zs, vol_rate) * hours
  tibble(date = as_date(date), PP_model = areal_rate_day)
}


# ================================
# 3) Modellen via tidymodels (LM/GAM/STM)
# ================================
fit_model_tidy_realcols <- function(df, model = c('lm','gam','stm'), stm_cfg = list()){
  model <- match.arg(model)
  
  # sort → per dag reduceren
  df <- df %>%
    dplyr::arrange(date) %>%
    collapse_daily_ts()
  
  req <- c('date','compartment','PBmax','alpha','Eopt','Kd','chl_surface')
  stopifnot(all(req %in% names(df)))
  param_cols <- c('PBmax','alpha','Eopt','Kd','chl_surface')
  
  # ---------- LM (lineaire interpolatie via recipe + zoo::na.approx) ----------
  if (model == 'lm') {
    
    build_wf <- function(y){
      rec <- recipe(as.formula(paste(y, "~ 1")), data = df) %>%
        step_mutate(
          !!rlang::ensym(y) := zoo::na.approx(!!rlang::ensym(y), na.rm = FALSE, rule = 2),
          .pkgs = "zoo"
        )
      
      mod <- null_model(mode = "regression") %>% set_engine("parsnip")
      workflow() %>% add_recipe(rec) %>% add_model(mod)
    }
    
    wfs  <- setNames(lapply(param_cols, build_wf), param_cols)
    fits <- purrr::imap(wfs, ~ fit(.x, data = df))
    
    preds_list <- purrr::imap(fits, function(wf_fit, nm){
      rec <- workflows::extract_recipe(wf_fit)
      baked <- bake(rec, new_data = df)
      tibble(date = df$date, !!nm := baked[[nm]])
    })
    
    params <- purrr::reduce(preds_list, left_join, by = "date") %>%
      dplyr::arrange(date) %>%
      dplyr::distinct(date, .keep_all = TRUE)
    
    return(list(type = "lm", workflows = fits, params = params, data = df))
  }
  
  # ---------- GAM (één mgcv::gam per parameter) ----------
  if (model == 'gam'){
    
    df_gam <- df %>%
      dplyr::mutate(
        date_num = as.numeric(date),
        doy      = lubridate::yday(date)
      )
    
    make_gam <- function(y) {
      fml <- as.formula(
        paste0('log(', y, ' + 1e-8) ~ s(date_num, bs="tp") + s(doy, bs="cc")')
      )
      mgcv::gam(fml, data = df_gam, method = "REML", na.action = na.exclude)
    }
    
    gams <- purrr::map(param_cols, make_gam)
    names(gams) <- param_cols
    
    preds_list <- purrr::imap(gams, function(gm, nm){
      tibble::tibble(
        date = df_gam$date,
        !!nm := exp(predict(gm, newdata = df_gam, type = "response"))
      )
    })
    
    params <- purrr::reduce(preds_list, dplyr::left_join, by = "date") %>%
      dplyr::arrange(date) %>%
      dplyr::distinct(date, .keep_all = TRUE)
    
    return(list(
      type   = "gam",
      gams   = gams,      # lijst met 5 mgcv::gam-objecten
      params = params,
      data   = df_gam     # bevat date_num en doy
    ))
  }
  
  # ---------- STM (Prophet via recipe; uitkomst log → skip bij bake) ----------
  if (model == 'stm'){
    K  <- stm_cfg$fourier_K  %||% 5
    c1 <- stm_cfg$bloom1_center %||% 75
    s1 <- stm_cfg$bloom1_sigma  %||% 10
    c2 <- stm_cfg$bloom2_center %||% 95
    s2 <- stm_cfg$bloom2_sigma  %||% 15
    
    build_wf <- function(y){
      rec <- recipe(as.formula(paste(y, '~ date')), data = df) %>%
        step_log(all_outcomes(), offset = 1e-8, skip = TRUE) %>%  # log-scale voor training, niet voor bake
        step_timeseries_signature(date) %>%
        step_rm(matches('(.iso$|.xts$|.wday.lbl$|.month.lbl$|.am.pm$)')) %>%
        step_mutate(index_num = as.numeric(date)) %>%
        step_fourier(date, K = K, period = 365.25) %>%
        step_mutate(
          bloom1 = exp(-0.5 * ((lubridate::yday(date) - c1)/s1)^2),
          bloom2 = exp(-0.5 * ((lubridate::yday(date) - c2)/s2)^2)
        ) %>%
        step_zv(all_predictors())
      
      mod <- prophet_reg(
        seasonality_yearly = TRUE,
        seasonality_weekly = FALSE,
        seasonality_daily  = FALSE
      ) %>% set_engine('prophet')
      
      workflow() %>% add_recipe(rec) %>% add_model(mod)
    }
    
    wfs  <- setNames(lapply(param_cols, build_wf), param_cols)
    fits <- purrr::imap(wfs, ~ fit(.x, data = df))
    
    preds_list <- purrr::imap(fits, function(wf_fit, nm){
      tibble(date = df$date, !!nm := exp(predict(wf_fit, new_data = df)$.pred))
    })
    
    params <- purrr::reduce(preds_list, left_join, by = 'date') %>%
      dplyr::arrange(date) %>%
      dplyr::distinct(date, .keep_all = TRUE)
    
    return(list(type='stm', workflows=fits, params=params, data=df, stm_cfg=stm_cfg))
  }
}


# ================================
# 4) Voorspellen + export (EP integratie) — join één-op-één
# ================================
make_predictions_realcols <- function(fit_obj, irradiance_df, compartment = NA_character_,
                                      out_dir = NULL, dz = 0.25, hours = 24, default_bottomdepth = 10){
  
  stopifnot('PAR' %in% names(irradiance_df))
  
  irr <- irradiance_df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::distinct(date, .keep_all = TRUE)
  
  prm <- fit_obj$params %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::distinct(date, .keep_all = TRUE)
  
  params <- dplyr::left_join(prm, irr, by = 'date')
  
  if (!('bottomdepth' %in% names(params)))
    params <- params %>% dplyr::mutate(bottomdepth = default_bottomdepth)
  
  # (optioneel) forceer atomic numeric:
  params <- params %>%
    dplyr::mutate(
      PBmax       = as.numeric(PBmax),
      alpha       = as.numeric(alpha),
      Eopt        = as.numeric(Eopt),
      Kd          = as.numeric(Kd),
      chl_surface = as.numeric(chl_surface),
      PAR         = as.numeric(PAR),
      bottomdepth = as.numeric(bottomdepth)
    )
  
  out <- pmap_dfr(
    params %>% dplyr::select(date, PBmax, alpha, Eopt, Kd, chl_surface, PAR, bottomdepth),
    ~ compute_daily_PP_realcols(..1, ..2, ..3, ..4, ..5, ..6, ..7,
                                bottomdepth = ..8, dz = dz, hours = hours)
  ) %>%
    dplyr::mutate(compartment = compartment)
  
  if (!is.null(out_dir)){
    readr::write_csv(out, file.path(out_dir, paste0('pp_predictions_', compartment, '.csv')))
    readr::write_csv(params, file.path(out_dir, paste0('parameter_estimates_', compartment, '.csv')))
  }
  out
}


# ================================
# 5) GAM componenten & diagnostiek
# ================================
plot_gam_components_realcols <- function(gam_obj, param = 'PBmax'){
  stopifnot(gam_obj$type == 'gam')
  gm <- gam_obj$gams[[param]]
  df <- gam_obj$data
  
  terms <- mgcv::predict.gam(gm, newdata = df, type = 'terms')
  comp_df <- as_tibble(terms) %>% mutate(date = df$date)
  
  comp_df %>%
    pivot_longer(cols = -date, names_to = 'component', values_to = 'contribution') %>%
    mutate(component_group = ifelse(grepl('doy', component), 'seasonal', 'long_term')) %>%
    group_by(date, component_group) %>%
    summarise(contribution = sum(contribution), .groups='drop') %>%
    ggplot(aes(date, contribution, color = component_group)) +
    geom_line() +
    labs(title = paste('GAM components:', param), y='Contribution (log-scale)') +
    theme_minimal()
}

plot_param_diagnostics_generic_realcols <- function(fit_obj, original_df, title_suffix=''){
  params_pred <- fit_obj$params
  original <- original_df %>% select(date, PBmax, alpha, Eopt, Kd, chl_surface)
  
  df <- left_join(original, params_pred, by = 'date', suffix = c('.obs','.fit'))
  vars <- c('PBmax','alpha','Eopt','Kd','chl_surface')
  
  plots <- purrr::map(vars, function(v){
    tibble(obs = df[[paste0(v,'.obs')]], fit = df[[paste0(v,'.fit')]]) %>%
      mutate(resid = obs - fit) %>%
      {
        p1 <- ggplot(., aes(fit, resid)) + geom_point(alpha=0.5) +
          labs(title=paste(v,'Residuals vs Fitted', title_suffix))
        p2 <- ggplot(., aes(obs, fit)) + geom_point(alpha=0.6) +
          geom_abline(lty=2) + labs(title=paste(v,'Observed vs Fitted', title_suffix))
        p1 + p2
      }
  })
  wrap_plots(plots, ncol = 2)
}


# ================================
# 6) STM SHAP
# ================================
compute_stm_shap_realcols <- function(stm_obj, parameter = 'PBmax'){
  stopifnot(stm_obj$type == 'stm')
  
  # 1) Workflow + recipe
  wf_fit <- stm_obj$workflows[[parameter]]
  rec    <- workflows::extract_recipe(wf_fit)
  
  # 2) Minimale new_data en feature-ruimte bakken
  data_tbl <- stm_obj$data %>% dplyr::select(date, all_of(parameter))
  x_mat <- bake(rec, new_data = data_tbl) %>%
    dplyr::select(-all_of(parameter))
  
  # 3) Voor iml: alleen numeric/factor
  x_mat_iml <- x_mat %>%
    dplyr::mutate(
      dplyr::across(where(is.character), as.factor),
      dplyr::across(
        where(~ inherits(.x, "Date") || inherits(.x, "POSIXt")),
        ~ as.numeric(.x)
      )
    )
  
  # 4) Predictor met expliciete predict.function
  predictor <- iml::Predictor$new(
    model = wf_fit,
    data  = x_mat_iml,
    y     = data_tbl[[parameter]],
    predict.function = function(mod, newdata){
      newdata <- tibble::as_tibble(newdata)
      # zet numeric 'date' terug naar Date voor recipe/predict
      if ("date" %in% names(newdata) && is.numeric(newdata$date)) {
        newdata$date <- as.Date(newdata$date, origin = "1970-01-01")
      }
      as.numeric(exp(predict(mod, new_data = newdata)$.pred))
    }
  )
  
  # 5) Kies een TIJDVOLGORDE subset (niet random), zodat je altijd meerdere datums hebt
  nS <- nrow(x_mat_iml)
  if (nS <= 1) {
    return(tibble::tibble(date = as.Date(character()), group = character(), phi = numeric()))
  }
  k   <- min(200, nS)                               # max 200 datums
  idx <- unique(floor(seq(1, nS, length.out = k)))  # gelijkmatig verdeeld in de tijd
  
  # 6) Feature-naam-groepen (Fourier -> seasonal; bloom1/2 -> spring_bloom; rest -> long_term)
  seasonal_cols <- names(x_mat_iml)[grepl('fourier', names(x_mat_iml))]
  bloom_cols    <- c('bloom1','bloom2')
  
  # 7) SHAP per rij (datum), direct de juiste datum eraan hangen
  one_shap <- function(i){
    shp <- iml::Shapley$new(predictor, x.interest = x_mat_iml[i, , drop = FALSE], sample.size = 50)
    res <- shp$results
    # sommige iml-versies leveren geen id-kolom; we hebben ‘m hier niet nodig
    res %>%
      dplyr::mutate(
        group = dplyr::case_when(
          .data$feature %in% seasonal_cols ~ 'seasonal',
          .data$feature %in% bloom_cols    ~ 'spring_bloom',
          TRUE                             ~ 'long_term'
        )
      ) %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(phi = sum(.data$phi), .groups = "drop") %>%
      dplyr::mutate(date = stm_obj$data$date[i]) %>%
      dplyr::select(date, group, phi)
  }
  
  # 8) Binden over de (tijd)subset
  shap_df <- purrr::map_dfr(idx, one_shap) %>%
    dplyr::arrange(date, group)
  
  shap_df
}

plot_stm_shap <- function(shap_df){
  shap_df %>%
    ggplot(aes(date, phi, color = group)) +
    geom_line() +
    labs(title='STM SHAP contributions', y='SHAP (log-scale)') +
    theme_minimal()
}

## 6.6 Vergelijk PP tussen modellen + observaties als punten
compare_pp_models_with_obs <- function(pred_list, comp_id, obs_df){
  
  # Observaties: zorg voor correcte kolomnamen & types
  obs2 <- obs_df %>%
    # mutate(compartment = dplyr::coalesce(compartment, compartiment)) %>%
    mutate(compartment = as.character(compartment)) %>%
    filter(compartment == as.character(comp_id)) %>%
    select(date, PP_observed) %>%
    # filter(year(date) < 2015)
  
  # Modelvoorspellingen (LM/GAM/STM) binden en linken aan observaties per date
  p_all <- bind_rows(
    pred_list$lm  %>% mutate(model = "LM"),
    pred_list$gam %>% mutate(model = "GAM"),
    pred_list$stm %>% mutate(model = "STM")
  ) %>%
    mutate(compartment = as.character(compartment)) %>%
    filter(compartment == as.character(comp_id)) %>%
    full_join(obs2, by = "date")
  
  ggplot(p_all, aes(x = date, y = PP_model, colour = model)) +
    geom_line(size = 1) +
    # Observaties: zwarte punten
    geom_point(aes(x = date, y = PP_observed),
               inherit.aes = FALSE, colour = "black", alpha = 0.85, size = 1.8) +
    scale_colour_manual(values = c(LM = "#1f77b4", GAM = "#2ca02c", STM = "#ff7f0e")) +
    labs(
      title = paste("PP vergelijking (lijnen) + Observaties (zwarte punten) —", comp_id),
      y = "Depth-integrated PP (mg C m^-2 d^-1)",
      colour = "Model",
      caption = "Zwarte punten = PP_observed"
    ) +
    theme_minimal()
}

