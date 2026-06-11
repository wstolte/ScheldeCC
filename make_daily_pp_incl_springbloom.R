# =========================================================
# Primary production prediction with structural TS models + EP curve
# (Daylength integration, f_PAR=0.45, diagnostics & plots)
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(tidymodels)
  library(broom)
  library(splines)
  library(timetk)
})

# --- Config ---
cfg <- list(
  file_pp   = "temp/pp_wide.csv",
  file_knmi = "temp/knmi_daily.csv",
  par_knmi = "data/KNMI/meteo/dailyPAR.csv",
  out_dir   = "output_latest",
  fig_dir   = "output_latest/fig",
  
  # irradiance -> PAR conversion (customizable)
  frac_PAR  = 0.45,   # per your preference for NL greenhouse/field practice
  J_to_mol  = 4.6,    # µmol J^-1
  
  # integrate during daylight (not 24h)
  use_daylength = TRUE,
  lat_deg   = 51.4,         # approx Westerschelde; adjust if needed
  tz        = "Europe/Amsterdam",
  
  # Seasonality & trend
  K_fourier = 3,    # seasonal Fourier order
  df_trend  = 1,    # ns() degrees of freedom for long-term trend
  bloom_mus = c(70, 85, 100, 115),  # DOY centers
  bloom_sigma = 15, # days (width)
  
  # Parameters to model  (names -> column names in pp_wide.csv)
  params = c(
    PBmax = "PBmax",
    alpha = "alpha",
    Eopt  = "Eopt",
    Kd    = "Kd",
    Chl   = "chl_surface"
  ),
  
  # date range for daily predictions
  start_date = as.Date("1990-01-01"),
  end_date   = as.Date("2024-12-31")
)

dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cfg$fig_dir, showWarnings = FALSE, recursive = TRUE)

# --- Helpers ---

daylength_hours <- function(date, lat_deg = cfg$lat_deg) {
  d <- yday(date)
  lat <- lat_deg * pi/180
  delta <- 23.45 * sin(2*pi*(284 + d)/365) * pi/180
  cos_omega0 <- -tan(lat) * tan(delta)
  cos_omega0 <- pmax(pmin(cos_omega0,  1), -1)
  omega0 <- acos(cos_omega0)
  2 * omega0 / (2*pi) * 24
}

knmi_par <- read_delim(cfg$par_knmi, delim = ";")

# Conversion refs: bigleaf::Rg.to.PPFD (frac_PAR, 4.6 µmol J^-1).  # [4](https://search.r-project.org/CRAN/refmans/bigleaf/html/Rg.to.PPFD.html)[5](https://rdrr.io/cran/bigleaf/man/Rg.to.PPFD.html)

ep_rate <- function(E, alpha, Eopt, PBmax) {
  denom <- (E^2)/(alpha * Eopt^2) + (E/PBmax) - (2*E)/(alpha*Eopt) + 1/alpha
  E / denom
}
# EP formula and parameterization per phytotools::fitEP documentation.  # [1](https://rdrr.io/cran/phytotools/man/fitEP.html)[2](https://www.rdocumentation.org/packages/phytotools/versions/1.0/topics/fitEP)[3](https://github.com/cran/phytotools/blob/master/man/fitEP.Rd)

pp_depth_integrated <- function(E0, Kd, Z, Chl_mg_m3, alpha, Eopt, PBmax, hours_day) {
  if (any(is.na(c(E0, Kd, Z, Chl_mg_m3, alpha, Eopt, PBmax, hours_day)))) return(NA_real_)
  fz <- function(z) {
    Ez <- E0 * exp(-Kd * z)
    PB <- ep_rate(E = Ez, alpha = alpha, Eopt = Eopt, PBmax = PBmax) # mgC mgChl^-1 h^-1
    PB * Chl_mg_m3                                                   # mgC m^-3 h^-1
  }
  val <- try(stats::integrate(fz, lower = 0, upper = Z)$value, silent = TRUE)
  if (inherits(val, "try-error")) return(NA_real_)
  as.numeric(val) * hours_day                                        # mgC m^-2 d^-1
}

bloom_bases <- function(doy, mus = cfg$bloom_mus, sigma = cfg$bloom_sigma) {
  purrr::map_dfc(mus, ~ tibble(!!paste0("bloom_", .x) := exp(-0.5 * ((doy - .x)/sigma)^2)))
}

# --- Load data ---

pp <- readr::read_csv(cfg$file_pp, show_col_types = FALSE) %>%
  # janitor::clean_names() %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(PBmax))

# diepte niet opgenomen in deze informatie; zet op 10 m
depth_per_comp <- pp %>%
  group_by(compartiment) %>%
  # summarize(bottomdepth = suppressWarnings(median(bottomdepth, na.rm = TRUE)), .groups = "drop")
  summarize(bottomdepth = 10, .groups = "drop")

# --- Visusal inspection --------

pp %>%
  filter(
    TRUE
  ) %>%
  select(
    -turbidity
  ) %>%
  pivot_longer(
    cols = -c(compartiment, date, conversionEtoC), 
    names_to = "parameter", 
    values_to = "value") %>%
  mutate(month = month(date)) %>%
  ggplot(aes(month, value)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(
    ~compartiment + parameter,
    scales = "free", 
    ncol = 6
    )
ggsave(file.path(pp_dir, "observations.png"), width = 12, height = 9)

pp %>%
  filter(
    TRUE
  ) %>%
  select(
    -turbidity
  ) %>%
  pivot_longer(
    cols = -c(compartiment, date, conversionEtoC), 
    names_to = "parameter", 
    values_to = "value") %>%
  mutate(month = month(date)) %>%
  ggplot(aes(month, value)) +
  geom_smooth(method = "loess", span = 0.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(
    ~compartiment + parameter,
    scales = "free", 
    ncol = 6
  )
ggsave(file.path(pp_dir, "observation_loess.png"), width = 12, height = 9)


7# --- Modeling functions ---

fit_model <- function(data_param,
                      param_col,
                      K_fourier = cfg$K_fourier,
                      df_trend  = cfg$df_trend,
                      bloom_mus = cfg$bloom_mus,
                      bloom_sigma = cfg$bloom_sigma) {
  
  df <- data_param %>%
    transmute(
      compartiment,
      date,
      y = .data[[param_col]]
    ) %>%
    filter(!is.na(y)) %>%
    mutate(
      doy  = yday(date),
      tnum = as.numeric(date) / (24*3600)
    ) %>%
    bind_cols(bloom_bases(.$doy, mus = bloom_mus, sigma = bloom_sigma))

  rec <- recipe(y ~ ., data = df) %>%
    update_role(compartiment, new_role = "id") %>%  # keep as id
    step_fourier(date, K = K_fourier, period = 365) %>%
    step_ns(tnum, deg_free = df_trend) %>%
    step_rm(date, doy)
  
  mod <- linear_reg() %>% set_engine("lm")
  wf  <- workflow() %>% add_model(mod) %>% add_recipe(rec)
  
  df %>%
    nest(.by = compartiment) %>%
    # <-- put the key back into each nested tibble:
    mutate(data = purrr::map2(data, compartiment, ~ dplyr::mutate(.x, compartiment = .y))) %>% 
    mutate(
      fit = map(data, ~ fit(wf, data = .x)),
      # diagnostics on training
      aug = map2(fit, data, ~ augment(.x, new_data = .y) %>% mutate(resid = y - .pred)),
      glance = map(fit, glance),
      # coef table
      tidy   = map(fit, tidy)
    ) %>%
    select(compartiment, fit, aug, glance, tidy)
}

term_groups <- list(
  trend    = c("^ns\\("),
  seasonal = c("^sin_", "^cos_"),
  bloom    = c("^bloom_")
)

# necessary for shapley component decomposition
predict_components_lm <- function(fit, new_data, groups = term_groups) {
  
  eng <- parsnip::extract_fit_engine(fit)
  mat <- predict(eng, newdata = new_data, type = "terms")
  const <- as.numeric(attr(mat, "constant"))
  
  sum_by_pattern <- function(patterns) {
    cols <- unique(unlist(purrr::map(patterns, \(p) grep(p, colnames(mat), value = TRUE))))
    if (length(cols) == 0) rep(0, nrow(mat)) else rowSums(mat[, cols, drop = FALSE])
  }
  
  comp_trend    <- sum_by_pattern(groups$trend)
  comp_seasonal <- sum_by_pattern(groups$seasonal)
  comp_bloom    <- sum_by_pattern(groups$bloom)
  
  tibble::tibble(
    date      = new_data$date,
    intercept = const,
    trend     = const + comp_trend,
    seasonal  = const + comp_seasonal,
    bloom     = const + comp_bloom,
    total     = const + rowSums(mat)
  ) |>
    tidyr::pivot_longer(-date, names_to = "component", values_to = "value")
}

# necessary for shapley component decompostion
compute_shapley <- function(pp_subsets_wide) {
  pp_subsets_wide %>%
    mutate(
      phi_trend =
        (trend - none)/3 +
        (ts - seasonal)/6 +
        (tb - bloom)/6 +
        (tsb - sb)/3,
      
      phi_seasonal =
        (seasonal - none)/3 +
        (ts - trend)/6 +
        (sb - bloom)/6 +
        (tsb - tb)/3,
      
      phi_bloom =
        (bloom - none)/3 +
        (tb - trend)/6 +
        (sb - seasonal)/6 +
        (tsb - ts)/3,
      
      PP_total     = tsb,
      PP_intercept = none
    ) %>%
    select(compartiment, date,
           PP_total, PP_intercept,
           phi_trend, phi_seasonal, phi_bloom)
}

make_predictions <- function(models_list,
                             knmi_par_df,
                             depth_lookup,
                             alt_chl = NULL,
                             start_date = cfg$start_date,
                             end_date   = cfg$end_date,
                             use_daylight_mean = TRUE) {
  
  # dates to predict
  pred_dates <- tibble(date = seq.Date(start_date, end_date, by = "day")) %>%
    mutate(
      tnum = as.numeric(date) / (24*3600),
      doy  = lubridate::yday(date)
    ) %>%
    bind_cols(bloom_bases(.$doy))  # Gaussian bloom bases
  
  # irradiance driver (E0) + daylight hours for integration
  E_df <- knmi_par_df %>%
    transmute(
      date,
      hours_day,
      E0 = if (use_daylight_mean) {
        dplyr::coalesce(PPFD_daylight, PPFD_24h)
      } else {
        PPFD_24h
      }
    )
  
  # predict each parameter per compartment on pred_dates
  preds <- purrr::imap(models_list, function(mtbl, pname) {
    mtbl %>%
      mutate(
        pred = purrr::map2(
          fit,
          compartiment,
          ~ predict(
            .x,
            new_data = pred_dates %>%
              dplyr::mutate(compartiment = .y)
          ) %>%
            dplyr::mutate(date = pred_dates$date) %>%
            dplyr::rename(.pred = .pred)
        )
        # Alternative:
        # pred = purrr::map(fit, ~ predict(.x, new_data = pred_dates) %>%
        #                      mutate(date = pred_dates$date) %>%
        #                      rename(.pred = .pred))
      ) %>%
      select(compartiment, pred) %>%
      unnest(pred) %>%
      mutate(param = pname)
  }) %>%
    list_rbind()
  
  # to wide: one column per parameter
  pars_wide <- preds %>%
    select(compartiment, date, param, .pred) %>%
    pivot_wider(names_from = param, values_from = .pred)
  
  # optional override of Chlorophyll a
  if (!is.null(alt_chl)) {
    alt_chl2 <- alt_chl %>%
      transmute(compartiment, date = as.Date(date), chl_alt = chl)
    pars_wide <- pars_wide %>%
      left_join(alt_chl2, by = c("compartiment", "date")) %>%
      mutate(Chl = coalesce(chl_alt, Chl)) %>%
      select(-chl_alt)
  }
  
  # join E0 (PAR at surface), depth, compute depth-integrated PP
  pars_wide %>%
    left_join(E_df,        by = "date") %>%
    left_join(depth_lookup, by = "compartiment") %>%
    mutate(
      PP_mgC_m2_d = pmap_dbl(
        list(E0, Kd, bottomdepth, Chl, alpha, Eopt, PBmax, hours_day),
        ~ pp_depth_integrated(
          E0 = ..1, Kd = ..2, Z = ..3,
          Chl_mg_m3 = ..4, alpha = ..5, Eopt = ..6,
          PBmax = ..7, hours_day = ..8
        )
      )
    )
}

make_predictions_with_shapley <- function(models_list,
                                          knmi_par_df,
                                          depth_lookup,
                                          alt_chl = NULL,
                                          start_date = cfg$start_date,
                                          end_date   = cfg$end_date,
                                          use_daylight_mean = TRUE) {
  
  # --- 1. base pred_dates (zoals jij al doet) ---
  base_dates <- tibble(date = seq.Date(start_date, end_date, by = "day"))
  
  pred_dates <- base_dates %>%
    mutate(
      tnum = as.numeric(date) / (24*3600),
      doy  = lubridate::yday(date)
    ) %>%
    bind_cols(bloom_bases(.$doy))
  
  # helper: zet componenten "uit"
  modify_subset <- function(df, subset) {
    df2 <- df
    
    if (!grepl("t", subset)) {
      df2$tnum <- mean(df2$tnum)
    }
    if (!grepl("s", subset)) {
      df2$date <- base_dates$date[1]  # vaste datum → geen seizoen
    }
    if (!grepl("b", subset)) {
      df2 <- df2 %>%
        mutate(across(starts_with("bloom_"), ~0))
    }
    
    df2
  }
  
  subsets <- c("none","trend","seasonal","bloom",
               "ts","tb","sb","tsb")
  
  subset_df <- purrr::map_dfr(subsets, function(s) {
    modify_subset(pred_dates, s) %>%
      mutate(subset = s)
  })
  
  # --- 2. E0 en hours ---
  E_df <- knmi_par_df %>%
    transmute(
      date,
      hours_day,
      E0 = if (use_daylight_mean) {
        dplyr::coalesce(PPFD_daylight, PPFD_24h)
      } else {
        PPFD_24h
      }
    )
  
  # --- 3. voorspellen per subset (zelfde stijl als jouw code!) ---
  preds <- purrr::imap(models_list, function(mtbl, pname) {
    
    mtbl %>%
      mutate(
        pred = purrr::map2(
          fit, compartiment,
          ~ predict(
            .x,
            new_data = subset_df %>%
              mutate(compartiment = .y)
          ) %>%
            mutate(
              date   = subset_df$date,
              subset = subset_df$subset
            )
        )
      ) %>%
      select(compartiment, pred) %>%
      unnest(pred) %>%
      mutate(param = pname)
    
  }) %>%
    list_rbind()
  
  # --- 4. naar wide parameters ---
  pars_wide <- preds %>%
    select(compartiment, date, subset, param, .pred) %>%
    pivot_wider(names_from = param, values_from = .pred)
  
  # --- optional chl override ---
  if (!is.null(alt_chl)) {
    alt_chl2 <- alt_chl %>%
      transmute(compartiment, date = as.Date(date), chl_alt = chl)
    
    pars_wide <- pars_wide %>%
      left_join(alt_chl2, by = c("compartiment", "date")) %>%
      mutate(Chl = coalesce(chl_alt, Chl)) %>%
      select(-chl_alt)
  }
  
  # --- 5. PP berekenen per subset ---
  pp_subsets <- pars_wide %>%
    left_join(E_df, by = "date") %>%
    left_join(depth_lookup, by = "compartiment") %>%
    mutate(
      PP = pmap_dbl(
        list(E0, Kd, bottomdepth, Chl, alpha, Eopt, PBmax, hours_day),
        ~ pp_depth_integrated(
          E0 = ..1, Kd = ..2, Z = ..3,
          Chl_mg_m3 = ..4, alpha = ..5, Eopt = ..6,
          PBmax = ..7, hours_day = ..8
        )
      )
    )
  
  # --- 6. shapley ---
  pp_wide <- pp_subsets %>%
    select(compartiment, date, subset, PP) %>%
    pivot_wider(names_from = subset, values_from = PP)
  
  shapley <- compute_shapley(pp_wide)
  
  shapley
}


models <- list()
for (nm in names(cfg$params)) {
  col <- cfg$params[[nm]]
  message("Fitting parameter: ", nm, " (", col, ")")
  models[[nm]] <- fit_model(pp, param_col = col)
}

# --- Save diagnostics ---
diag_dir <- file.path(cfg$out_dir, "diag")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

save_param_diagnostics <- function(models_for_param, pname) {
  # Stats (R2, RMSE, MAE), coef tables, residuals
  stats <- models_for_param %>%
    mutate(
      rmse = map_dbl(aug, ~ rmse_vec(truth = .x$y, estimate = .x$.pred)),
      mae  = map_dbl(aug, ~ mae_vec (truth = .x$y, estimate = .x$.pred)),
      r_sq = map_dbl(glance, ~ .x$r.squared %||% NA_real_)
    ) %>%
    transmute(compartiment, param = pname, r_sq, rmse, mae)
  
  readr::write_csv(stats, file.path(diag_dir, paste0("fitstats_", pname, ".csv")))
  
  coefs <- models_for_param %>%
    select(compartiment, tidy) %>%
    unnest(tidy) %>%
    mutate(param = pname, .before = 1)
  
  readr::write_csv(coefs, file.path(diag_dir, paste0("coefs_", pname, ".csv")))
  
  # Plots: observed vs predicted; residuals
  plot_dir <- file.path(cfg$fig_dir, paste0("param_", pname))
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  pwalk(
    list(models_for_param$compartiment, models_for_param$aug),
    function(comp, aug_df) {
      p1 <- ggplot(aug_df, aes(.pred, y)) +
        geom_point(alpha = 0.7) + geom_abline(slope = 1, intercept = 0, linetype = 2) +
        labs(title = paste(pname, "- Predicted vs Observed (comp", comp, ")"),
             x = "Voorspeld", y = "Geobserveerd") +
        theme_minimal()
      ggsave(file.path(plot_dir, paste0("pred_obs_comp", comp, ".png")), p1, width = 5.5, height = 4)
      
      p2 <- ggplot(aug_df, aes(x = date, y = resid)) +
        geom_hline(yintercept = 0, color = "grey60") + geom_line() +
        labs(title = paste(pname, "- Residuen in de tijd (comp", comp, ")"),
             x = "Datum", y = "Residuen") +
        theme_minimal()
      ggsave(file.path(plot_dir, paste0("residuals_comp", comp, ".png")), p2, width = 6.5, height = 3.8)
    }
  )
}

iwalk(models, save_param_diagnostics)


# --- Predict daily & compute PP (daylight integration) ---
preds <- make_predictions(
  models_list = models,
  knmi_par_df = knmi_par,
  depth_lookup = depth_per_comp,
  alt_chl = NULL,
  start_date = cfg$start_date,
  end_date   = cfg$end_date,
  use_daylight_mean = TRUE
)

preds_shapley <- make_predictions_with_shapley(
  models_list = models,
  knmi_par_df = knmi_par,
  depth_lookup = depth_per_comp,
  alt_chl = NULL,
  start_date = cfg$start_date,
  end_date   = cfg$end_date,
  use_daylight_mean = TRUE
)

readr::write_csv(preds, file.path(cfg$out_dir, "daily_PP_predictions.csv"))

readr::write_csv(preds_shapley, file.path(cfg$out_dir, "daily_PP_predictions_shapley.csv"))

ann <- preds %>%
  group_by(compartiment, year = year(date)) %>%
  summarize(PP_annual_gCm2 = sum(PP_mgC_m2_d, na.rm = TRUE)/1000, .groups = "drop")

readr::write_csv(ann, file.path(cfg$out_dir, "annual_summaries.csv"))

# --- Plots: daily PP per compartment ---
pp_dir <- file.path(cfg$fig_dir, "daily_PP")
dir.create(pp_dir, showWarnings = FALSE, recursive = TRUE)

pp_val <- readr::read_delim("data\\Deltares\\combined_ppp\\monthly_mean_integrated_ppp.csv", delim = ";") %>%
  mutate(
    date = lubridate::ymd(paste(year, month, "15"))) %>%
  select(
    compartiment,
    date,
    PP_validation = monthlyMean
  )

preds %>%
  left_join(pp_val) %>% 
  group_split(compartiment) %>%
  walk(function(df) {
    comp <- df$compartiment[1]
    p <- ggplot(df, aes(date, PP_mgC_m2_d)) +
      geom_line(color = "steelblue") +
      geom_point(aes(y = PP_validation, color = "observed")) +
      labs(title = paste("Dagelijkse primaire productie (comp", comp, ")"),
           x = "Datum", y = "mg C m^-2 d^-1") +
      theme_minimal() +
      scale_x_date(breaks = "5 year", date_labels = "%Y")
    ggsave(file.path(pp_dir, paste0("PP_timeseries_comp", comp, ".png")), p, width = 9, height = 4)
  })



plot_pp_shapley <- function(shapley_df) {
  shapley_long <- shapley_df %>%
    pivot_longer(
      c(phi_trend, phi_seasonal, phi_bloom),
      names_to = "component",
      values_to = "PP_contrib"
    ) %>%
    mutate(component = recode(component,
                              phi_trend = "Trend",
                              phi_seasonal = "Seasonal",
                              phi_bloom = "Bloom"))
  
  ggplot(shapley_long,
         aes(x = date, y = PP_contrib, fill = component)) +
    geom_area(alpha = 0.7) +
    facet_wrap(~ compartiment, scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) +
    labs(
      title = "Shapley Decomposition of Primary Production",
      x = NULL,
      y = "Contribution to PP (mg C m⁻² d⁻¹)",
      fill = "Component"
    )
}



message("Done. See '", cfg$out_dir, "' for tables and '", cfg$fig_dir, "' for figures.")


# alt_chl <- tibble(
#   compartiment = c(1,1,2, ...),
#   date = as.Date(c("2011-03-01", ...)),
#   chl  = c(12.5, ...)  # mg m^-3 (== µg L^-1)
# )
# 
# # Using alternative Chlorophyll a measurements
# preds_alt <- make_predictions(
#   models_list = models,
#   knmi_par_df = knmi_par,
#   depth_lookup = depth_per_comp,
#   alt_chl = alt_chl,
#   start_date = cfg$start_date,
#   end_date   = cfg$end_date,
#   use_daylight_mean = TRUE
# )

# Plot annual summaries

file = "output_latest\annual_summaries.csv"
ann_summ <- read_csv(file)
