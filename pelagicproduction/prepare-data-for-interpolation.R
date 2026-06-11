require(tidyverse)
require(plotly)

calc_par_from_knmi <- function(knmi_df,
                               frac_PAR = cfg$frac_PAR,
                               J_to_mol = cfg$J_to_mol,
                               use_daylength = cfg$use_daylength,
                               lat_deg = cfg$lat_deg) {
  knmi_df %>%
    mutate(
      J_m2_day = value * 1e4,          # J/m2/day  (J/cm2 -> J/m2)
      W_m2_avg = J_m2_day / 86400,     # mean over 24h
      PPFD_24h = W_m2_avg * frac_PAR * J_to_mol,    # µmol m-2 s-1
      DLI_mol  = PPFD_24h * 86400 / 1e6
    ) %>%
    mutate(
      hours_day = if (use_daylength) daylength_hours(datum, lat_deg) else 24,
      PPFD_daylight = DLI_mol * 1e6 / (hours_day * 3600)
    )
}

## only necessary when new or more KNMI data are available
knmi_daily <- read_delim(file = "data/KNMI/meteo/dailymeteo.csv", delim = ";")
knmi_daily_Q = knmi_daily %>% filter(parameter == "Q")

knmi_par <- calc_par_from_knmi(knmi_daily_Q,
                               frac_PAR = cfg$frac_PAR,
                               J_to_mol = cfg$J_to_mol,
                               use_daylength = cfg$use_daylength,
                               lat_deg = cfg$lat_deg) %>%
  select(date = datum, STN, PPFD_24h, DLI_mol, hours_day, PPFD_daylight)

write_delim(knmi_par, "data/KNMI/meteo/dailyPAR.csv", delim = ";")


filelist <- list.files("data/Deltares/combined_ppp/", full.names = T)
filelist <- filelist[!grepl("old", filelist)]
filelist <- filelist[!grepl("mean", filelist)]

pp_long <- lapply(filelist,
                  \(x) read_delim(x, delim = ";")
) %>%
  map(
    \(x) x %>% mutate(compartiment = as.character(compartiment))
  ) %>%
  bind_rows() %>%
  mutate(
    compartiment = case_when(
      grepl("7", compartiment) ~ "7",
      .default = compartiment
    )
  )

pp_long %>% distinct(parameter)

require(magrittr)

pp_long %>%
  sample_n(50000) %>%
  # filter(parameter == "PP_depth_integrated") %>%
  mutate(date = as.Date(datetime)) %>%
  select(compartiment, date, method, parameter, value, unit) %>%
  # coverage of parameters
  filter(
    parameter %in% c("PBmax_EP", "alfaB_EP", "Eopt_EP", "turbidity", "chlfa", "Kd", "conversionEtoC")
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(date, as.factor(parameter))) +
  # geom_point(aes()) +
  geom_point(aes(y = value)) +
  facet_grid(parameter ~ ., scales = "free_y")

depth = 10 # m

pp_wide <- pp_long %>%
  # filter(parameter == "PP_depth_integrated") %>%
  mutate(date = as.Date(datetime)) %>%
  select(compartiment, date, parameter, value) %>%
  group_by(compartiment, date, parameter) %>%
  summarise(value = mean(value, na.rm = T), .groups = "drop") %>%
  filter(
    parameter %in% c("PBmax_EP", "alfaB_EP", "Eopt_EP", "turbidity", "chlfa", "Kd", "conversionEtoC")
  ) %>%
  filter(!is.na(value)) %>% 
  pivot_wider(id_cols = c(compartiment, date), names_from = parameter, values_from = value) %>%
  rename(
    Kd = Kd,
    PBmax = PBmax_EP, 
    alpha = alfaB_EP,
    chl_surface = chlfa,
    Eopt = Eopt_EP
  ) %>%
  filter(!is.na(compartiment)) %>%
  left_join(knmi_daily_Q %>% select(date = datum, irradiance_J_cm2 = value))

## Checks

pl <- pp_wide %>%
  ggplot(aes(x = chl_surface, y = PBmax, color = as.factor(year(date)))) +
  geom_point() +
  facet_wrap("compartiment", scales = "free_y")

ggplotly(pl)
# chlorophyll > 40 seems to be an outlier (propose to take out. )

pp_wide %>%
  ggplot(aes(x = chl_surface)) +
  geom_histogram(aes(fill = as.factor(year(date))))

pp_wide <- pp_wide %>%
  # filter(year(date) < 2015) %>%
  filter(chl_surface <= 40)

  write_csv(pp_wide, "temp/pp_wide.csv")

knmi_daily_Q %>% 
  write_csv("temp/knmi_daily.csv")
