require(tidyverse)

knmi_daily <- read_delim(file = "data/KNMI/meteo/dailymeteo.csv", delim = ";")
knmi_daily_Q = knmi_daily %>% filter(parameter == "Q") 


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
  # filter(parameter == "PP_depth_integrated") %>%
  mutate(date = as.Date(datetime)) %>%
  select(compartiment, date, method, parameter, value, unit) %>%
  
  # coverage of parameters
  
  filter(
    parameter %in% c("PBmax_EP", "alfaB_EP", "Eopt_EP", "turbidity", "chlfa", "Kd", "conversionEtoC")
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(date, as.factor(parameter))) +
  geom_point(aes())



depth = 10 # m

# coverage of parameters

pp_long %>%
  # filter(parameter == "PP_depth_integrated") %>%
  mutate(date = as.Date(datetime)) %>%
  select(compartiment, date, method, parameter, value, unit) %>%
  filter(
    parameter %in% c("PBmax_EP", "alfaB_EP", "Eopt_EP", "turbidity", "chlfa", "Kd", "conversionEtoC")
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(date, as.factor(parameter))) +
  geom_point(aes())


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



pp_wide %>%
  # filter(year(date) < 2015) %>%
  write_csv("c:/temp/pp_wide.csv")
knmi_daily_Q %>% 
  write_csv("c:/temp/knmi_daily.csv")
