


NIOZpp <- read_delim("data/NIOZ/PPDick/bewerkt/ppp_berekend_per_vak_per_maand.csv", 
            delim = ";", comment = "#") %>%
  mutate(stationname = case_when(
    compartment == 1 ~ "Vlissingen",
    compartment == 3 ~ "Terneuzen",
    compartment == 4 ~ "Hansweert",
    compartment == 5 ~ "Baalhoek",
    compartment == 6 ~ "Bath",
    compartment == 7 ~ "Schaar van Ouden Doel",
    compartment == 9 ~ ""
  )) %>%
  select(-compartment) %>%
  mutate(source = "NIOZ", method = "FRRF") %>%
  mutate(valuesign = case_when(
    # year(date) == 2017 ~ ">", # check nog eens of deze wel of niet mee mogen doen.
    year(date) == 2018 & month(date) != 8 ~ ">",
    year(date) == 2019 ~ ">"
  )) %>%
  mutate(monthlyMean = ifelse(is.na(valuesign), monthlyMean, NA_real_))



RWSpp <-  read_delim("data/RWS/bewerkt/interpolated_ppp_berekend_per_station_per_maand.csv", 
                      delim = ";", comment = "#") %>%
  rename(compartment = Compartiment) %>%
  mutate(stationname = case_when(
    compartment == "1" ~ "Vlissingen",
    compartment == "3" ~ "Terneuzen",
    compartment == "4" ~ "Hansweert",
    compartment == "5" ~ "Baalhoek",
    compartment == "6" ~ "Bath",
    compartment == "7a" ~ "Schaar van Ouden Doel",
    compartment == "9" ~ ""
  )) %>%
  select(-compartment) %>%
  mutate(source = "RWS", method = "FRRF")



SMpp <- read_delim("data/Scheldemonitor/bewerkt/ppp_berekend_per_station_per_maand.csv", 
                   delim = ";", comment = "#") %>%
  filter(stationname %in% c("WS1", "WS4", "WS6", "WS10", "WS11")) %>%
  mutate(stationname = case_when(
    stationname == "WS1"  ~ "Vlissingen", 
    stationname == "WS4"  ~ "Terneuzen", 
    stationname == "WS6"  ~ "Hansweert", 
    stationname == "WS10" ~ "Bath", 
    stationname == "WS11" ~ "Schaar van Ouden Doel"
  )) %>%
  mutate(source = "Scheldemonitor", method = "14C")

stations <- c("Vlissingen", "Terneuzen", "Hansweert", "Baalhoek", "Bath", "Schaar van Ouden Doel")

allPP <- bind_rows(SMpp, NIOZpp, RWSpp)

badDates = allPP$date[!is.na(allPP$valuesign)]

allPP %>% 
  filter(stationname %in% stations) %>%
  mutate(stationname = factor(stationname, levels = stations)) %>%
  ggplot(aes(date, monthlyMean)) +
  geom_point(aes(color = method), size = 2) +
  facet_grid(stationname ~ .) +
  geom_vline(xintercept = badDates, alpha = 0.1, size = 1) +
  coord_cartesian(ylim = c(0,2)) +
  labs(subtitle = "monthly mean gross primary production in gC/m2/d") +
  ylab("PP in gC/m2/d") +
  theme_light() +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_x_date(breaks = scales::pretty_breaks(10))

debieten <- read_delim("data/Scheldemonitor/bewerkt/debieten_grens_berekend_per_maand.csv", 
                        delim = ";", comment = "#") %>%
  rename(debiet = monthlyMean)

allPP %>% left_join(debieten) %>% 
  filter(month %in% c(3,4,5,6,7,8)) %>%
  ggplot(aes(log(debiet), log(monthlyMean))) +
  geom_point(aes(color = stationname)) +
  geom_smooth(method = "glm", alpha = 0.05) +
  ggtitle("monthly mean discharge vs. primary production (growth season)") +
  ylab("log monthly mean production") +
  xlab("log monthly mean discharge in m3/s") +
  theme_light()

