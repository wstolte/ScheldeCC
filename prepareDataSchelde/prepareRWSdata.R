## Prepare RWS primary production data
require(tidyverse)
require(data.table)

# fix locations

# read csv
# make wkt field (list of lists)
# maak het sf object

vakkenFileName <- "data\\RWS\\compartimenten_ws.csv"
vakkenCSV <- read_delim(vakkenFileName, delim = ";")
vakkenList <- vakkenCSV %>%
  mutate(
    Compartiment = (
      case_when(
        !Compartiment %in% c("0", "7a", "7b") ~ Compartiment,
        Compartiment == "0" ~ "9",
        Compartiment == "7a" ~ "7",
        Compartiment == "7b" ~ "8"
      )
    )
  ) %>%
  split("Compartiment")

## KIJK IN https://github.com/ropensci/wellknown VOOR FUNCTIES


  group_by(Compartiment) %>%
  summarize(geom = matrix(X, Y))

vakken <- list(vakkenList$geom)
names(vakken[[1]]) <- vakkenList$Compartiment

sf::st_multipolygon(vakken[[1]])

# read measurements

RwsPppFile <- "data\\RWS\\PrimaireProductie2020_2021.csv"

RWSdat <- read_delim(RwsPppFile, delim = ";") %>%
  mutate(date = as.Date(DateTimeStartAms)) %>%
  mutate(`production gC/m2` = productionMgC/1000)

RWSprod <- RWSdat %>%
  select(
    meetpuntnaam,
    datetime = DateTimeStartAms,
    XScanfish, YScanfish,
    `production gC/m2`,
    PBmax_EP,
    alfaB_EP,
    Eopt_EP,
    Chl
         )

write_lines(
  x = c(
    "# calculated from RWS data", 
    "# pelagic primary production in gC/m2/d from FRRF metingen",
    "# locations only those related to MWTL stations",
    "# year - year", 
    "# month - month", 
    "# monthlymean - monthly mean gross production in gC/m2/d", 
    "# date - constructed date"),
  file = "data/RWS/bewerkt/ppp_berekend_per_station_per_maand.csv"
)

  
RWSprodMaand <- RWSprod %>%
  filter(!is.na(meetpuntnaam)) %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(meetpuntnaam, year, month) %>%
  summarize(monthlyMean = mean(`production gC/m2`), na.rm = T) %>%
  mutate(date = ymd(paste(year, month, "15"))) %>%
  write_delim("data/RWS/bewerkt/ppp_berekend_per_station_per_maand.csv", 
              delim = ";", append = T, col_names = T)


RWSprodMaand %>%
  ggplot(aes(date, monthlyMean)) +
  geom_line(color = "darkolivegreen3", size = 1) +
  # geom_boxplot(fill = "transparent", aes(group = year), outlier.size =  0) +
  labs(subtitle = "calculated Production in gC/m2/d") +
  coord_cartesian(ylim = c(0,NA)) +
  ylab("production gC/m2/d") +
  facet_grid(meetpuntnaam ~ .) +
  theme_minimal()


# read reanalysis data

PrimaireProductie2020_2021_elkeDagJaarLineairGeinterpoleerd <- read_delim("data/RWS/PrimaireProductie2020_2021_elkeDagJaarLineairGeinterpoleerd.csv", 
delim = ";", escape_double = FALSE, trim_ws = TRUE)

write_lines(
  x = c(
    "# calculated from RWS interpolated data", 
    "# pelagic primary production in gC/m2/d from FRRF metingen",
    "# locations only those related to MWTL stations",
    "# year - year", 
    "# month - month", 
    "# monthlymean - monthly mean gross production in gC/m2/d", 
    "# date - constructed date"),
  file = "data/RWS/bewerkt/interpolated_ppp_berekend_per_station_per_maand.csv"
)


PrimaireProductie2020_2021_elkeDagJaarLineairGeinterpoleerdMaand <- PrimaireProductie2020_2021_elkeDagJaarLineairGeinterpoleerd %>%
  mutate(date = as_date(dtm)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(Compartiment, year, month) %>%
  summarize(monthlyMean = mean(SummedDailyProduction_gC_m2_dag), na.rm = T) %>%
  mutate(date = ymd(paste(year, month, "15"))) %>%
  write_delim("data/RWS/bewerkt/interpolated_ppp_berekend_per_station_per_maand.csv", 
              delim = ";", append = T, col_names = T)





