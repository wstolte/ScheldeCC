
require(tidyverse)

#! duurt lang

opp_obs <- read_csv("https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Scheldemonitor/2024/Data_FysChem_opp.csv")


opp_obs %>% distinct(parametername)

opp_obs %>% 
  filter(parametername == "Chlorofyl a in ug/l in oppervlaktewater") %>%
  select(
    latitude,
    longitude,
    stationname,
    datetime,
    depth,
    parametername,
    parameterunit,
    value
  ) %>%
  write_delim(file = "data/Scheldemonitor/chlorophyl.csv")


  ggplot(aes(datetime, value)) +
  geom_point(aes(color = stationname))
