
require(tidyverse)
source("refresh_data.R")

smpath <- "data/Scheldemonitor"

# Primaire productie

WSstations <- c("WS1", "WS4", "WS6", "WS10", "WS11")

url3 <- "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Aabiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%283620%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Clongitude%2Clatitude%2Cdatetime%2Cdepth%2Cparametername%2Cvaluesign%2Cvalue%2Cdataprovider%2Cdatasettitle%2Clod%2Cloq%2Ccategory%2Cseason%2Cclassunit%2Cclass%2Caphiaid%2Cscientificname%2Cdateprecision%2Cdatafichetitle%2Cdataficheid%2Cstandardparameterid%2Cparameterunit&outputFormat=csv"

ppsm <- read_csv(url3, col_types = cols()) %>%
  filter(stationname %in% WSstations) %>% # alleen Westerscheldestations
  mutate(stationname = factor(stationname, levels = WSstations)) 
# selecteer kolommen voor verdere analyse

write_delim(ppsm, file.path(smpath, "primaireproductieNL.csv"), delim = ";")

# monthly means

write_lines(
  x = c(
    "# calculated from SM data", 
    "# pelagic primary production in gC/m2/d from 14C metingen",
    "# locations only those related to SM stations",
    "# year - year", 
    "# month - month", 
    "# monthlymean - monthly mean gross production in gC/m2/d", 
    "# date - constructed date"),
  file = "data/Scheldemonitor/bewerkt/ppp_berekend_per_station_per_maand.csv"
)


ppsmMaand <- ppsm %>%
  filter(!is.na(stationname)) %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(stationname, year, month) %>%
  summarize(monthlyMean = mean(value), na.rm = T) %>%
  mutate(date = ymd(paste(year, month, "15"))) %>%
  write_delim("data/Scheldemonitor/bewerkt/ppp_berekend_per_station_per_maand.csv", 
              delim = ";", append = T, col_names = T)






# korrelgroottes en koolstof in Zwevend stof
parIDs <- IDs %>%
  filter(grepl("in zwevend", Parameternaam, ignore.case = T)) %>%
  filter(
    grepl("korrel", Parameternaam, ignore.case = T) |
    grepl("koolstof", Parameternaam, ignore.case = T)
  ) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

refresh_fysischchemischzwevendstof(1970, 2022, parameterIDs = parIDs)

# oppwater uit Scheldemonitor

parIDs <- IDs %>%
  filter((grepl("in water", Parameternaam, ignore.case = T) | grepl("in oppervlaktewater", Parameternaam, ignore.case = T) | grepl("licht", Parameternaam, ignore.case = T))
         & !grepl("in waterbodem", Parameternaam, ignore.case = T)) %>%
  filter(
    grepl("temperatuur", Parameternaam, ignore.case = T) |
      grepl("saliniteit", Parameternaam, ignore.case = T) |
      grepl("nitraat", Parameternaam, ignore.case = T) |
      grepl("ammonium", Parameternaam, ignore.case = T) |
      grepl("fosfaat", Parameternaam, ignore.case = T) |
      grepl("chlorofyl", Parameternaam, ignore.case = T) |
      grepl("doorzicht", Parameternaam, ignore.case = T) |
      grepl("licht", Parameternaam, ignore.case = T) |
      grepl("zuurstof", Parameternaam, ignore.case = T) |
      grepl("koolstof", Parameternaam, ignore.case = T) |
      grepl("silicaat", Parameternaam, ignore.case = T)
  ) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

refresh_fysischchemischoppwater(1970, 2022, parameterIDs = parIDs[[1]])

#check
# Saliniteit <- c(998)
# Temperatuur <- c(1045, 1046)
# Zuurstof <- c(1214,1213)
# Chlorofyl_a <- c(238,1800)
# BZV_BOD_CZV <- c(125,178)
# Lichtklimaat <- c(461,495, 5141, 5134, 1321)
# Zwevende_stof <- c(1223)
# Nutrienten <- c(26,2829,1996,528,2370,529,530,531,1843,1019,828,834,866,1010,3217,833,1021,1022,1972)
# Organisch_koolstof <- c(663,674)
# Metalen <- c(133,1737,259,260,268,269,1178,2014,1181,2018,1201,1202,686,687)
# 
# parIDs <- c(Saliniteit,Temperatuur,Zuurstof,Chlorofyl_a, Lichtklimaat,Zwevende_stof,Nutrienten,Organisch_koolstof)

filepath = "fysChemOppWater.csv"
df.oppwater <- read_csv(file.path(datapath, paste(today(), filepath)))

unique(df.oppwater$parametername)


# parameters in zwevendstof uit Scheldemonitor 

filepath = "fysChemZwevendStof.csv"

df.zwevendstof <- read_csv(file.path(datapath, paste(today(), filepath)))

df.zwevendstof %>%
  filter(grepl("korrel", parametername, ignore.case = T)) %>%
  group_by(parametername) %>% summarize(n = n())

df.zwevendstof %>%
  filter(grepl("koolstof", parametername, ignore.case = T)) %>%
  group_by(parametername) %>% summarize(n = n())

df.zwevendstof %>%
  # filter(grepl("", parametername, ignore.case = T)) %>%
  group_by(parametername) %>% summarize(n = n()) %>% View


