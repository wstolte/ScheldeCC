
require(tidyverse)
source("prepareDataSchelde/refresh_data.R")

smpath <- "data/Scheldemonitor"
savepath <- smpath

#== Primaire productie ================================

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

#========= oppwater uit Scheldemonitor ================================


filepath = "2024-02-21_oppWater.csv"

refreshData = F

if(refreshData){
  lapply(2015:2022, 
         function(x){
           refresh_fysischchemischoppwater(x, x, filepath = paste(x, filepath, sep = "_"))
         })
}

## Only if data need to be refreshed. 
## parameter codes are hardcoded in function

df.oppwater <- lapply(2015:2022,
                      function(x) {
                        read_csv(
                          file = file.path(savepath, paste(x, filepath, sep = "_")),
                          guess_max = 200000
                          ) %>%
                          select(
                            latitude,
                            longitude,
                            depth,
                            datetime,
                            value,
                            parametername,
                            parameterunit,
                            stationname,
                            external_name,
                            externalid,
                            valuesign
                          )
                        }
) %>% 
  bind_rows()

write_csv(x = df.oppwater, file = file.path(savepath, "bewerkt", "2015-2022_2024-02-21_oppWater.csv"))

df.oppwater %>% group_by(external_name, parametername) %>% summarize(n = n()) %>% 
  write_csv(file = file.path(savepath, "bewerkt", "parameternames_oppWater.csv"))

additionalStation = tibble(
  stationname = "Vlakte van de Raan", longitude = 3.30904, latitude = 51.46421
)

stationLocations <- df.oppwater %>% 
  bind_rows(additionalStation) %>%
  filter(stationname != "Westerschelde") %>%
  distinct(stationname, longitude, latitude)

(stationLocations$longitude)
scales::scientific(stationLocations$longitude, digits = 10)
scales::scientific(stationLocations$latitude, digits = 10)

stationLocations$longitude <- format(stationLocations$longitude, digits = 10)
stationLocations$latitude <- format(stationLocations$latitude, digits = 10)

write.csv(x = stationLocations, file = file.path(savepath, "bewerkt", "stationLocations_oppWater.csv"), row.names = F)

otherLocations <- df.oppwater %>% 
  filter(stationname == "Westerschelde") %>%
  distinct(stationname, latitude, longitude, datetime)

write_csv(x = otherLocations, file = file.path(savepath, "bewerkt", "otherLocations_oppWater.csv"))

stationLocations %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(label = ~ stationname, radius = 2)

otherLocations %>%
  sample_n(1000) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers()


