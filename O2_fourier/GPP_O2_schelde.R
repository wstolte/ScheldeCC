
#  package at https://github.com/tomjscox/GPPFourier

if(!require(GPPFourier)){
  remotes::install_github("tomjscox/GPPFourier")
}
require(GPPFourier)
require(tidyverse)
require(lubridate)
require(data.table)
require(leaflet)

# read data

datadirs = c("data\\Deltares\\modeluitvoer\\4tom_3D_141218_kruibeke",
            "data\\Deltares\\modeluitvoer\\4tom_3D_141218_benedenzeeschelde",
            "data\\Deltares\\modeluitvoer\\4tom_36092019")

# de files in kruibeke en benedenschelde zijn korter dan de gecorrigeerde tijdserie! \
# alleen files in 3609

datadir = datadirs[3]

list.files(datadir)
# timedefpath <- file.path(datadir, "Time_3D.dat")
# timedata <- read_table(timedefpath, col_names = "time") %>% 
#   mutate(time = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))

timedef_correctedpath <- file.path(datadir, "Time_corrected.dat")
timedata <- read_table(timedef_correctedpath, col_names = c("date", "time")) %>% 
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  mutate(time = as_datetime(paste(date, time))) %>%
  select(time)

# "O2_3D_all_area2.dat"
# "O2_3D_all_kra.dat"            
# "O2_3D_all_o2paal.dat"
# "O2_3D_all_oli.dat"

# stations 
# area2 (beneden zeeschelde), 
# kra (raai), 
# o2paal (locatie paal), 
# oli (in geul ter hoogte van paal?)

station = "kra"

O2timeseriespath <- file.path(datadir, paste0("O2_3D_all_", station, ".dat"))
O2fromModel <- timedata %>%
  bind_cols(read_table(O2timeseriespath, col_names = paste("layer", c(1:6))))




GPPfromModel <- timedata %>%
  bind_cols(read_table(file.path(datadir, paste0("GPP_3D_", station, ".dat")), col_names = "gpp"))

GPPfromModel_day_avg <- read_table(
  file.path(
    datadir, paste0("GPP_3D_day_avg_", station, ".dat")), 
  col_names = c("time", "gpp_day")) %>% 
  mutate(time = as.POSIXct(
    (time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))

gppData <- O2fromModel %>%
  select(time, O2 = `layer 3`) %>%
  drop_na() %>%
  as.data.frame()

# ggplot(O2fromModel, aes(time, `layer 1`)) +
#   geom_line() +
#   geom_line(data = GPPfromModel, aes(time, gpp*1e3), color = "red")

# niet nodig, alleen als er gaten in de tijdserie zitten
# gppData <- GPPFourier::gapfill(gppData)

gpp <- WindowGPPFourier(x = gppData,
                         phi = 51.176932, 
                         lambda = 4.325788,
                         Width = 1)

myPal = scales::hue_pal()(5)

myColors = c(
  "GPPfromModel" = myPal[1],
  "GPPberekend" = myPal[2],
  "GPPberekendLoess" = myPal[3]
)

ggplot(gpp, aes(time, GPP)) +
  geom_point(aes(color = "GPPberekend")) + 
  geom_smooth(aes(color = "GPPberekendLoess"), method = "loess", span = 0.1, alpha = 0.1) +
  # geom_line() +
  geom_line(
    data = GPPfromModel_day_avg, 
    aes(time, 1e3*gpp_day, color = "GPPfromModel")
    ) +
  ggtitle(station) +
  ylab("GPP in mg Oxygen per liter per day") +
  coord_cartesian(ylim = c(NA,NA)) +
  scale_color_manual(values = myColors) +
  theme_light()

