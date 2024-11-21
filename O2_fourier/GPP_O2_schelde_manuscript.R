
#  package at https://github.com/tomjscox/GPPFourier

if(!require(GPPFourier)){
  remotes::install_github("tomjscox/GPPFourier")
}
require(GPPFourier)
require(tidyverse)
require(lubridate)
require(data.table)
require(leaflet)

# check exacte periode van O2 veranderingen tijdens pieken in fourier
# 
# Hypothese: als periode meer lijkt op die van getij dan zou het kunnen 
# zijn dat getij meer invloed heeft dan productie. dwz, de productie wordt
# dan waarschijnlijk niet goed geschat (waarschijnlijk overschat).
# 
# 
# 
# kan het model gedraaid worden zonder productie?
# check relatie met temperatuur en saliniteit (thijs maakt uitvoer)
# transport
# aanbevelingen
# meting aan de kant lijkt zelfde resultaat te geven als in het midden
# mogelijk geven meerdere palen een beter signaal? Kies een strategische afstand
# 


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

stations = c(
  "area2",
  "kra",
  "o2paal",
  "oli"
)

selectedLayer = "1"
# calculate for all stations

gpplist <- lapply(
  stations,
  function(station){
    
    O2timeseriespath <- file.path(datadir, paste0("O2_3D_all_", station, ".dat"))
    O2fromModel <- timedata %>%
      bind_cols(read_table(O2timeseriespath, col_names = paste("layer", c(1:6))))
    
    gppData <- O2fromModel %>%
      select(time, O2 = paste0("layer ", selectedLayer)) %>%
      drop_na() %>%
      as.data.frame()
    
    gpp <- WindowGPPFourier(x = gppData,
                            # Nfilt = 1,
                            phi = 51.176932, 
                            lambda = 4.325788,
                            Width = 1#,
                            # filtWidth = 10
    )
  }
)

names(gpplist) <- stations

gsplist <- lapply(
  stations,
  function(station){
    Saltimeseriespath <- file.path(datadir, paste0("Sal_3D_all_", station, ".dat"))
    SalfromModel <- timedata %>%
      bind_cols(read_table(Saltimeseriespath, col_names = paste("layer", c(1:6))))
    gspData <- SalfromModel %>%
      select(time, Sal = paste0("layer ", selectedLayer)) %>%
      drop_na() %>%
      as.data.frame()
    
    gsp <- WindowGPPFourier(x = gspData,
                            # Nfilt = 1,
                            phi = 51.176932, 
                            lambda = 4.325788,
                            Width = 1#,
                            # filtWidth = 10
    )
  }
)
    
    



O2fromMOdel_list <- lapply(
  stations,
  function(station){
    O2timeseriespath <- file.path(datadir, paste0("O2_3D_all_", station, ".dat"))
    O2fromModel <- timedata %>%
      bind_cols(read_table(O2timeseriespath, col_names = paste("layer", c(1:6))))
    O2fromModel %>%
      select(time, O2 = paste0("layer ", selectedLayer)) %>%
      drop_na() %>%
      as.data.frame()
  }
)

names(O2fromMOdel_list) <- stations

O2fromModel <- O2fromMOdel_list %>%
  bind_rows(.id = "station")

GPPfromMOdel_list <- lapply(
  stations,
  function(station){
    GPPfromModel <- timedata %>%
      bind_cols(read_table(file.path(datadir, paste0("GPP_3D_", station, ".dat")), col_names = "gpp"))
    read_table(
      file.path(
        datadir, 
        paste0("GPP_3D_day_avg_", station, ".dat")
      ), 
      col_names = c("time", "gpp_day")
    ) %>% 
      mutate(time = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
    
  }
)

names(GPPfromMOdel_list) <- stations
GPPfromModel_day_avg <- GPPfromMOdel_list %>%
  bind_rows(.id = "station")


SalfromMOdel_list <- lapply(
  stations,
  function(station){
    Saltimeseriespath <- file.path(datadir, paste0("Sal_3D_all_", station, ".dat"))
    SalfromModel <- timedata %>%
      bind_cols(read_table(Saltimeseriespath, col_names = paste("layer", c(1:6))))
    SalfromModel %>%
      select(time, Sal = paste0("layer ", selectedLayer)) %>%
      drop_na() %>%
      as.data.frame()
  }
)


names(SalfromMOdel_list) <- stations

SalfromModel <- SalfromMOdel_list %>%
  bind_rows(.id = "station")


# colors from colorbrew set2 for 5 discrete values (https://colorbrewer2.org/?type=qualitative&scheme=Set2&n=5)
colors = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')

gpp <- gpplist %>% 
  bind_rows(.id = "station")


gpp %>%
  ggplot(aes(time, GPP)) +
  geom_point(aes(color = "GPPcalculated"), alpha = 0.7, size = 0.7) + 
  geom_line(data = sample_n(O2fromModel, 10000), aes(time, O2-10, color = "O2fromModel"), linewidth = 0.75) +
  geom_line(data = sample_n(SalfromModel, 10000), aes(time, (Sal/5)+2, color = "SalfromModel"), linewidth = 0.75) +
  geom_smooth(aes(color = "GPPberekendLoess"), method = "loess", span = 0.1, alpha = 0.1) +
  geom_line(
    data = GPPfromModel_day_avg,
    aes(time, 1e3*gpp_day, color = "GPPfromModel"),
    linewidth = 1
    ) +
  ggtitle(paste("layer", selectedLayer)) +
  ylab("GPP in mg Oxygen per liter per day") +
  facet_wrap(vars(station)) +
  # coord_cartesian(
  #   xlim = c(as_datetime("2014-05-01"), as_datetime("2014-05-10")),
  #   # xlim = c(as_datetime("2014-08-01"), as_datetime("2014-08-10")),
  #   ylim = c(NA,2)
  # ) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%h") +
  theme_light()

ggsave(filename = "O2_fourier/gpp_sal_layer1.png", height = 5, width = 7)

