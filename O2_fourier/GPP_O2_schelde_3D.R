
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

list.files(datadir, pattern = "Depth")

timedef_correctedpath <- file.path(datadir, "Time_corrected.dat")
timedata <- read_table(timedef_correctedpath, col_names = c("date", "time")) %>% 
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  mutate(time = as_datetime(paste(date, time))) %>%
  select(time)

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

# fetch depth information (total depth)
DepthfromModel <- lapply(
  stations,
  function(station){
    Depthtimeseriespath <- file.path(datadir, paste0("Depth_3D_", station, ".dat"))
    DepthfromModel <- timedata %>%
      bind_cols(read_table(Depthtimeseriespath, col_names = "Depth"))
    DepthfromModel %>%
      select(
        datetime = time, 
        Depth) %>%
      drop_na() %>%
      as.data.frame()
  }
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station")

# make dataframe with daily average depth
DepthfromModel_day <- DepthfromModel %>%
  group_by(station, date = as.Date(datetime)) %>%
  summarize(day_avg_depth = mean(Depth), .groups = 'drop') %>%
  mutate(datetime = as_datetime(paste(date, "12:00:00"))) %>%
  select(station, datetime, day_avg_depth)


# calculate O2 production with time window
selectedLayer = "2"
# calculate for all stations

gpp <- lapply(
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
                            phi = 51.176932, 
                            lambda = 4.325788,
                            Width = 1#,
    ) %>%
      select(-t1, -t2)
  }
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station") %>%
  mutate(GPP_C = GPP / 2.67) %>%
  rename(datetime = time)

# calculate "salinity production"

gsp <- lapply(
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
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station") %>%
  rename(datetime = time)

# calculate O2 production with demodulated series

gppt <- lapply(
  stations,
  function(station){
    
    O2timeseriespath <- file.path(datadir, paste0("O2_3D_all_", station, ".dat"))
    O2fromModel <- timedata %>%
      bind_cols(read_table(O2timeseriespath, col_names = paste("layer", c(1:6))))
    
    gppData <- O2fromModel %>%
      select(time, O2 = paste0("layer ", selectedLayer)) %>%
      drop_na() %>%
      as.data.frame()
    
    gpp <- GPPFourier_t(x = gppData,
                        Nfilt = 15*24*6, # moving average filter
                        NLowPass = 24*6, # moving average filter width for low pass filtering demodulated O2 series
                        phi = 51.176932,
                        lambda = 4.325788)
  }
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station") %>%
  mutate(GPPt_C = GPPt / 2.67) %>%
  rename(datetime = times)
  


# extract O2 concentrations time series from model
O2fromMOdel <- lapply(
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
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station") %>%
  rename(datetime = time)


# extract modelled GPP from model output
# 

GPPdiatFromMOdel <- lapply(
  stations,
  function(station){
    read_table(
      file.path(
        datadir, 
        paste0("GPP_3D_day_avgdiat_", station, ".dat")
      ), 
      col_names = c("time", "gpp_day_m2")
    ) %>% 
      mutate(datetime = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
  }
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station") %>%
  left_join(DepthfromModel_day) %>%
  mutate(
    gpp_day_m3 = gpp_day_m2/day_avg_depth
  )

GPPgreenFromMOdel <- lapply(
  stations,
  function(station){
    read_table(
      file.path(
        datadir, 
        paste0("GPP_3D_day_avg_", station, ".dat")
      ), 
      col_names = c("time", "gpp_day_m2")
    ) %>% 
      mutate(datetime = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
  }
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station") %>%
  left_join(DepthfromModel_day) %>%
  mutate(
    gpp_day_m3 = gpp_day_m2/day_avg_depth
  )


# extract salinity concentrations from modeloutput
SalfromMOdel <- lapply(
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
) %>%
  set_names(stations) %>%
  bind_rows(.id = "station")

gppt_day_avg <- gppt %>%
  group_by(station, date = as_date(datetime)) %>%
  summarize(
    GPPt_day_avg = mean(GPPt, na.rm = T),
    GPPt_C_day_avg = mean(GPPt_C, na.rm = T),
    .groups = "drop"
  ) %>%
  mutate(
    datetime = as_datetime(paste(date, "12:00:00"))
  )

gppTot_fromModel <- GPPdiatFromMOdel %>%
  select(
    station,
    datetime,
    gpp_day_m3_diat = gpp_day_m3) %>%
  bind_rows(
    GPPgreenFromMOdel %>%
      select(
        station,
        datetime,
        gpp_day_m3_green = gpp_day_m3)
  ) %>%
  mutate(gpp_day_m3_tot = gpp_day_m3_diat + ifelse(is.na(gpp_day_m3_green), 0, gpp_day_m3_green))
  
  


# colors from colorbrew set2 for 5 discrete values (https://colorbrewer2.org/?type=qualitative&scheme=Set2&n=5)
colors = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')

ggplot() +
  geom_path(
    data = gppTot_fromModel,
    aes(datetime, gpp_day_m3_tot, color = "gpp_model"),
    linewidth = 1
  ) +
  # geom_path(
  #   data = GPPdiatFromMOdel,
  #   aes(datetime, gpp_day_m3, color = "gpp_model"),
  #   linewidth = 1) +
  geom_point(
    data = gppt_day_avg,
    aes(datetime, GPPt_C_day_avg, color = "gpp_fourier_t")
  ) +
  # geom_segment(
  #   data = GPPfromMOdel %>%
  #     left_join(gppt_day_avg) %>%
  #     mutate(diff = GPPt_C_day_avg - gpp_day_m3),
  #   aes(datetime, y = 0, yend = diff, color = "difference"),
  #   linewidth = 2,
  # alpha = 0.3
  # ) +
  facet_wrap("station", ncol = 1) +
  # coord_cartesian(ylim = c(0,0.01))
  xlab("time") +
  ylab("GPP in gC/m3/d") +
  scale_color_manual(
    values = c("gpp_fourier_t" = plotcolors[1],
               "gpp_model" = plotcolors[2],
               "difference" = plotcolors[4])
  ) +
  ggtitle("gpp fourier vs modelled gpp in gC/m3/d")


ggsave(filename = "O2_fourier/manuscript/gppt_3D_layer2.png", height = 7, width = 8)

