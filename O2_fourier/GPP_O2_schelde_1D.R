
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

modeldatadir = c("data\\Deltares\\modeluitvoer\\4tom0405/paper/")

list.files(modeldatadir)

stationlist = c(
  '31' = 'Schellebelle',
  '32' = 'Kruibeke',
  '33' = 'Liefkenshoek',
  '34' = 'Hansweert',
  '35' = 'Terneuzen'
)  

modeltimedata = tibble(
  time = 
    seq(ymd_hms('2014-03-03 00:00:00'), ymd_hms('2014-05-03 00:00:00'), by='10 min')
)

 stations = names(stationlist)

modeloutputlist <- lapply(
  stations,
  function(station){
  
    O2timeseriespath <- file.path(modeldatadir, paste0("O2avg_", station, "ds.dat"))
    O2data <- read_table(O2timeseriespath, col_names = "o2avg_ds")
    
    O2L2timeseriespath <- file.path(modeldatadir, paste0("O2_", station, "ds.dat"))
    O2L2data <- read_table(O2L2timeseriespath, col_names = "o2L2_ds")
    
    Depthtimeseriespath <- file.path(modeldatadir, paste0("Depth_", station, "ds.dat"))
    Depthdata <- read_table(Depthtimeseriespath, col_names = "Depth_ds")
    
    modeltimedata %>%
      bind_cols(O2data) %>%
      bind_cols(O2L2data) %>%
      bind_cols(Depthdata)
    
  }
)

names(modeloutputlist) = unname(stationlist)

list_ggpInput_df <- modeloutputlist %>%
  map(function(x) x %>% select(time, O2 = o2avg_ds)) 

# standard way to calculate daily average volumetric gpp
ggp_fourier <- list_ggpInput_df %>%
  map(
    function(df) WindowGPPFourier(
      x = as.data.frame(df),
      phi = 51.176932, 
      lambda = 4.325788,
      Width = 1
    )
  ) %>%
  bind_rows(.id = "station") %>%
  rename(datetime = time) %>%
  select(-t1, -t2)


# calculate volumetric gpp with fourier method with lowpass filter for tides
ggp_fourier_t <- list_ggpInput_df %>%
  map(
    function(df) GPPFourier_t(
      x = as.data.frame(df),
      Nfilt = 6*24,
      phi = 51.176932, 
      lambda = 4.325788,
      NLowPass = 6*24
    )
  ) %>%
  bind_rows(.id = "station") %>%
  group_by(station, date = as_date(times)) %>%
  summarize(GPPt = mean(GPPt, na.rm = T), .groups = "drop") %>%
  mutate(datetime = as_datetime(paste(date, "12:00:00")))
  

# check diff between gpp and gpp_t

ggplot() +
  geom_point(
    data = ggp_fourier,
    aes(datetime, GPP, color = "gpp")
    ) +
  geom_line(
    data = ggp_fourier_t,
    aes(datetime, GPPt, color = "gppt")
  ) +
  facet_wrap(
    "station", 
    scales = "free_y"
  ) +
  ggtitle("GPP and GPPt (see package description); for gppt a lowpass filter is used of 24 hours.")

modeloutput <- modeloutputlist %>%
      bind_rows(.id = "stationname") %>%
      rename(
        station = stationname,
        datetime = time
      )

modeloutput_day_avg <- modeloutput %>%
  group_by(station, date = as_date(datetime)) %>%
  summarize(
    o2avg_ds_day_avg = mean(o2avg_ds),
    o2L2_ds_day_avg = mean(o2L2_ds),
    Depth_ds_day_avg = mean(Depth_ds),
  ) %>%
  mutate(datetime = as_datetime(paste(date, "12:00:00"))) %>%
  select(-date)

# gpp from model is alrady dat averaged and is expressed as grams C per m2
GPP_from_model <- lapply(
  stations, function(station){
    GPPtimeseriespath <- file.path(modeldatadir, paste0("GPP_", station, "ds_day_avg.dat"))
    read_table(GPPtimeseriespath, col_names = c("time", "GPP_day_avg_ds_m2")) %>%
      mutate(date = as_datetime(as_date(time - 719529 ))) # uses matlab time stamp
  }
) %>% set_names(stationlist) %>%
  bind_rows(.id = "station") %>%
  rename(datetime = date) %>%
  select(-time) %>%
  left_join(modeloutput_day_avg) %>%
  mutate(GPP_dayavg_ds_m3 = GPP_day_avg_ds_m2/Depth_ds_day_avg)

#=== Grafiek voor publicatie ===================================================

# colors from colorbrew set2 for 5 discrete values (https://colorbrewer2.org/?type=qualitative&scheme=Set2&n=5)
# not used at the moment
plotcolors = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')

# Figuur met standaardmethode (ziet er niet zo heel goed uit)
ggplot() +
  geom_path(
    data = GPP_from_model, 
    aes(datetime, GPP_dayavg_ds_m3, color = "gpp_model")) +
  geom_point(
    data = ggp_fourier,
    aes(datetime, GPP/2.67, color = "gpp_fourier")
  ) +
facet_wrap("station", scales = "free", ncol = 1) +
  ylab("GPP in gC/m3/d") +
  scale_color_manual(
    values = c("gpp_fourier" = plotcolors[1],
               "gpp_model" = plotcolors[2],
               "difference" = plotcolors[4])
  ) +
  ggtitle("gpp fourier vs modelled gpp in gC/m2/d")

# figuur met lopend gemiddelde van 1 dag. Ziet er beter uit. 
ggplot() +
  geom_path(
    data = GPP_from_model, 
    aes(datetime, GPP_dayavg_ds_m3, color = "gpp_model"),
    linewidth = 1) +
  geom_point(
    data = ggp_fourier_t,
    aes(datetime, GPPt/2.67, color = "gpp_fourier_t"),
    method = "loess", span = 0.1
  ) +
  geom_segment(
    data = GPP_from_model %>%
      left_join(ggp_fourier_t) %>%
      mutate(diff = GPPt/2.67 - GPP_dayavg_ds_m3),
    aes(datetime, y = 0, yend = diff, color = "difference"),
    linewidth = 2
  ) +
  facet_wrap("station", scales = "free", ncol = 1) +
  xlab("time") +
  ylab("GPP in gC/m3/d") +
  scale_color_manual(
    values = c("gpp_fourier_t" = plotcolors[1],
               "gpp_model" = plotcolors[2],
               "difference" = plotcolors[4])
  ) +
  ggtitle("gpp fourier vs modelled gpp in gC/m3/d")

ggsave(filename = "O2_fourier/manuscript/gppt_1D.png", height = 7, width = 8)
