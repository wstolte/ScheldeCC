
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
      # Nfilt = 1,
      phi = 51.176932, 
      lambda = 4.325788,
      Width = 1#,
      # filtWidth = 10
    )
  ) %>%
  bind_rows(.id = "station")


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
  rename(
    datetime = times,
  )

# check diff between gpp and gpp_t

ggplot() +
  geom_point(
    data = ggp_fourier,
    aes(time, GPP, color = "gpp")
    ) +
  geom_line(
    data = ggp_fourier_t,
    aes(times, GPPt, color = "gppt")
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

GPP_from_model <- lapply(
  stations, function(station){
    GPPtimeseriespath <- file.path(modeldatadir, paste0("GPP_", station, "ds_day_avg.dat"))
    read_table(GPPtimeseriespath, col_names = c("time", "GPP_day_avg_ds_m2")) %>%
      mutate(date = as_datetime(as_date(time - 719529 ))) # uses matlab time stamp
  }
) %>% set_names(stationlist) %>%
  bind_rows(.id = "station") %>%
  rename(
    datetime = date
  ) %>%
  select(
    -time
  )

#=== Grafiek voor publicatie ===================================================

ggp_fourier_t %>% str() # 10-minutes 

GPP_from_model %>% str() # day average

GPP_all <- ggp_fourier_t %>%
  mutate(fourier_GPPt_C = GPPt/2.67) %>%
  # group_by(day(datetime)) %>% 
  # mutate(fourier_GPPt_day_avg_C = mean(fourier_GPPt_d_C)) %>% ungroup() %>%
  full_join(
    GPP_from_model
  ) %>%
  full_join(
    modeloutput
  ) %>%
  mutate(
    GPP_day_avg_ds_m3 = GPP_day_avg_ds_m2 / Depth_ds
  ) %>%
  # mutate(
  #   diff_GPP_day_avg = model_GPP_day_avg_ds - fourier_GPPt_d_C
  # ) %>%
  select(
    station,
    datetime,
    Depth_ds,
    fourier_GPPt_C,
    # fourier_GPPt_day_avg_C,
    GPP_day_avg_ds_m3
  ) #%>%
  # pivot_longer(
  #   c(
  #     fourier_GPPt_d_C, 
  #     # fourier_GPPt_day_avg_C,
  #     model_GPP_day_avg_ds, 
  #     diff_GPP_day_avg
  #   ), 
  #   names_to = "method", 
  #   values_to = "GPP in gC/m2/d"
  # )

# colors from colorbrew set2 for 5 discrete values (https://colorbrewer2.org/?type=qualitative&scheme=Set2&n=5)
# not used at the moment
colors = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')

GPP_all %>%
  # filter(!is.na(`GPP in gC/m2/d` & !is.nan(`GPP in gC/m2/d`))) %>%
  # distinct() %>%
  ggplot(
    aes(x = datetime)) +
  geom_line(aes(y = fourier_GPPt_C, color = "gpp_fourier"))+
  geom_point(aes(y = GPP_day_avg_ds_m3, color = "gpp_model")) +
  facet_wrap("station", scales = "free", ncol = 1) +
  ylab("GPP in gC/m3/d")
  
GPP_all %>%
  group_by(station, day = day(datetime - 60*60*12)) %>% # average over -12 h to +12h
  summarize(
    Depth_day_avg = mean(Depth_ds, na.rm = T),
    fourier_GPPt_C_day_avg = mean(fourier_GPPt_C, na.rm = T),
    GPP_day_avg_ds_m3 = mean(GPP_day_avg_ds_m3, na.rm = T)
    ) %>% ungroup() %>%
  mutate(difference = fourier_GPPt_C_day_avg - GPP_day_avg_ds_m3) %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = fourier_GPPt_C_day_avg, color = "gpp_fourier_day_avg"))+
  geom_point(aes(y = GPP_day_avg_ds_m3, color = "gpp_model")) +
  geom_segment(aes(y = 0, yend = difference, color = "difference"), size = 3) +
  facet_wrap("station", scales = "free", ncol = 1) +
  ylab("GPP in gC/m3/d")






