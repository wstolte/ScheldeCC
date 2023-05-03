
require(tidyverse)
require(stringr)
require(lubridate)
require(leaflet)
require(sf)

dirnames <- list.files("data/NIOZ/PPDick/Archive", pattern = "Meetvis", recursive = T, full.names = T, include.dirs = T)

filenames <- unlist(
  map(
    dirnames, function(x) list.files(pattern = "xls", recursive = T, full.names = T)
    )
  )

filenames[344]
readxl::excel_sheets(filenames[344])
readxl::read_excel(filenames[1]) %>% str()

selectedColumns = c('X', 'Y', 'Longitude', 'Latitude', 'DATUM', 'TIJD', 'SENSHTE', 'T', 'pH', 'O2', '%O2', 'SALNTT', 'CHFLYSI', 'TURBID', 'LIOW', 'Libo')

data <- lapply(
  filenames, 
  function(x) readxl::read_excel(x) %>%
    select(any_of(selectedColumns)) %>%
    mutate(across(matches(selectedColumns), as.numeric))
    # select(-any_of(c('Echms'))) %>%
)

df <- bind_rows(data) %>%
  mutate(datetime = as_datetime(paste(DATUM, stringr::str_pad(as.character(TIJD), 6, "left", "0"))))

time_space <- df %>% 
  select(X, Y, datetime) %>%
  filter(Y > 1000) %>%
  filter(!is.na(datetime))
  
write_csv(time_space, "data/NIOZ/PPDick/clean/times-and-locations.csv")

# test coordinates

time_space %>% 
  # sample_n(1000) %>%
  st_as_sf(coords = c("X", "Y"), crs = 28992) %>%
  st_transform(4236) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(label = ~ paste(geometry))

# test join with data

ppdata <- read_csv("data\\NIOZ\\PPDick\\clean\\Voor_John_FRRFtidy.csv", skip = 2, skip_empty_rows = T) %>%
  mutate(datetime = as_datetime(date, format = "%d/%m/%Y %H:%M"))

find_closest <- function(d1, d2){
  # find closest datetime to d1 in vector d2
  # 
  (map_int(d2, duration(d1, units = "seconds")))
  
}

min(difftime(ppdata$datetime[100], time_space$datetime), na.rm = T)

  map(time_space$datetime, function(x) duration(ppdata$datetime[1], x))


