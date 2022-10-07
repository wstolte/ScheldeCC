

# Data repos https://repos.deltares.nl/repos/rwsprojectarchief/data/Schelde

# import van lokale checkout

require(tidyverse)
require(lubridate)
require(sf)

# convert E50 (dms) to decimal coordinates function
dms2dec <- function(dms) {
  dms <- str_pad(as.character(dms), 8, "left", "0")
  deg <- substr(dms,1,2)
  min <- substr(dms,3,4)
  sec <- substr(dms,5,8)
  
  dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 360000)
  return(dec)
} # end dms2dec function


rawdatadir <-      "c:/repos_checkouts/rwsprojectarchief/data/Schelde/raw/"
standarddatadir <- "c:/repos_checkouts/rwsprojectarchief/data/Schelde/standard/"

#==== convert file M200506980.csv =============================

sed1data <- read_delim(file.path(rawdatadir, "sedimentDataRWS", "M200506980.csv"), delim = ";", na = "-999999999")
sed1data <- rename_with(sed1data, tolower)

# bemonstering
sed1data %>% 
  distinct(bemoms, anaoms, vatoms)
# meenemen
sed1data %>% 
  distinct(typ, sys)
# lijkt niet zo belangrijk
sed1data %>% 
  distinct(`plt:refvlak`, `plt:bmh`)
# Belangrijk. Lijkt in cm te zijn.
sed1data %>% 
  distinct(`plt:refvlak`, paroms)
# niet duidelijk... 
sed1data %>% distinct(`loc:coordsrt`)


sed1data2 <- sed1data %>%
  mutate(
    waarde = as.numeric(str_replace(waarde, "#\\+", "")),
    tijd = str_replace(tijd, "'", "")
    ) %>%
  mutate(datumtijd = ymd_hm(paste(datum, tijd))) %>%
  select(
    locatie.code = loc,
    datumtijd,
    compartiment.omschrijving = cpmoms,
    parameter.code = par,
    parameter.omschrijving = paroms,
    eenheid.code = ehd,
    hoedanigheid.omschrijving = hdhoms,
    hoedanigheid.omschrijving = hdhoms,
    analysemethode.omschrijving = anaoms,
    bemonsteringsmethode.omschrijving = bemoms,
    veldapparaat.omschrijving = vatoms,
    referentievlak.code = `plt:refvlak`,
    bemonsteringshoogte = `plt:bmh`,
    `loc:coordsrt`,
    `loc:x`,
    `loc:y`,
    kwaliteitswaarde.code = kwc,
    waarde) %>%
  mutate(monsterprogramma = "onbekend")
  

sed1data2.rd <- sed1data2 %>% filter(`loc:coordsrt` == "RD") %>% 
  mutate(x_RD = `loc:x`/100, y_RD = `loc:y`/100) %>%
  st_as_sf(coords = c("x_RD", "y_RD"), crs = 28992) %>%
  st_transform(4326)

sed1data2.E50 <- sed1data2 %>% filter(`loc:coordsrt` == "E50") %>%
  mutate(
    x = dms2dec(`loc:x`),
    y = dms2dec(`loc:y`),
    coordinatensoort = "4326"
  ) %>%
  drop_na(x, y) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

sed1data2.sf <- sed1data2.E50 %>% bind_rows(sed1data2.rd)

#==== convert file sedimentRWS_20210510 =============================

sed2data <- read_delim(file.path(rawdatadir, "sedimentRWS_20210510", "Sedimen.csv"), delim = ";", na = c("-999999999", "999999999999"), locale = locale(decimal_mark = ",")) %>%
  mutate(waarde = as.numeric(str_replace(waarde, ",", "."))) 
# sed2data[sed2data %>% duplicated %>% which(),] %>% View
#  Weird.. filter op bijv. parcod != "parcod"
sed2data <- sed2data %>%
  filter(parcod != "parcod")


sed2data %>%
  group_by(paroms, hdhoms) %>% summarize(n = n())
sed2data %>% 
  distinct(bemoms, anaoms, vatoms)
# meenemen, sommige rare waarden "anaoms"
sed2data %>% 
  distinct()
# Belangrijk. Lijkt in cm te zijn.
sed2data %>% 
  distinct(refvlk, paroms)
# niet duidelijk... 


sed2data2.sf <- sed2data %>%
  mutate(
    datumtijd = as_datetime(paste(rks_begdat, rks_begtyd), format = "%d %m %Y %H:%M"),
    bemhgt = as.numeric(bemhgt),
    kwlcod = as.numeric(kwlcod)
    ) %>%
  select(
    locatie.code = loccod,
    datumtijd,
    compartiment.omschrijving = cpmoms,
    parameter.code = parcod,
    parameter.omschrijving = paroms,
    eenheid.code = ehdcod,
    hoedanigheid.omschrijving = hdhoms,
    hoedanigheid.omschrijving = hdhoms,
    analysemethode.omschrijving = anaoms,
    bemonsteringsmethode.omschrijving = bemoms,
    veldapparaat.omschrijving = vatoms,
    referentievlak.code = refvlk,
    bemonsteringshoogte = bemhgt,
    crdtyp,
    loc_xcrdgs,
    loc_ycrdgs,
    kwaliteitswaarde.code = kwlcod,
    waarde) %>%
  mutate(monsterprogramma = "chlorofyl") %>%
  mutate(loc_xcrdgs = as.numeric(loc_xcrdgs)/100, loc_ycrdgs = as.numeric(loc_ycrdgs)/100) %>%
  st_as_sf(coords = c("loc_xcrdgs", "loc_ycrdgs"), crs = 28992) %>%
  st_transform(4326)

#==== convert Chlorof.csv  ==============================


sed3data <- read_delim(file.path(rawdatadir, "sedimentRWS_20210510", "Chlorof.csv"), delim = ";", na = c("-999999999", "999999999999"), locale = locale(decimal_mark = ",")) %>%
  mutate(waarde = as.numeric(str_replace(waarde, ",", "."))) 

sed3data %>%
  group_by(paroms, hdhoms) %>% summarize(n = n())
sed3data %>% 
  distinct(bemoms, anaoms, vatoms)
# meenemen, sommige rare waarden "anaoms"
sed3data %>% 
  distinct(bemhgt)
# Belangrijk. Lijkt in cm te zijn.
sed3data %>% 
  distinct(refvlk, paroms)


sed3data2.sf <- sed3data %>%
  mutate(
    datumtijd = as_datetime(paste(rks_begdat, rks_begtyd), format = "%d %m %Y %H:%M"),
    bemhgt = as.numeric(bemhgt),
    kwlcod = as.numeric(kwlcod)
  ) %>%
  select(
    locatie.code = loccod,
    datumtijd,
    compartiment.omschrijving = cpmoms,
    parameter.code = parcod,
    parameter.omschrijving = paroms,
    eenheid.code = ehdcod,
    hoedanigheid.omschrijving = hdhoms,
    hoedanigheid.omschrijving = hdhoms,
    analysemethode.omschrijving = anaoms,
    bemonsteringsmethode.omschrijving = bemoms,
    veldapparaat.omschrijving = vatoms,
    referentievlak.code = refvlk,
    bemonsteringshoogte = bemhgt,
    crdtyp,
    loc_xcrdgs,
    loc_ycrdgs,
    kwaliteitswaarde.code = kwlcod,
    waarde) %>%
  mutate(monsterprogramma = "chlorofyl") %>%
  mutate(loc_xcrdgs = as.numeric(loc_xcrdgs)/100, loc_ycrdgs = as.numeric(loc_ycrdgs)/100) %>%
  st_as_sf(coords = c("loc_xcrdgs", "loc_ycrdgs"), crs = 28992) %>%
  st_transform(4326)



# require(leaflet)
# leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data = sed1data2.sf, color = "green", label = ~geometry) %>%
#   addCircleMarkers(data = sed2data2.sf %>% distinct(geometry, datumtijd), label = ~geometry)


#=== combine data in one table ===================

sedDataWS <- sed1data2.sf %>% bind_rows(sed2data2.sf) %>% bind_rows(sed3data2.sf) %>%
  mutate(x_wgs = st_coordinates(.)[,1],
         y_wgs = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  filter(y_wgs < 52)

sedDataWS %>%
  group_by(parameter.omschrijving) %>%
  summarize(n = n()) %>%
  arrange(-n)

sedDataWS %>%
  filter(parameter.omschrijving %in% c("chlorofyl-a", "Feofytine", "Humus", "Percentage organische stof", "Percentage calciumcarbonaat")) %>%
ggplot(aes(x_wgs, y_wgs)) +
  geom_jitter(aes(color = parameter.omschrijving))

sedDataWS %>%
  filter(!parameter.omschrijving %in% c("chlorofyl-a", "Humus", "Percentage organische stof", "Percentage calciumcarbonaat")) %>%
  ggplot(aes(x_wgs, y_wgs)) +
  geom_jitter(aes(color = parameter.omschrijving))


write_delim(sedDataWS, file.path(standarddatadir, paste0(lubridate::today(), "_sedimentWS_RWS.csv")), delim = ";")
  
           
           
           
