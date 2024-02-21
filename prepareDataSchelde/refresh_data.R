
require(tidyverse)
# refresh locally stored data by downloading from Scheldemonitor
require(smwfs)

# source("r/functions.r") # from project Eerstelijnsrapportage

dataIDpath <- "datasetIDs.xlsx"
sheets <- readxl::excel_sheets(dataIDpath)

getSMdata2 <- function (startyear, endyear, parID, propname = NULL, datasetID = c(588,500, 479, 135, 1527, 476), quiet = F) 
{
  require(httr)
  require(sf)
  require(tidyverse)
  if (!quiet) {
    if (any(!is.na(datasetID))) {
      print("you are currently searching for data in datasetID")
      print(datasetID)
      print("if you want to search in all datasets, choose datasetID = NA")
      print("if you want to search in other datasets, provide a vector of ID's")
    }
  }
  urllist <- structure(
    list(
      scheme = "http", hostname = "geo.vliz.be", 
      port = NULL, path = "geoserver/wfs/ows", query = list(
        service = "WFS", 
        version = "1.1.0", request = "GetFeature", typeName = "Dataportal:abiotic_observations", 
        resultType = "results", viewParams = "placeholder", 
        propertyName = propname, outputFormat = "csv"
        ),
      params = NULL, 
      fragment = NULL, username = NULL, password = NULL
      ),
    class = "url"
  )
  viewParams <- paste(
    "where:obs.context+&&+ARRAY[1]+AND+standardparameterid+IN+(", 
    stringr::str_replace_all(paste(parID, collapse = ","), ",", "\\\\,"), ")+", ifelse(any(!is.na(datasetID)),paste0("AND+imisdatasetid+IN+(", paste(datasetID, collapse = "\\,"), ")+"), ""), "AND+((datetime_search+BETWEEN+'", 
    startyear, "-01-01'+AND+'", endyear, "-12-31'+))", ";context:0001", 
    ";loggedin:1", sep = "")
  urllist$query$viewParams <- viewParams
  urllist$query$viewParams <- stringr::str_replace_all(urllist$query$viewParams, 
                                                       "\\+", " ")
  downloadURL = httr::build_url(urllist)
  result <- httr::RETRY("GET", url = downloadURL, times = 3) %>% 
    httr::content(., "text") %>% readr::read_csv(guess_max = 1e+05) %>% 
    dplyr::mutate(value = dplyr::na_if(as.character(value), "999999999999")) %>% 
    dplyr::mutate(value = as.numeric(value))
  return(result)
}





IDs <- lapply(
  sheets, 
  function(x){
    readxl::read_excel(dataIDpath, sheet = x) 
  }
) %>%
  bind_rows()

# Hydrodynamiek - waterstanden

refresh_waterstanden <- function(base_path, datajaar){
  Waterstand <- c(9695,9694,2438,2439)
  for(jaar in 1998:datajaar){
    df <- smwfs::getSMdata(startyear = jaar, endyear = jaar + 1, parID = c(Waterstand), datasetID = c(476,1527,945))
    write.csv(df, file = file.path(savepath, paste0("Data_Hydro_waterstanden_", jaar,'.csv', sep = "")))
  }
  allFiles <- list.files(file.path(savepath), pattern = "Data_Hydro_waterstanden_", full.names = T)
  allFiles <- allFiles[!grepl("all", allFiles)]
  df <- lapply(
    allFiles, function(x) # nameless function. Wat hierna staat wordt uitgevoerd voor elke elemente van allFiles
      read_delim(x, delim = ",", col_types = cols(.default = "c",
                                                  datetime = "T",
                                                  latitude = "d",
                                                  longitude = "d",
                                                  value = "d")) %>%
      select( # kolomnamen van kolommen die je wilt behouden
        stationname,
        latitude,
        longitude,
        datetime,
        parametername,
        class,
        value) #%>%
  ) %>% bind_rows() # alles wordt geplakt
  
  write_delim(df, file.path(savepath, paste0("Data_Hydro_waterstanden_all_", ".csv")), delim = ",")
  rm(df)
  
}

# Hydrodynamiek - golven

refresh_golven <- function(datajaar){
  Golven <- c(2599,2601,1816,2594,2596,2597,2598)
  for(jaar in 2014:datajaar){
    df <- smwfs::getSMdata(startyear = jaar, endyear = jaar + 1, parID = c(Golven), datasetID = c(8032))
    write.csv(df, file.path(savepath,paste0("Data_Hydro_golven_", jaar,'.csv')))
  }

  # bewerkingen
  
allFiles <- list.files(file.path(savepath), pattern = "Data_Hydro_golven_", full.names = T)
df <- lapply(
  allFiles, function(x) # nameless function. Wat hierna staat wordt uitgevoerd voor elke elemente van allFiles
    read_delim(x, delim = ",", col_types = cols(.default = "c",
                                                datetime = "T",
                                                latitude = "d",
                                                longitude = "d",
                                                value = "d")) %>%
    select( # kolomnamen van kolommen die je wilt behouden
      stationname,
      latitude,
      longitude,
      datetime,
      parametername,
      value) #%>%
) %>% bind_rows() # alles wordt geplakt

cdf_H3 <- plotCDF(df, "H3: Gemiddelde hoogte van het 1/3 deel hoogste golven in cm")
ggsave(cdf_H3, width = 7, height = 4, filename = "Figuren/cdf_H3.png")
cdf_th3 <- plotCDF(df,"TH3: Gemiddelde periode van de golven waaruit H3 bepaald is in 0.1 s")
ggsave(cdf_th3, width = 8, height = 4, filename =  "Figuren/cdf_TH3.png")

df2 <- df %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(stationname, parametername, year, month) %>% 
  summarize(mean = mean(value), max = max(value), n = n(), latitude = mean(latitude), longitude = mean(longitude)) %>%
  mutate(datum = lubridate::ymd(paste(year, month, "15"))) %>%
  select(stationname,
         latitude,
         longitude,
         parametername,
         datetime = datum,
         value = mean,
         value_max = max,
         datapoints = n) %>%
  filter(stationname %in% trendstations) %>%
  mutate(stationname = factor(stationname, levels = trendstations)) %>%
  mutate(parametername = case_when(
    str_detect(parametername, "TH3") ~ "TH3: Gemiddelde periode van de golven waaruit H3 bepaald is in 0.1 s",
    str_detect(parametername, "H3") ~ "H3: Gemiddelde hoogte van het 1/3 deel hoogste golven in cm",
    str_detect(parametername, "TM02") ~ "TM02: Golfperiode berekend uit het spectrum in 0.1 s",
    str_detect(parametername, "Hm0") ~ "Hm0: Significante golfhoogte uit 10mHz spectrum in cm"))

write_delim(df2, file.path(savepath, paste0("Data_Hydro_golven_all", ".csv")), delim = ",")
rm(df)

}



# Fysisch-chemisch - oppervlaktewater

refresh_fysischchemischoppwater <- function(startyear = 1998, endyear, filepath = "Data_FysChem_opp.csv"){
  Saliniteit <- c(13611) #998
  Temperatuur <- c(1046)
  Zuurstof <- c(1214,1213)
  Chlorofyl_a <- c(238,1800)
  BZV_BOD_CZV <- c(125,178)
  Lichtklimaat <- c(461,495)
  Zwevende_stof <- c(1223)
  Nutrienten <- c(26,2829,1996,528,2370,529,530,531,1843,1019,828,834,866,1010,3217,833,1021,1022,1972)
  Organisch_koolstof <- c(663,674)
  Metalen <- c(133,1737,259,260,268,269,1178,2014,1181,2018,1201,1202,686,687)

  parID <- c(
    Saliniteit,
    Temperatuur,
    Zuurstof,
    Chlorofyl_a,
    BZV_BOD_CZV,
    Lichtklimaat,
    Zwevende_stof
    # Nutrienten,
    # Organisch_koolstof,
    # Metalen
    )

  # df <- get_y_SMdata(2019, 2020, parID)
  df <- getSMdata2(startyear = startyear, endyear = endyear, parID = parID)
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write_csv(df, file.path(savepath, filepath))
}


# Fysisch-chemisch - zwevende stof

refresh_fysischchemischzwevendstof <- function(startyear = 1998, endyear, filepath = 'Data_FysChem_zwevend.csv'){
  parIDs <- IDs %>%
    filter(grepl(" in zwevend stof", Parameternaam, ignore.case = T)) %>%
    select(`(Parameternr)`) %>%
    unlist %>% unname
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                        'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                        'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parIDs, function(x) getSMdata(startyear,endyear, propname = retrievedcolumns, parID = x)) %>%
    delete.NULLs() %>% 
    # map( ~ mutate(.x, id = as.numeric(id))) %>%
    bind_rows()
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write.csv(df, file.path(savepath, filepath))
}

# Fysisch-chemisch - bodem

refresh_fysischchemischbodem <- function(startyear = 1998, endyear, filepath = "Data_FysChem_bodem.csv"){
  
parIDs <- IDs %>%
  filter(grepl("in bodem/sediment", Parameternaam, ignore.case = T)) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                          'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                          'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")

df <- lapply(parIDs, function(x) getSMdata(startyear, endyear, propname = retrievedcolumns, parID = x)) %>%
  delete.NULLs() %>% 
  bind_rows()

df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
write.csv(df, file.path(savepath, filepath))

}

refresh_fysischchemischbiota <- function(startyear = 1998, endyear, filepath = "Data_FysChem_biota.csv"){
  
  parIDs <- IDs %>%
    filter(grepl("biota", Parameternaam, ignore.case = T)) %>%
    select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
    unlist %>% unname
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                            'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit', 'class', 'category',
                            'scientificname', 'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parIDs, function(x) getSMdata(startyear, endyear, propname = NULL, parID = x)) %>%
    delete.NULLs() %>% 
    bind_rows()
  
  # df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  
  write_csv(df, file.path(savepath, filepath))
}



refresh_fytoplanktondata <- function(filepath){
  
  url = "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Abiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+imisdatasetid+IN+%28949%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Caphiaid%2Cscientificname%2Cobservationdate%2Clongitude%2Clatitude%2Cvalue%2Cparametername%2Cdataprovider%2Cimisdatasetid%2Cdatasettitle%2Cdatafichetitle%2Cdataficheid%2Careaname%2Cdateprecision%2Cstadium%2Cgender%2Cvaluesign%2Cdepth%2Cclassunit%2Cclass%2Cstandardparameterid%2Cparameterunit&outputFormat=csv"

  df.fytoplankton <- read_csv(url)
  
  write.csv(df.fytoplankton, file.path(savepath, filepath, '.csv'))
  
  
  }

