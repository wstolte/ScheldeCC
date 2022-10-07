
require(tidyverse)
require(lubridate)
# refresh locally stored data by downloading from Scheldemonitor

if(!require(smwfs)) devtools::install_github("scheldemonitor/smwfs")

dataIDpath <- "datasetIDs.xlsx"
sheets <- readxl::excel_sheets(dataIDpath)

# storage path of downloaded data
datapath = file.path("../data/Scheldemonitor")

# test <- readxl::read_excel(dataIDpath, sheet = sheets[3]) 

IDs <- lapply(
  sheets, 
  function(x){
    readxl::read_excel(dataIDpath, sheet = x) 
  }
) %>%
  bind_rows() %>% distinct

# Fysisch-chemisch - oppervlaktewater

refresh_fysischchemischoppwater <- function(startyear = 1998, endyear, parameterIDs, filepath = "fysChemOppWater.csv"){
  
  df <- lapply(
    parameterIDs, function(x) getSMdata(startyear, endyear, propname = retrievedcolumns, parID = x) %>%
      mutate(dataproviderid = as.character(dataproviderid))
  ) %>%
    delete.NULLs() %>% 
    bind_rows()
  
  write_csv(df, file.path(datapath, paste(today(), filepath)))
}

# Fysisch-chemisch - zwevend stof

refresh_fysischchemischzwevendstof <- function(startyear = 1998, endyear, parameterIDs, filepath = "fysChemZwevendStof.csv"){
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                            'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                            'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parameterIDs, function(x) getSMdata(startyear, endyear, propname = retrievedcolumns, parID = x)) %>%
    delete.NULLs() %>% 
    bind_rows()
  
  # df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write.csv(df, file.path(datapath, paste(today(), filepath)))
  
}


# Fysisch-chemisch - bodem

refresh_fysischchemischbodem <- function(startyear = 1998, endyear, filepath = "fysChemBodem.csv"){
  
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
write.csv(df, filepath)

}

