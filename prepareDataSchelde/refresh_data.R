
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

require(smwfs)

refresh_fysischchemischoppwater <- function(startyear = 1998, endyear, filepath = fysChemOppDataPath){
  Saliniteit <- c(998)
  Temperatuur <- c(1046)
  Zuurstof <- c(1214,1213)
  Chlorofyl_a <- c(238,1800)
  BZV_BOD_CZV <- c(125,178)
  Lichtklimaat <- c(461,495)
  Zwevende_stof <- c(1223)
  Nutrienten <- c(26,2829,1996,528,2370,529,530,531,1843,1019,828,834,866,1010,3217,833,1021,1022,1972)
  Organisch_koolstof <- c(663,674)
  Metalen <- c(133,1737,259,260,268,269,1178,2014,1181,2018,1201,1202,686,687)
  
  parID <- c(Saliniteit,Temperatuur,Zuurstof,Chlorofyl_a,Lichtklimaat,Zwevende_stof,Nutrienten,Organisch_koolstof)
  
  #df <- get_y_SMdata(2019, 2020, parID)
  df <- smwfs::get_y_SMdata(startyear = startyear, endyear = endyear, parID = parID)
  # df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write_csv(df, file.path(datapath, filepath), append = T)
}

refresh_fysischchemischoppwater(2000, 2021, filepath = "fyschemoppWater.csv")

# calculate monthly means and yearly means and save 






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

