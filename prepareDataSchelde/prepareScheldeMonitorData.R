
source("refresh_data.R")



# korrelgroottes en koolstof in Zwevend stof
parIDs <- IDs %>%
  filter(grepl("in zwevend", Parameternaam, ignore.case = T)) %>%
  filter(
    grepl("korrel", Parameternaam, ignore.case = T) |
    grepl("koolstof", Parameternaam, ignore.case = T)
  ) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

refresh_fysischchemischzwevendstof(1970, 2022, parameterIDs = parIDs)


# oppwater uit Scheldemonitor

parIDs <- IDs %>%
  filter((grepl("in water", Parameternaam, ignore.case = T) | grepl("in oppervlaktewater", Parameternaam, ignore.case = T) | grepl("licht", Parameternaam, ignore.case = T))
         & !grepl("in waterbodem", Parameternaam, ignore.case = T)) %>%
  filter(
    grepl("temperatuur", Parameternaam, ignore.case = T) |
      grepl("saliniteit", Parameternaam, ignore.case = T) |
      grepl("nitraat", Parameternaam, ignore.case = T) |
      grepl("ammonium", Parameternaam, ignore.case = T) |
      grepl("fosfaat", Parameternaam, ignore.case = T) |
      grepl("chlorofyl", Parameternaam, ignore.case = T) |
      grepl("doorzicht", Parameternaam, ignore.case = T) |
      grepl("licht", Parameternaam, ignore.case = T) |
      grepl("zuurstof", Parameternaam, ignore.case = T) |
      grepl("koolstof", Parameternaam, ignore.case = T) |
      grepl("silicaat", Parameternaam, ignore.case = T)
  ) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

refresh_fysischchemischoppwater(1970, 2022, parameterIDs = parIDs[[1]])

#check
# Saliniteit <- c(998)
# Temperatuur <- c(1045, 1046)
# Zuurstof <- c(1214,1213)
# Chlorofyl_a <- c(238,1800)
# BZV_BOD_CZV <- c(125,178)
# Lichtklimaat <- c(461,495, 5141, 5134, 1321)
# Zwevende_stof <- c(1223)
# Nutrienten <- c(26,2829,1996,528,2370,529,530,531,1843,1019,828,834,866,1010,3217,833,1021,1022,1972)
# Organisch_koolstof <- c(663,674)
# Metalen <- c(133,1737,259,260,268,269,1178,2014,1181,2018,1201,1202,686,687)
# 
# parIDs <- c(Saliniteit,Temperatuur,Zuurstof,Chlorofyl_a, Lichtklimaat,Zwevende_stof,Nutrienten,Organisch_koolstof)

filepath = "fysChemOppWater.csv"
df.oppwater <- read_csv(file.path(datapath, paste(today(), filepath)))

unique(df.oppwater$parametername)


# parameters in zwevendstof uit Scheldemonitor 

filepath = "fysChemZwevendStof.csv"

df.zwevendstof <- read_csv(file.path(datapath, paste(today(), filepath)))

df.zwevendstof %>%
  filter(grepl("korrel", parametername, ignore.case = T)) %>%
  group_by(parametername) %>% summarize(n = n())

df.zwevendstof %>%
  filter(grepl("koolstof", parametername, ignore.case = T)) %>%
  group_by(parametername) %>% summarize(n = n())

df.zwevendstof %>%
  # filter(grepl("", parametername, ignore.case = T)) %>%
  group_by(parametername) %>% summarize(n = n()) %>% View


