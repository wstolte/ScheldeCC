

require(smwfs)

require(tidyverse)

url = "https://geo.vliz.be/geoserver/Dataportal/wfs?service=wfs&version=1.1.0&typeName=abiotic_observations&request=GetFeature&outputFormat=text%2Fcsv&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%281755%29"

chlor <- read_csv(url)

chlor %>% 
  sample_n(1000) %>%
  ggplot(aes(longitude, latitude)) +
  geom_point(aes(color=value))

# dit zijn tracks


url2 = "https://geo.vliz.be/geoserver/Dataportal/wfs?service=wfs&version=1.1.0&typeName=abiotic_observations&request=GetFeature&outputFormat=text%2Fcsv&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%28998%5C%2C13611%29+AND+imisdatasetid+IN+%28135%5C%2C476%29"

chlor <- read_csv(url2)
chlor %>% 
  group_by(stationname) %>%
  summarise(n = n())

chlor %>% 
  filter(stationname == "Overloop van Hansweert") %>%
  sample_n(20000) %>%
  mutate(deep = ifelse(depth>4.5, "deep", "shallow")) %>%
  # filter(depth > 4.5) %>%
  # sample_n(10000) %>%
  ggplot(aes(datetime, value)) +
  geom_point(aes(color = deep), alpha = 0.2)

# seems to be a short period with measurements. mainly at the surface, but also few in deeper location (~ 6 m)

url3 = "https://geo.vliz.be/geoserver/Dataportal/wfs?service=wfs&version=1.1.0&typeName=abiotic_observations&request=GetFeature&outputFormat=text%2Fcsv&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%28998%29+AND+imisdatasetid+IN+%28476%29"

chlor <- read_csv(url3)
chlor %>% 
  group_by(stationname) %>%
  summarise(n = n())

chlor %>% 
  filter(stationname == "Overloop van Hansweert") %>%
  sample_n(20000) %>%
  mutate(deep = ifelse(depth>4.5, "deep", "shallow")) %>%
  # filter(depth > 4.5) %>%
  # sample_n(10000) %>%
  ggplot(aes(datetime, value)) +
  geom_point(aes(color = deep), alpha = 0.2)

# same as above
# 
# (272)	Geleidendheid (Conductiviteit) in mS/cm in de waterkolom	mS/cm
# (998)	Saliniteit in PSU in oppervlaktewater	PSU
# (1046)	Temperatuur in graden celcius in oppervlaktewater	graden celcius
# (1755)	Chloride in mg/l Uitgedrukt in Chloride in oppervlaktewater	mg/l
# (2562)	Geleidendheid (Conductiviteit) in us/cm in oppervlaktewater	uS/cm
# (13611)	Saliniteit in DIMSLS in oppervlaktewater	DIMSLS

chlor <- smwfs::getSMdata(startyear = 2010, endyear = 2011, parID = c(1046), datasetID = NA)

chlor %>% 
  group_by(stationname) %>%
  summarise(n = n()) %>%
  print(n = 30)

chlor %>% 
  filter(stationname == "Overloop van Hansweert") %>%
  sample_n(20000) %>%
  mutate(deep = ifelse(depth>4.5, "deep", "shallow")) %>%
  # filter(depth > 4.5) %>%
  # sample_n(10000) %>%
  ggplot(aes(datetime, value)) +
  geom_point(aes(color = deep), alpha = 0.2)


require(rwsapi)
metadata <- rwsapi::rws_metadata()
loccodes <- metadata$content$LocatieLijst
which(loccodes$Code == 'OVHA')
loccodes[which(loccodes$Code == 'OVHA'),]
rwsapi::rws_getParameters(metadata, locatiecode = 'OVHA')
