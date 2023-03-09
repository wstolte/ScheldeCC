

#  https://github.com/tomjscox/GPPFourier
#  

if(!require(GPPFourier)){
  remotes::install_github("tomjscox/GPPFourier")
}
require(GPPFourier)
require(tidyverse)
require(lubridate)
require(data.table)
require(leaflet)

# read data

datadir = ""

timeseriespath <- file.path(datadir, "")

timeseriesdata <- read(timeseriespath)


gppData <- timeseriesdata %>%
  drop_na() %>%
  as.data.frame()

## werkt niet als gppData class tibble is. 

plot(gppData$time, gppData$O2)

gppData <- GPPFourier::gapfill(gppData)

gppw <- WindowGPPFourier(x = as.data.frame(gppData),
                         phi = meanLatitude, 
                         lambda = meanLongitude)


gppDays <- WindowGPPFourier.gts(x = gppData,
                                phi = meanLatitude, 
                                lambda = meanLongitude,
                                Width = 1
)

ggplot(gppDays, aes(time, GPP)) +
  geom_line(data = meetboeiData, aes(datumtijd, `ODO mg/L`), color = "blue") +
  geom_line(data = meetboeiData, aes(datumtijd, 1*`Chl ug/L`), color = "green") +
  geom_point() + geom_line() +
  ylab("GPP in mg Oxygen per liter per day") +
  coord_cartesian(ylim = c(NA,15))


