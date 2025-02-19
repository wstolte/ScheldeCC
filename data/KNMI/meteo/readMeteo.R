
# example
# change station and change to hourly data
# https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script

  require(data.table)
temp <- tempfile()
  download.file(
    url = "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_312.zip", destfile = temp)
unzip(temp, "etmgeg_312.txt", exdir = "data")
unlink(temp)
