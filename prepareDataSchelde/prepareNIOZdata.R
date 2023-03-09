

## FRRF

p_report <- readxl::read_excel("data/NIOZ/PPDick/report/MONEOS Final report vs6 - AppendixC.xlsx") %>%
  select(date = Datum,
         compartment = Compartiments,
         `Prim prod (mgC m-2 d-1)`
  ) %>%
  mutate(`Prim prod (gC m-2 d-1)` = `Prim prod (mgC m-2 d-1)`/1000)

write_lines(
  x = c(
    "# calculated from NIOZ data", 
    "# pelagic primary production in gC/m2/d from FRRF",
    "# locations related to areas",
    "# year - year", 
    "# month - month", 
    "# monthlymean - monthly mean gross production in gC/m2/d", 
    "# date - constructed date"),
  file = "data/NIOZ/PPDick/bewerkt/ppp_berekend_per_vak_per_maand.csv"
)


NIOZprodMaand <- p_report %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(compartment, year, month) %>%
  summarize(monthlyMean = mean(`Prim prod (gC m-2 d-1)`, na.rm = T)) %>%
  mutate(date = ymd(paste(year, month, "15"))) %>%
  write_delim("data/NIOZ/PPDick/bewerkt/ppp_berekend_per_vak_per_maand.csv", 
              delim = ";", append = T, col_names = T)
