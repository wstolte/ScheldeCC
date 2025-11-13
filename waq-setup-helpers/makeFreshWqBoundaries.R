


require(tidyverse)
require(leaflet)
library(rworldxtra)
library(htmltools)
library(sf)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

data("countriesHigh") 
countriesHigh <- st_as_sf(countriesHigh)

files <-  c("p:/11210350-002-sito-ps-oosterschel/02_pre-processing/Raw_data/ddl/standard/2025-10-30all_ddl_fysisch.csv",
            "p:/11210350-002-sito-ps-oosterschel/02_pre-processing/Raw_data/ddl/standard/2025-06-27all_ddl_chemisch.csv",
            "p:/11210350-002-sito-ps-oosterschel/02_pre-processing/Raw_data/ddl/standard/2024-09-19all_ddl_chla.csv")

# salinity for stations in Westerschelde are missing there. DDL does not return 
#salinity before 2021. Therefore, salinity was obtained from Scheldemonitor.

url_sal <- "http://geo.vliz.be/geoserver/Dataportal/wfs?service=wfs&version=1.1.0&typeName=abiotic_observations&request=GetFeature&outputFormat=text%2Fcsv&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%2813611%5C%2C998%29+AND+imisdatasetid+IN+%28135%5C%2C476%29"

df_sal <- read_csv(url_sal)

sal_stations <- df_sal %>%
  dplyr::filter(
    stationname %in% c(
      "Walcheren 20 km uit de kust",
      "Walcheren 2 km uit de kust",
      "Vlissingen boei SSVH",      
      "Terneuzen boei 20",
      "Hansweert boei OHMG",
      "Hansweert geul",
      "Schaar van Ouden Doel"
    )
  ) %>%
  select(
    # latitude,
    # longitude,
    bemonsteringshoogte = depth,
    tijdstip = datetime,
    numeriekewaarde = value,
    parameter.wat.omschrijving = parametername,
    eenheid.code = parameterunit,
    locatie.naam = stationname
  )

# format omwerken naar bruikbaar format voor combineren met rest van de data

wq.list <- map(
  files, 
  \(x) read_delim(x,
                  delim = ";"
  )
)

# bind rows and filter for parameters and locations

wq <- bind_rows(wq.list) %>%
  filter(
    locatie.code %in% c(
      "WALCRN20",
      "WALCRN2",
      "VLISSGBISSVH",
      "TERNZBI20",
      "HANSWGL",
      "SCHAARVODDL"
    )
  ) %>%
  select(
    locatie.naam,
    tijdstip,
    parameter.wat.omschrijving,
    eenheid.code,
    grootheid.code,
    parameter.code,
    bemonsteringshoogte,
    hoedanigheid.omschrijving,
    numeriekewaarde
  )

wq_combined <- wq %>% 
  bind_rows(sal_stations) %>%
  mutate(
    parameter.wat.omschrijving = case_when(
      parameter.wat.omschrijving == "Saliniteit Oppervlaktewater " ~ "Saliniteit in DIMSLS in oppervlaktewater",
      .default = parameter.wat.omschrijving)
  ) %>%
  mutate(
    parameter_short = case_when(
      parameter.wat.omschrijving == "(massa)Concentratie ammonium in Oppervlaktewater uitgedrukt in stikstof / opgeloste fractie in mg/l"                ~ "NH4Nnf_mg/l",
      parameter.wat.omschrijving == "(massa)Concentratie fosfor totaal in Oppervlaktewater uitgedrukt in Fosfor na filtratie in mg/l"                    ~ "PorgPnf_mg/l",
      parameter.wat.omschrijving == "(massa)Concentratie koolstof organisch in Oppervlaktewater uitgedrukt in koolstof / opgeloste fractie in mg/l"      ~ "CorgCpg_mg/l",
      parameter.wat.omschrijving == "(massa)Concentratie nitraat in Oppervlaktewater uitgedrukt in stikstof / opgeloste fractie in mg/l"                 ~ "NO3Nnf_mg/l",
      parameter.wat.omschrijving == "(massa)Concentratie orthofosfaat in Oppervlaktewater uitgedrukt in Fosfor na filtratie in mg/l"                     ~ "PO4Pnf_mg/l",
      parameter.wat.omschrijving ==  "(massa)Concentratie siliciumdioxide in Oppervlaktewater uitgedrukt in Silicium na filtratie in mg/l"               ~ "SiO2Sinf_mg/l",
      parameter.wat.omschrijving ==  "(massa)Concentratie som nitraat en nitriet in Oppervlaktewater uitgedrukt in stikstof / opgeloste fractie in mg/l" ~ "NO2NO3Nnf_mg/l",
      parameter.wat.omschrijving ==  "(massa)Concentratie stikstof totaal in Oppervlaktewater uitgedrukt in stikstof / opgeloste fractie in mg/l"        ~ "NtotNnf_mg/l",
      parameter.wat.omschrijving ==  "(massa)Concentratie zuurstof in Oppervlaktewater mg/l"                                                             ~ "O2NVT_mg/l",
      parameter.wat.omschrijving ==  "Saliniteit Oppervlaktewater "                                                                                      ~ "Sal_psu",  # uit Scheldemonitor
      parameter.wat.omschrijving ==  "Temperatuur Oppervlaktewater oC"                                                                                  ~ "Temp_oC",
      parameter.wat.omschrijving ==  "(massa)Concentratie chlorofyl-a in Oppervlaktewater ug/l"                                                         ~ "Chlfa_ug/l",
      parameter.wat.omschrijving ==  "(massa)Concentratie nitriet in Oppervlaktewater uitgedrukt in stikstof / opgeloste fractie in mg/l"               ~ "NO2Nnf_mg/l",
      parameter.wat.omschrijving ==  "Saliniteit in DIMSLS in oppervlaktewater"                                                                         ~ "Sal_psu",
      .default = parameter.wat.omschrijving 
    )
  ) %>%
  mutate(year = as.integer(format(tijdstip, "%Y")),
         month = as.integer(format(tijdstip, "%m")),
         season = ifelse(month >2 & month < 10, "summer", "winter")
  ) %>%
  # add indices for quarters
  mutate(
    quarter = case_when(
      .$month %in% c(1:3) ~ 1,
      .$month %in% c(4:6) ~ 2,
      .$month %in% c(7:9) ~ 3,
      .$month %in% c(10:12) ~ 4
    )
  ) 



wq_combined %>% count(locatie.naam, parameter.wat.omschrijving, parameter_short) %>% View()


## Construct upstream concentrations from other data by regression
## Wq as function of salinity

wq_Sal <- wq_combined %>%
  mutate(month = month(tijdstip)) %>%
  select(
    year,
    month,
    locatie.naam,
    parameter_short,
    numeriekewaarde,
    quarter
  ) %>%
  group_by(parameter_short, locatie.naam) %>%
  mutate(numeriekewaarde2 = remove_outliers(numeriekewaarde)) %>%
  group_by(year, month, locatie.naam, parameter_short, quarter) %>%
  summarize(numeriekewaarde = mean(numeriekewaarde2, na.rm = T), .groups = "drop") %>%
  pivot_wider(names_from = parameter_short, values_from = numeriekewaarde) %>% 
  dplyr::select(
    -`Temp_oC`,
    -`NO2Nnf_mg/l`,
    -`Chlfa_ug/l`,
    - `(massa)Concentratie Chemisch zuurstofverbruik in Oppervlaktewater mg/l`,
    # -`Massafractie koolstof in Zwevende stof t.o.v. drooggewicht in %`,
    -`Massafractie koolstof anorganisch in Zwevende stof t.o.v. drooggewicht in %`,
    # -`(massa)Concentratie zuurstof in Oppervlaktewater mg/l`,
    ) %>% 
  pivot_longer(cols = -c(year, month, locatie.naam, quarter, `Sal_psu`)) %>%
  # rename(Salinity = `Saliniteit in DIMSLS in oppervlaktewater`) %>%
  filter(!is.na(Sal_psu)) %>%
  filter(!is.na(value))

wq_Sal %>% count(locatie.naam)
wq_Sal %>% count(name)

model <- wq_Sal %>%
  group_by(year, quarter, name) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(value ~ Sal_psu, data = .x)),
    tidy = map(model, broom::tidy),
    glance = map(model, broom::glance),
    augment = map(model, \(x) broom::augment(x, conf.int = T)),
    intercept = map_dbl(model, ~ broom::tidy(.x) %>% filter(term == "(Intercept)") %>% pull(estimate)),
    std.error = map_dbl(model, ~ broom::tidy(.x) %>% filter(term == "(Intercept)") %>% pull(std.error)),
    slope = map_dbl(model, ~ broom::tidy(.x) %>% filter(term == "Sal_psu") %>% pull(estimate)),
    rsq = glance %>% map_dbl("r.squared"),
    p = glance %>% map_dbl("p.value")
  ) %>%
  mutate(
    mean = map_dbl(
      data, 
      \(x) mean(x$value)
      )
    )


# plot correlations with lines

model %>%
  select(
    name,
    quarter,
    data,
    augment
  ) %>%
  unnest(c(data, augment), names_sep = "_") %>%
  ggplot(aes(data_Sal_psu, data_value)) +
  geom_point(aes(color = as.factor(year))) +
  geom_line(aes(augment_Sal_psu, augment_.fitted, color = as.factor(year))) +
  facet_grid(name ~ quarter, scales = "free") +
  theme(strip.text.y = element_text(angle = 0))

ggsave("waq-setup-helpers/results/nutrients_vs_salinity.png", height = 7.5, width = 9)

# write intercepts and averages to file.




model %>% ungroup() %>%
  select(
    name,
    year,
    quarter,
    data,
    augment,
    intercept,
    slope
  ) %>%
  unnest(c(data, augment), names_sep = "_") %>%
  ggplot(aes(quarter, intercept)) +
  geom_line(aes(), linewidth = 1) +
  geom_point(aes(), size = 3) +
  facet_grid(name ~ year, scale = "free_y") +
  theme(
    strip.text.y = element_text(angle = 0)
  )

ggsave("waq-setup-helpers/results/time_intercept_sal_nut.png", height = 7.5, width = 9)


# csv uitvoer maken met Delwaq namen
# slope
# intercept + std.error
# R2 of p

model %>% ungroup() %>%
  select(
    name,
    year,
    quarter,
    intercept,
    std.error,
    slope
  ) %>%
  mutate(
    alpha = ifelse(year == 2018, 0.5, 0.3),
    size = ifelse(year == 2018, 2, 1)
  ) %>%
ggplot(aes(color = as.factor(year), y = intercept, x = quarter)) +
  geom_ribbon(
    aes(
      ymin = intercept - std.error,
      ymax = intercept + std.error,
      # fill = as.factor(year),
      alpha = alpha
    ), 
    fill = "grey"
    # color = "white"
  ) +
  geom_point(aes(size = size)) +
  geom_line(aes(linewidth = size, alpha = alpha)) +
  scale_alpha_continuous(range = c(0,1)) +
  scale_size_continuous(range = c(1,2)) +
  scale_linewidth_continuous(range = c(0.7,1.5)) +
  facet_wrap("name", scale = "free_y") +
  guides(
    size = 'none',
    alpha = 'none',
    linewidth = 'none'
  )


ggsave("waq-setup-helpers/results/compare_years_intercept_sal_nut.png", height = 7.5, width = 9)


model %>% ungroup() %>%
  select(
    name,
    year,
    quarter,
    intercept,
    std.error,
    slope
  ) %>%
  mutate(
    conc_at_sea = intercept + slope * 35 
  ) %>% 
  ggplot(aes(y = conc_at_sea)) +
  geom_histogram(aes(fill = name))


model %>% ungroup() %>%
  select(
    name,
    year,
    quarter,
    intercept,
    std.error,
    slope,
    mean
  ) %>%
  mutate(
    intercept = signif(intercept, 4),
    std.error = signif(std.error, 4),
    slope = signif(slope, 4),
    mean = signif(mean, 4)
  ) %>%
  mutate(
    datetime = case_when(
      quarter == "1" ~ "2014/01/01 00:00:00",
      quarter == "2" ~ "2014/04/01 00:00:00",
      quarter == "3" ~ "2014/07/01 00:00:00",
      quarter == "4" ~ "2014/10/01 00:00:00"
    )
  ) %>%
  write_delim("waq-setup-helpers/results/freshwaterboundaries.csv", delim = ";")








