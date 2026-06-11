


filelist <- list.files("data/Deltares/combined_ppp/", full.names = T)
filelist <- filelist[!grepl("old", filelist)]
filelist <- filelist[!grepl("mean", filelist)]

pp <- lapply(filelist,
             \(x) read_delim(x, delim = ";")
) %>%
  map(
    \(x) x %>% mutate(compartiment = as.character(compartiment))
  ) %>%
  bind_rows() %>%
  mutate(
    compartiment = case_when(
      grepl("7", compartiment) ~ "7",
      .default = compartiment
    )
  )



require(magrittr)

pp %>%
  filter(parameter == "PP_depth_integrated") %>%
  mutate(datetime = as_datetime(datetime)) %>%
  ggplot(aes(datetime, value)) +
  geom_point(aes(color = method), linewidth = 1, alpha = 0.3) +
  facet_wrap("compartiment")



pp %>%
  filter(parameter == "PP_depth_integrated") %>%
  mutate(datetime = as_datetime(datetime)) %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(year, month, compartiment, method) %>%
  summarize(monthlyMean = mean(value, na.rm = T)) %T>%
  write_delim("data/Deltares/combined_ppp/monthly_mean_integrated_ppp.csv", delim = ";") %>% # export as csv
  mutate(date = ymd(paste(year, month, 15))) %>%
  ggplot(aes(date, monthlyMean)) +
  geom_path(aes(color = method), linewidth = 1) +
  facet_wrap("compartiment")
