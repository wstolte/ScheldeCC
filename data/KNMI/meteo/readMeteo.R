
# https://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script




get_knmi_daydata_long <- function(start, end, vars = "Q:TG", stns = "310", prefer_json = TRUE) {
  library(httr); library(tidyverse); library(stringr); library(jsonlite)
  
  url <- "https://www.daggegevens.knmi.nl/klimatologie/daggegevens"
  
  # JSON preferred
  if (prefer_json) {
    body <- list(start = start, end = end, vars = vars, stns = stns, fmt = "json")
    resp <- POST(url, body = body, encode = "form")
    if (!http_error(resp)) {
      j <- content(resp, "text", encoding = "UTF-8")
      x <- fromJSON(j)
      df <- as.data.frame(x$data, stringsAsFactors = FALSE)
      names(df) <- x$header
      
      out <- df %>%
        mutate(datum = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>%
        pivot_longer(cols = -c(STN, YYYYMMDD, datum),
                     names_to = "parameter",
                     values_to = "value") %>%
        mutate(value = as.numeric(value)) %>%
        select(datum, STN, parameter, value) %>%
        arrange(datum, STN, parameter)
      
      return(out)
    }
  }
  
  # CSV fallback
  body <- list(start = start, end = end, vars = vars, stns = stns, fmt = "csv")
  resp <- POST(url, body = body, encode = "form")
  stop_for_status(resp)
  
  txt <- content(resp, "text", encoding = "UTF-8")
  lines <- readLines(textConnection(txt), warn = FALSE)
  
  # Extract units from header lines
  var_lines <- grep("^#\\s*[A-Z0-9]+\\s*:", lines, value = TRUE)
  unit_map <- tibble(
    parameter = gsub("^#\\s*([A-Z0-9]+).*", "\\1", var_lines),
    unit = str_extract(var_lines, "\\(.*?\\)") %>% str_replace_all("[()]", "")
  )
  
  # Find header and data lines
  data_hdr_i <- which(grepl("^#\\s*STN\\s*,\\s*YYYYMMDD", lines))
  header <- strsplit(sub("^#\\s*", "", lines[data_hdr_i[1]]), ",")[[1]] |> trimws()
  data_lines <- lines[(data_hdr_i[1] + 1):length(lines)]
  data_lines <- data_lines[!grepl("^#", data_lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
  
  clean_csv <- paste(data_lines, collapse = "\n")
  df <- read.csv(text = clean_csv, sep = ",", header = FALSE,
                 col.names = header, fill = TRUE, stringsAsFactors = FALSE)
  
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  
  out <- df %>%
    mutate(datum = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>%
    pivot_longer(cols = -c(STN, YYYYMMDD, datum),
                 names_to = "parameter",
                 values_to = "value") %>%
    mutate(value = as.numeric(value)) %>%
    left_join(unit_map, by = "parameter") %>%
    select(datum, STN, parameter, value, unit) %>%
    arrange(datum, STN, parameter)
  
  return(out)
}


# example

knmi_daily <- get_knmi_daydata_long(start = "19950101", end = "20250101", vars = c("Q:TG"), prefer_json = FALSE)

write_delim(knmi_daily, file = "data/KNMI/meteo/dailymeteo.csv", delim = ";")

