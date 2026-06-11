

write2CSV_pp <- function(outdata, filename){
  # define directory
  savedir <- "../../data/Deltares/combined_ppp"
  
  expected_names <- c(
    "gebied",
    "compartiment", 
    "datetime", 
    "X_RD", 
    "Y_RD",
    # "notTRUE", # for testing
    "method", 
    "date_added", 
    "source", 
    "script",
    "parameter", 
    "value", 
    "unit"
  )
  
  if(!all(expected_names %in% names(outdata))){
    missing_names <- expected_names[which(!expected_names %in% names(outdata))]
    cat(
      cat(dQuote(missing_names), sep = ", "),
      "columns are missing in dataframe"
    )
  }
  
  stopifnot(
    all(
      expected_names %in% names(outdata) 
    )
  )
  
  write_delim(outdata, file.path(savedir, filename), delim = ";")
  cat("file written")
  
}
