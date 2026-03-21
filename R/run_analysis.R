run_analysis <- function(
    msfile = "",
    snr = 30,
    peakrange = c(20, 30),
    peaklists = TRUE,
    plots = TRUE,
    mascot = FALSE,
    msfit = FALSE) {
  if (msfile == "") {
    raise_error("I file path needs to be supplied as the first argument.")
  }
  
  # process_file
  run_processing(params = ...)
  
  # run mascot
  if (mascot == TRUE) {
    run_mascot(params = ...)
  }
  
  # run ms-fit
  if (msfit == TRUE) {
    run_masfit(params = ...)
  }
  
  
  
  
}
