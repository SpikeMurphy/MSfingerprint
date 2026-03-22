run_analysis <- function(
    FILE, 
    
    MASCOT = FALSE,
    MSFIT = FALSE,
    
    PROCESSING_ARGS = list(),
    
    MASCOT_ARGS = list(),
    
    MSFIT_ARGS = list(),
    
    TEST = FALSE
  ){
  
  # check inputs
  if (TEST == FALSE){
    cat("Checking inputs")
    loading_animation()
  }
  
  check_lists(PROCESSING_ARGS, MASCOT_ARGS, MSFIT_ARGS)
  checkpeaks <- check_peaks(PROCESSING_ARGS, MASCOT_ARGS, MSFIT_ARGS)
  if (checkpeaks$processing == TRUE){
    PROCESSING_ARGS$FILE <- NULL
  }
  if (checkpeaks$mascot == TRUE){
    MASCOT_ARGS$PEAKS <- NULL
  }
  if (checkpeaks$msfit == TRUE){
    MSFIT_ARGS$PEAKS <- NULL
  }

  
  # process_file
  if (TEST == FALSE){
    cat("Processing File")
    loading_animation()
  }
  
  result <- do.call(run_processing, c(list(FILE = FILE), PROCESSING_ARGS))
  
  peaks = result$monoisotopic_peaks$peaks
  
  # run mascot
  if (MASCOT == TRUE) {
    if (TEST == FALSE){
      cat("Running `search_mascot`")
      loading_animation()
    }
    do.call(run_mascot, c(list(PEAKS = peaks), MASCOT_ARGS))
  }
  
  # run ms-fit
  if (MSFIT == TRUE) {
    if (TEST == FALSE){
      cat("Running `search_msfit`")
      loading_animation()
    }
    
    do.call(run_msfit, c(list(PEAKS = peaks), MSFIT_ARGS))
  }
  
  if (TEST == FALSE){
    if (PROCESSING_ARGS$PLOTS == TRUE){
      cat("Processing done. Returning peaklists and plots. \n")
    }else {
      cat("Processing done. Returning peaklists. \n")
    }

  }
  return(result)
  
}


# =================================================== #
# ===== HELPER FUNCTIONS ============================ #
# =================================================== #

loading_animation <- function(t = 0.2){
  for (i in c(1:3)) {
    Sys.sleep(t)
    cat(".")
  }
  Sys.sleep(t)
  cat("\n")
}


# =================================================== #
# ===== TESTING FUNCTIONS =========================== #
# =================================================== #

check_lists <- function(PROCESSING_ARGS, MASCOT_ARGS, MSFIT_ARGS){
  if (!is.list(PROCESSING_ARGS)) {
    stop("Argument `PROCESSING_ARGS` is not a list.")
  }
  if (!is.list(MASCOT_ARGS)) {
    stop("Argument `MASCOT_ARGS` is not a list.")
  }
  if (!is.list(MSFIT_ARGS)) {
    stop("Argument `MSFIT_ARGS` is not a list.")
  }
}


check_peaks <- function(PROCESSING_ARGS, MASCOT_ARGS, MSFIT_ARGS){
  checkpeaks <- list(
    processing = FALSE,
    mascot     = FALSE,
    msfit      = FALSE
  )
  if ("FILE" %in% names(PROCESSING_ARGS)) {
    warning("Argument `FILE` in `PROCESSING_ARGS` is ignored.")
    checkpeaks$processing <- TRUE
  }
  if ("PEAKS" %in% names(MASCOT_ARGS)) {
    warning("Argument `PEAKS` in `MASCOT_ARGS` is ignored.")
    checkpeaks$mascot <- TRUE
  }
  if ("PEAKS" %in% names(MSFIT_ARGS)) {
    warning("Argument `PEAKS` in `MSFIT_ARGS` is ignored.")
    checkpeaks$msfit <- TRUE
  }
  return(checkpeaks)
}
