# TODO: ask for ms digest for tag, then ask for peaklist file for tag from ms-digest (comma separated list)

SCRIPT_NAME = "MassSpectFPAutoTool for Mascot"
SCRIPT_VERSION = "1.0.0"
SCRIPT_AUTHOR = "Spike Murphy Müller"

# ---------------------------------
# USER INFO
# ---------------------------------

# consult the README file at https://github.com/SpikeMurphy/MassSpectFPAutoTool

# ---------------------------------
# INSTALL LIBRARIES IF NEEDED
# ---------------------------------

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!requireNamespace("MALDIquant", quietly = TRUE))
  BiocManager::install("MALDIquant")

if (!requireNamespace("MALDIquantForeign", quietly = TRUE))
  BiocManager::install("MALDIquantForeign")

library(MALDIquant)
library(MALDIquantForeign)

# ---------------------------------
# MASCOT PARAMETERS
# ---------------------------------

mascot_parameters <- list(
  
  USERNAME = "USERNAME",
  USEREMAIL = "USEREMAIL",
  
  SEARCH = "PMF",
  FORMVER = 1.01,
  
  DB = "SwissProt", # database
  
  CLE = "Trypsin", # restriction enzyme
  PFA = 1, # max missed cleavage sites
  
  MODS = "", # definite modifications
  IT_MODS = "Oxidation (M)", # possible modifications
  
  TOL = 0.3, # tolerance
  TOLU = "Da", # toleracne unit
  
  MASS = "Monoisotopic", # Monoisotopic/Average Peaks
  CHARGE = "1+",
  
  REPORT = "AUTO" # amount of reports
)

# convert parameter list to readable text for log
param_lines <- c()
for (n in names(mascot_parameters)) {
  param_lines <- c(
    param_lines,
    paste(n, "=", mascot_parameters[[n]])
  )
}
param_lines <- c(param_lines, "")

# ---------------------------------
# MS-DIGEST PARAMETERS
# ---------------------------------

# ---------------------------------
# SELECT FILES
# ---------------------------------

cat("\nSelect a spectrum file\n")

f <- file.choose()
files <- c(f)

cat("\n---------------------------------\n")
cat("FILE SELECTED\n")
cat("---------------------------------\n")
print(files)

# ---------------------------------
# ASK USER TO RUM MASCOT SEARCH
# ---------------------------------

ans_mascot <- readline("Run Mascot search? (y/n): ")
run_mascot <- tolower(ans_mascot) == "y"

cat("\nSearch settings:\n")
cat("Mascot:", run_mascot, "\n")

# ---------------------------------
# ASK USER TO INCLUDE TAG PEAKS
# ---------------------------------

...

# ---------------------------------
# ASK USER TO RUN MS-DIGEST
# ---------------------------------

... # ask to run... run digest ask user if redy, only then ask for file

# ---------------------------------
# CREATE RESULTS DIRECTORY
# ---------------------------------

base_dir <- dirname(files[1])
output_dir <- file.path(base_dir, "results")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ---------------------------------
# PREPARE LOG
# ---------------------------------

logfile <- file.path(output_dir, "processing_log.txt")
log_lines <- c()

start_time <- Sys.time()

log_lines <- c(
  log_lines,
  "=====================================",
  "MassSpectFPAutoTool for Mascot Log",
  paste("Start time:", format(start_time,"%Y-%m-%d %H:%M:%S")),
  paste("Files processed:", length(files)),
  "=====================================",
  "",
  "-------------------------------------",
  "Mascot Parameters",
  "-------------------------------------",
  param_lines,
  "-------------------------------------",
  "FILES",
  "-------------------------------------",
  paste(seq_along(files), basename(files)),
  ""
)

# summary storage
summary <- data.frame(
  File=character(),
  SNR=numeric(),
  Peaks=integer(),
  Duration=numeric(),
  stringsAsFactors=FALSE
)

# ---------------------------------
# MS-Digest Submission
# ---------------------------------

... #(https://prospector.ucsf.edu/prospector/cgi-bin/msform.cgi?form=msdigest)

# ---------------------------------
# MASCOT PMF SUBMISSION
# ---------------------------------

submit_mascot <- function(mono, mascot_parameters){
  
  if(nrow(mono) == 0) return(NULL)
  
  peaks <- mono$mz
  peak_text <- paste(peaks, collapse="\n")
  
  html <- paste0(
    '<!DOCTYPE html>
<html>
<body onload="document.forms[0].submit()">

<form method="POST"
action="https://www.matrixscience.com/cgi/nph-mascot.exe?1+-batch+-format+msr"
enctype="multipart/form-data">

<input type="hidden" name="USERNAME" value="', mascot_parameters$USERNAME, '">
<input type="hidden" name="USEREMAIL" value="', mascot_parameters$USEREMAIL, '">

<input type="hidden" name="SEARCH" value="', mascot_parameters$SEARCH, '">
<input type="hidden" name="FORMVER" value="', mascot_parameters$FORMVER, '">

<input type="hidden" name="DB" value="', mascot_parameters$DB, '">

<input type="hidden" name="CLE" value="', mascot_parameters$CLE, '">
<input type="hidden" name="PFA" value="', mascot_parameters$PFA, '">

<input type="hidden" name="MODS" value="', mascot_parameters$MODS, '">
<input type="hidden" name="IT_MODS" value="', mascot_parameters$IT_MODS, '">

<input type="hidden" name="TOL" value="', mascot_parameters$TOL, '">
<input type="hidden" name="TOLU" value="', mascot_parameters$TOLU, '">

<input type="hidden" name="MASS" value="', mascot_parameters$MASS, '">

<input type="hidden" name="CHARGE" value="', mascot_parameters$CHARGE, '">

<input type="hidden" name="REPORT" value="', mascot_parameters$REPORT, '">

<textarea name="QUE">', peak_text, '</textarea>

</form>
</body>
</html>'
  )
  
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp)
  
  browseURL(tmp)
}

# ---------------------------------
# PROCESS FILES
# ---------------------------------

for (i in seq_along(files)) {
  f <- files[i]
  file_start <- Sys.time()
  
  cat("\nProcessing:", basename(f), "\n")
  
  try({
    # preprocessing of spectrums
    spectra_raw <- import(f)
    
    spectra_processed <- spectra_raw
    spectra_processed <- transformIntensity(spectra_processed, method="sqrt")
    spectra_processed <- smoothIntensity(spectra_processed, method="SavitzkyGolay", halfWindowSize=10)
    spectra_processed <- removeBaseline(spectra_processed, method="SNIP", iterations=100)
    spectra_processed <- calibrateIntensity(spectra_processed, method="TIC")
    
    spectra <- spectra_processed
    
    # initilize variables
    snr <- 30
    attempts <- 0
    
    repeat {
      
      peaks <- detectPeaks(
        spectra,
        SNR = snr,
        halfWindowSize = 20
      )
      
      # remove trypsin peaks
      trypsin_peaks <- c(
        802.4, 842.87, 1046.00, 1567.6, 1713.8, 1940.9, 2083.4, 2211.10
        # source: https://home.pavlab.msl.ubc.ca/wp-content/uploads/2013/06/Notes-on-trouble-shooting-LCMS-contamination-full.pdf
      )
      tol <- 3
      mz_vals <- mass(peaks[[1]])
      remove_idx <- sapply(mz_vals, function(m)
        any(abs(m - trypsin_peaks) < tol)
      )
      trypsin_removed <- data.frame(
        mz = mz_vals[remove_idx],
        intensity = intensity(peaks[[1]])[remove_idx]
      )
      # create monoisotopic trypsin peaks
      trypsin_mono <- trypsin_removed[order(trypsin_removed$mz), ]
      if(nrow(trypsin_mono) > 0){
        trypsin_mono <- trypsin_mono[c(TRUE, diff(trypsin_mono$mz) > 1.2), ]
      }
      peaks[[1]] <- peaks[[1]][!remove_idx]
      
      # remove keratin peaks
      keratin_peaks <- c(
        704.4, 809.4, 827.4, 875.0, 973.5, 995.5, 1000.6, 1003.5, 1031.6, 1033.5, 1036.5, 1060.6, 1066.0, 1129.6, 1141.5, 1165.6, 1179.6, 1190.6, 1262.6, 1265.6, 1277.7, 1278.5, 1301.7, 1302.7, 1307.7, 1315.7, 1340.6, 1344.7, 1350.7, 1357.7, 1371.7, 1381.6, 1383.7, 1390.7, 1394.56, 1418.7, 1419.7, 1442.8, 1453.8, 1475.8, 1476.7, 1493.7, 1549.6, 1586.8, 1599.8, 1657.8, 1708.8, 1716.8, 1765.7, 1791.7, 1792.9, 1798.1, 1837.9, 1847.8, 1851.9, 1994.0, 2082.9, 2109.0, 2171.0, 2184.1, 2240.1, 2330.49, 2501.2, 2510.1, 2565.88, 2581.1, 2705.1, 2746.4, 2872.4, 2902.7, 2904.4, 2932.5, 3223.2
        # source: https://home.pavlab.msl.ubc.ca/wp-content/uploads/2013/06/Notes-on-trouble-shooting-LCMS-contamination-full.pdf
      )
      tol <- 3
      mz_vals <- mass(peaks[[1]])
      keratin_idx <- sapply(mz_vals, function(m)
        any(abs(m - keratin_peaks) < tol)
      )
      keratin_removed <- data.frame(
        mz = mz_vals[keratin_idx],
        intensity = intensity(peaks[[1]])[keratin_idx]
      )
      # create monoisotopic keratin peaks
      keratin_mono <- keratin_removed[order(keratin_removed$mz), ]
      if(nrow(keratin_mono) > 0){
        keratin_mono <- keratin_mono[c(TRUE, diff(keratin_mono$mz) > 1.2), ]
      }
      peaks[[1]] <- peaks[[1]][!keratin_idx]
      
      p <- peaks[[1]]
      peaklist <- data.frame(
        mz = mass(p),
        intensity = intensity(p)
      )

      # remove individual peaks
      individual_peaks <- c(
        # remove "#" if tag is applicable
        
        # RFP (Red Fluorescent Protein)
        # 857.4338, 873.4287, 895.536, 924.4825, 1036.5608, 1052.5557, 1052.5775, 1055.5156, 1060.615, 1095.6157, 1109.5851, 1164.5684, 1167.5099, 1183.5048, 1183.6106, 1295.6049, 1311.5998, 1395.7056, 1405.7474, 1544.8108, 1635.8199, 1651.8149, 1656.8632, 1696.8112, 1712.8061, 1763.9149, 1779.9098, 1857.9204, 1862.9833, 1873.9153, 1878.9782, 2208.1124, 2225.0357, 2350.2152, 2366.2101, 2536.3235, 2668.2485, 2697.4075, 2852.3117, 2868.3066, 3079.4751, 3095.47, 3102.3858, 3118.3807, 3323.5347, 3339.5297, 3343.5648, 3357.5553, 3359.5597, 3373.5502, 3862.881
        # source: sequence from uniprot, peaks from MS-Digest (https://prospector.ucsf.edu/prospector/cgi-bin/msform.cgi?form=msdigest)
        # GFP (Green Fluorescent Protein)
        # 806.3502, 821.3941, 919.536, 968.4432, 982.4952, 1050.5214, 1062.6194, 1224.7021, 1266.5783, 1282.5732, 1347.6579, 1477.7645, 1503.6598, 1533.8271, 1542.7911, 1592.7315, 1608.7264, 1902.9418, 1918.9368, 1958.9706, 1973.9062, 1989.9011, 2068.9545, 2084.9495, 2230.0597, 2246.0546, 2398.2264, 2437.2609, 2590.268, 2606.2629, 2622.2578, 2746.3691, 2762.364, 2778.3589, 2783.4284, 2799.4233, 2937.3836, 2953.3785, 3148.5998, 3169.5638, 3185.5587, 3921.9029
        # source: sequence from uniprot, peaks from MS-Digest (https://prospector.ucsf.edu/prospector/cgi-bin/msform.cgi?form=msdigest)
         
        # custom tag
        # values
      )
      tol <- 3
      mz_vals <- mass(peaks[[1]])
      individual_idx <- sapply(mz_vals, function(m)
        any(abs(m - individual_peaks) < tol)
      )
      individual_removed <- data.frame(
        mz = mz_vals[individual_idx],
        intensity = intensity(peaks[[1]])[individual_idx]
      )
      # create monoisotopic individual peaks
      individual_mono <- individual_removed[order(individual_removed$mz), ]
      if(nrow(individual_mono) > 0){
        individual_mono <- individual_mono[c(TRUE, diff(individual_mono$mz) > 1.2), ]
      }
      peaks[[1]] <- peaks[[1]][!individual_idx]
      
      p <- peaks[[1]]
      peaklist <- data.frame(
        mz = mass(p),
        intensity = intensity(p)
      )
      
      # remove isotopic peaks 
      mono <- peaklist[order(peaklist$mz), ]
      mono <- mono[c(TRUE, diff(mono$mz) > 1.2), ]
      
      # check amout of peaks and adjust SNR
      n_peaks <- nrow(mono)
      if (n_peaks >= 20 && n_peaks <= 40) break
      attempts <- attempts + 1
      if (n_peaks < 20) {
        snr <- snr * 0.8
      } else {
        snr <- snr * 1.2
      }
      if (attempts > 10) break
    }
    
    # ---------------------------------
    # RUN SEARCH
    # ---------------------------------
    
    # submit searches depending on user choice
    if(run_mascot){
      submit_mascot(mono, mascot_parameters)
    }
    
    # ---------------------------------
    # Base Filename
    # ---------------------------------
    
    base <- tools::file_path_sans_ext(basename(f))
    
    # ---------------------------------
    # EXPORT original spectrum
    # ---------------------------------
    
    png(
      file.path(output_dir,
                paste0(base,"_spectrum_original.png")),
      width = 1200,
      height = 800
    )
    
    plot(
      spectra_raw[[1]],
      main = paste(
        "Original Spectrum |",
        basename(f)
      ),
      sub = ""
    )
    
    dev.off()
    
    # ---------------------------------
    # EXPORT preprocessed spectrum
    # ---------------------------------
    
    png(
      file.path(output_dir,
                paste0(base,"_spectrum_preprocessed.png")),
      width = 1200,
      height = 800
    )
    
    plot(
      spectra_processed[[1]],
      main = paste(
        "Preprocessed Spectrum |",
        basename(f)
      ),
      sub = ""
    )
    
    dev.off()
    
    # ---------------------------------
    # EXPORT DETECTED PEAKS PLOT
    # ---------------------------------
    
    png(
      file.path(output_dir,
                paste0(base,"_spectrum_peaks.png")),
      width=1200,
      height=800
    )
    
    plot(
      spectra[[1]],
      main=paste(
        "Detected Peaks |",
        basename(f),
        "| SNR =",round(snr,2)
      ),
      sub=""
    )
    
    points(peaks[[1]],col="blue",pch=19)
    
    points(
      trypsin_removed$mz,
      trypsin_removed$intensity,
      col="orange",
      pch=19,
    )
    
    points(
      keratin_removed$mz,
      keratin_removed$intensity,
      col="brown",
      pch=19,
    )

    points(
      individual_removed$mz,
      individual_removed$intensity,
      col="#777777",
      pch=19,
      )
    
    dev.off()
    
    # ---------------------------------
    # EXPORT MONOISOTOPIC PEAKS PLOT
    # ---------------------------------
    
    png(
      file.path(output_dir,
                paste0(base,"_spectrum_monoisotopic_peaks.png")),
      width=1200,
      height=800
    )
    
    plot(
      spectra[[1]],
      main=paste(
        "Monoisotopic Peaks |",
        basename(f)
      ),
      sub=""
    )
    
    points(
      mono$mz,
      mono$intensity,
      col="forestgreen",
      pch=19
    )
    
    points(
      trypsin_mono$mz,
      trypsin_mono$intensity,
      col="orange",
      pch=19,
    )
    
    points(
      keratin_mono$mz,
      keratin_mono$intensity,
      col="brown",
      pch=19,
    )

    points(
      individual_mono$mz,
      individual_mono$intensity,
      col="#777777",
      pch=19,
      )
    
    dev.off()
    
    # ---------------------------------
    # EXPORT PEAK LIST
    # ---------------------------------
    
    outfile <- file.path(
      output_dir,
      paste0(base,"_monoisotopic_peaks.txt")
    )
    
    write.table(
      mono$mz,
      outfile,
      row.names=FALSE,
      col.names=FALSE
    )
    
    # ---------------------------------
    # EXPORT ADDITIONAL CSV FILES
    # ---------------------------------
    
    write.csv(
      mono,
      file.path(output_dir, paste0(base, "_monoisotopic_peaks.csv")),
      row.names = FALSE
    )
    
    write.csv(
      trypsin_removed,
      file.path(output_dir, paste0(base, "_removed_trypsin_peaks.csv")),
      row.names = FALSE
    )
    
    write.csv(
      keratin_removed,
      file.path(output_dir, paste0(base, "_removed_keratin_peaks.csv")),
      row.names = FALSE
    )
    
    write.csv(
      individual_removed,
      file.path(output_dir, paste0(base, "_removed_individual_peaks.csv")),
      row.names = FALSE
    )
    
    # ---------------------------------
    # LOG
    # ---------------------------------
    
    file_end <- Sys.time()
    
    duration <- round(
      as.numeric(difftime(file_end,file_start,units="secs")),2
    )
    
    # add to summary
    summary <- rbind(
      summary,
      data.frame(
        File=base,
        SNR=round(snr,2),
        Peaks=n_peaks,
        Duration=duration
      )
    )
    
    # add log entry
    log_lines <- c(
      log_lines,
      paste0("Processing file ",i,"/",length(files),": ",basename(f)),
      paste("Start:",format(file_start,"%H:%M:%S")),
      paste("Mono peaks detected:",n_peaks),
      paste("SNR adjustments:",attempts),
      paste("Final SNR:",round(snr,2)),
      paste("Duration:",duration,"sec"),
      paste("Output:",basename(outfile)),
      ""
    )
    
  }, silent=TRUE)
}

# ---------------------------------
# SUMMARY
# ---------------------------------

log_lines <- c(
  log_lines,
  "-------------------------------------",
  "SUMMARY",
  "-------------------------------------",
  sprintf("%-8s %-8s %-8s %-12s",
          "File","SNR","Peaks","Duration")
)

for(i in 1:nrow(summary)){
  
  log_lines <- c(
    log_lines,
    sprintf("%-8s %-8s %-8s %-12s",
            summary$File[i],
            summary$SNR[i],
            summary$Peaks[i],
            summary$Duration[i])
  )
}

end_time <- Sys.time()

runtime <- round(
  as.numeric(difftime(end_time,start_time,units="secs")),2
)

log_lines <- c(
  log_lines,
  "",
  paste("Total runtime:",runtime,"sec"),
  paste("End time:",format(end_time,"%Y-%m-%d %H:%M:%S")),
  "====================================="
)

# ---------------------------------
# WRITE LOG FILE
# ---------------------------------

writeLines(log_lines, logfile)

cat("\nProcessing finished.\n")
cat("Log saved to:\n")
print(logfile)

