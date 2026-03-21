\name{run_analysis}
\alias{run_analysis}
\title{Run Analysis}
\description{Runs full mass spectrometry peptide mass fingerprint analysis}
\usage{
run_analysis(
  msfile,
  snr = 30,
  peakrange = c(20, 30),
  peaklists = TRUE,
  plots = TRUE,
  mascot = FALSE,
  msfit = FALSE
)
}
\arguments{
  \item{msfile}{Character string. Path to the mass spectrometry file (e.g. mzXML).}
  
  \item{snr}{Numeric. Signal-to-noise ratio used for peak detection.}
  
  \item{peak_range}{Numeric vector of length 2. Minimum and maximum number of peaks.}
  
  \item{export_peaks}{Logical. Whether to export detected peaks to file.}
  
  \item{plots}{Logical. Whether to generate and save plots.}
  
  \item{mascot}{Logical. Whether to run Mascot search.}
  
  \item{msfit}{Logical. Whether to run MS-Fit search.}
}
\value{
A list containing processed spectra, detected peaks, and monoisotopic peak data.
}
\examples{
run_analysis(
  msfile = "example.mzXML"
)
run_analysis(
  msfile = "example.mzXML",
  peakrange = c(20, 30),
  peaklists = TRUE,
  plots = FALSE,
  mascot = TRUE
)
run_analysis(
  msfile = "example.mzXML",
  snr = 30,
  peakrange = c(20, 30),
  peaklists = TRUE,
  plots = TRUE,
  mascot = TRUE,
  msfit = FALSE
)
}