\name{msf_preprocessing}
\alias{msf_preprocessing}
\title{Run preprocessing and peak detection for mass spectrometry peptide mass fingerprinting data}
\description{This function performs preprocessing of a mass spectrometry 
    peptide mass fingerprinting (PMF) file, including intensity transformation, 
    smoothing, baseline correction, calibration, and peak detection. It further 
    identifies monoisotopic peaks and separates potential contaminant peaks 
    originating from cleavage enzymes, keratin contamination, protein tags, 
    or user-defined exclusions.
    
    Optionally, the function generates diagnostic plots and prepares outputs 
    suitable for downstream analysis or database searches.
}
\usage{
msf_processing(
    FILE = "filepath", 
    
    CLEAVAGE = "Trypsin", 
    KERATIN = TRUE, 
    TAG = NULL, 
    EXCLUDE = c(), 
    
    SNR = 30, 
    PEAKS = c(30, 10), 
    
    PLOTS = TRUE, 
    
    TRANSFORM = list(method = "sqrt"), 
    SMOOTH = list(method = "SavitzkyGolay", halfWindowSize = 10), 
    BASELINE = list(method = "SNIP", iterations = 100), 
    CALIBRATE = list(method = "TIC"),
    NOISE = list(method = "SuperSmoother")
)
}
\arguments{
\item{FILE}{Character string. Path to the mass spectrometry file (e.g., \code{.msXML}) to be processed.}
  
  \item{CLEAVAGE}{Character string. Enzyme used for protein digestion. The default argument is \code{"Trypsin"}.}
  
  \item{KERATIN}{Logical. Whether to identify and remove keratin contaminant peaks. The default argument is \code{TRUE}.}
  
  \item{TAG}{Character string. Whether to identify and remove peaks from a recombinant protein tag. The default argument is \code{NULL}.}
  
  \item{EXCLUDE}{Numeric vector. Custom list of m/z values to exclude from the detected peaks.}
  
  \item{SNR}{Integer. Signal-to-noise ratio threshold used for peak detection. The default argument is \code{30}.}
  
  \item{PEAKS}{Numeric vector. Threshhold for the number of monoisotopic peaks to detect. The default argument is \code{c(30, 10)} with 30 ± 10 (20 to 40) peaks to be detected.}
  
  \item{PLOTS}{Logical. Whether to include plots in the returned list. The default argument is \code{TRUE}.}
  
  \item{TRANSFORM}{List. Intensity transformation parameters applied to the spectrum. The default method is \code{"sqrt"} with alternative being \code{"log"}, \code{"log2"} and \code{"log10"}.}
  
  \item{SMOOTH}{List. Smoothing parameters applied to the spectrum. The default method is \code{"SavitzkyGolay"} with a default halfWindowSize of \code{10} with an alternative being \code{"MovingAverage"} with a default halfWindowSize of \code{2}}
  
  \item{BASELINE}{List. Baseline correction parameters applied to the spectrum. The default method is \code{"SNIP"} with alternatives being \code{"TopHat"}, \code{"ConvexHull"} and \code{"median"}.}
  
  \item{CALIBRATE}{List. Calibration parameters applied to the spectrum. The default argument is \code{"TIC"} with alternatives being \code{"PQN"} and \code{"median"}.}
  
  \item{NOISE}{List. Noise determination parameters applied to the spectrum. The default argument is \code{"SuperSmoother"} with an alternative being \code{"MAD"}. The methode \code{"MAD"} generally generates a lover number for the noise.}
\value{
Returns a list of monoisotopic peaks with their mass to charge ratio (m/z) and 
peak intensities fot the target protein and contaminants as well as optional plots:

\itemize{
  \item{\code{[[1]] monoisotopic_peaks}}: 
  
  {Numeric vector of detected monoisotopic peaks for the target protein.}
  
  \item{\code{[[2]] monoisotopic_peaks_cleavage}}: 
  
  {Numeric vector of peaks attributed to the cleavage enzyme.}
  
  \item{\code{[[3]] monoisotopic_peaks_keratin}}: {
  
  Numeric vector of keratin contaminant peaks.}
  
  \item{\code{[[4]] monoisotopic_peaks_tag}}: 
  
  {Numeric vector of peaks corresponding to the specified protein tag.}
  
  \item{\code{[[5]] monoisotopic_peaks_custom}}: 
  
  {Numeric vector of user-defined excluded peaks.}
  
  \item{\code{[[6]] plot_raw}}: 
  
  {Plot of the raw mass spectrometry spectrum.}
  
  \item{\code{[[7]] plot_preprocessed}}: 
  
  {Plot of the preprocessed spectrum after transformation, smoothing, baseline correction and calibration.}
  
  \item{\code{[[8]] plot_peaks}}: 
  
  {Plot of detected peaks including isotopic peaks.}
  
  \item{\code{[[9]] plot_peaks_mono}}: 
  
  {Plot of monoisotopic peaks for the target protein.}
  
  \item{\code{[[10]] plot_peaks_mono_contaminants}}: 
  
  {Plot of monoisotopic peaks including those for the target protein and the identified contaminants.}
}
}
\examples{
\dontrun{
}
msf_processing(
    FILE = "~/dir/example.mzML", 
    
    CLEAVAGE = "Trypsin", 
    KERATIN = TRUE, 
    TAG = NULL, 
    EXCLUDE = c(), 
    
    SNR = 30, 
    PEAKS = c(30, 10), 

    PLOTS = TRUE,
    
    TRANSFORM = list(method = "sqrt"), 
    SMOOTH = list(method = "SavitzkyGolay", halfWindowSize = 10), 
    BASELINE = list(method = "SNIP", iterations = 100), 
    CALIBRATE = list(method ="TIC"),
    NOISE = list(method="SuperSmoother")
)
}
