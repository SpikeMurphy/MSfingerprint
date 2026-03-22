\name{msf_workflow}
\alias{msf_workflow}
\title{Run Full Peptide Mass Fingerprint Analysis Pipeline}
\description{This function performs a complete peptide mass fingerprinting 
    (PMF) analysis workflow for mass spectrometry data. It integrates 
    preprocessing, peak detection, and optional database searches using Mascot 
    and/or MS-Fit.

    The function first calls \code{msf_processing()} to preprocess the input 
    file and detect monoisotopic peaks. The resulting peak list can then be 
    submitted to external search engines depending on user selection.
    
    \strong{Important:} External search services (Mascot and MS-Fit) do not 
    allow automated batch submissions. This function should only be used for 
    individual analyses.
    }
\usage{
msf_workflow <- function(
    FILE, 
    
    MASCOT = FALSE,
    MSFIT = FALSE,
    
    PROCESSING_ARGS = list(),
    
    MASCOT_ARGS = list(),
    
    MSFIT_ARGS = list()
)
}
\arguments{
  \item{FILE}{Character string. Path to the mass spectrometry file (e.g., \code{.msXML}) to be processed.}
  
  \item{MASCOT}{Logical. Whether to perform a Matrix Science Mascot peptide mass fingerprint search using \code{search_mascot()}. The default argument is \code{FALSE}.}
  
  \item{MSFIT}{Logical. Whether to perform a Protein Prospector MS-Fit peptide mass fingerprint search using \code{search_msfit()}. The default argument is \code{FALSE}.}
  
  \item{PROCESSING_ARGS}{List. Additional arguments passed to \code{msf_processing()}.
    
    Common elements include:
    \describe{
      \item{CLEAVAGE}{Character string. Enzyme used for protein digestion. The default argument is \code{"Trypsin"}.}
      \item{KERATIN}{Logical. Whether to identify and remove keratin contaminant peaks. The default argument is \code{TRUE}.}
      \item{TAG}{Character string. Whether to identify and remove peaks from a recombinant protein tag. The default argument is \code{NULL}.}
      \item{EXCLUDE}{Numeric vector. Custom list of m/z values to exclude from the detected peaks.}
      \item{SNR}{Integer. Signal-to-noise ratio threshold used for peak detection. The default argument is \code{30}.}
      \item{PEAKS}{Numeric vector. Threshhold for the number of monoisotopic peaks to detect. The default argument is \code{c(30, 10)} with 30 ± 10 (20 to 40) peaks to be detected.}
      \item{PLOTS}{Logical. Whether to include plots in the returned list. The default argument is \code{TRUE}.}
    }
    See \code{\link{msf_processing}} for full details.
  }
  
  \item{MASCOT_ARGS}{List. Additional arguments passed to \code{search_mascot()}. Only used if \code{MASCOT = TRUE}.
    
    Common elements include:
    \describe{
      \item{USERNAME}{Character string. Name of the user submitting the Mascot search.}
      \item{USEREMAIL}{Character string. Valid email address required by the Mascot server. May need to be confirmed upon first use.}
      \item{DATABASE}{Character string. Protein sequence database to search against. The default argument is \code{"SwissProt"}.}
      \item{CLEAVAGE}{Character string. Enzyme used for protein digestion. The default argument is \code{"Trypsin"}.}
      \item{VARMODS}{Character string. Variable modifications. The default argument is \code{"Oxidation (M)"}.}
      \item{TOL}{Numeric. Peptide mass tolerance value.}
      \item{TOLU}{Character string. Units for mass tolerance. The default argument is \code{"Da"} for Dalton with alternatives being \code{"ppm"} for parts per million, \code{"mmu"} for millimass unit, where 1 mmu = 0.001 Da, or \code{"\%"}.}
    }
    See \code{\link{search_mascot}} for full details.
  }
  
  \item{MSFIT_ARGS}{List. Additional arguments passed to \code{search_msfit()}. Only used if \code{MSFIT = TRUE}.
      
      Common elements include:
      \describe{
        \item{SEQUENCE}{Character string. Optional protein or nucleotide sequence to restrict the search. The default argument is \code{"NULL"}.}
        \item{DATABASE}{Character string. Character string. Protein sequence database to search against. The default argument is \code{"SwissProt.2025.02.26"}.}
        \item{SPECIES}{Character string. Taxonomic restriction for the search. The default argument is \code{"All"}.}
        \item{OUTPUT}{Character string. Output format. The default argument is \code{"HTML"} with an alternative being \code{"XML"}.}
        \item{CLEAVAGE}{Character string. Enzyme used for protein digestion. The default argument is \code{"Trypsin"}.}
        \item{VARMODS}{Character string. Variable modifications. The default argument is \code{"Oxidation of M"}.}
        \item{TOL}{Numeric. Peptide mass tolerance value. The default argument is \code{0.3}.}
        \item{TOLU}{Character string. Units for mass tolerance. The default argument is \code{"Da"} for Dalton with an alternative being \code{"ppm"} for parts per million.}
        \item{MINMATCH}{Integer. Minimum number of peptide matches required. The default argument is \code{4}.}
        \item{MINPMATCH}{Integer. Minimum number of parent mass matches required. The default argument is \code{1}.}
        \item{MAXMODS}{Integer. Maximum number of modifications per peptide. The default argument is \code{1}.}
    }
    See \code{\link{search_msfit}} for full details.
  }
}
\value{
Returns a list of monoisotopic peaks with their mass to charge ratio (m/z) and 
peak intensities fot the target protein and contaminants as well as optional 
plots.

This includes but is not limited to:
\itemize{
  \item{\code{[[1]] monoisotopic_peaks}}: 
  
  {Numeric vector of detected monoisotopic peaks for the target protein.}
  
  
  \item{\code{[[10]] plot_peaks_mono_contaminants}}: 
  
  {Plot of monoisotopic peaks including those for the target protein and the identified contaminants.}
}

See \code{\link{msf_processing}} for full details.
}
\examples{
\dontrun{
# Basic usage
msf_workflow(
  "sample.msXML"
)

# With preprocessing customization
msf_workflow(
  "sample.msXML",
  PROCESSING_ARGS = list(SNR = 20, PEAKS = c(40, 15))
)

# With Mascot search
msf_workflow(
  "sample.msXML",
  MASCOT = TRUE,
  MASCOT_ARGS = list(USERNAME = "John Doe", USEREMAIL = "john@example.com")
)

# With both Mascot and MS-Fit searches
msf_workflow(
  "sample.msXML",
  MASCOT = TRUE,
  MSFIT = TRUE
)
}
}