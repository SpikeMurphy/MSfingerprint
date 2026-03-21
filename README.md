msfingerprint
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# msfingerprint

<!-- badges: start -->

<!-- badges: end -->

**msfingerprint** provides an automated workflow for preprocessing MALDI
mass spectrometry data, detecting and cleaning peaks, removing
contaminants, and submitting peptide mass fingerprinting (PMF) data to
external identification tools.

The package is designed to streamline analysis pipelines from raw
spectra to protein identification.

------------------------------------------------------------------------

## Features

- Preprocessing of MALDI spectra using `MALDIquant`

- Noise reduction, baseline correction, and intensity calibration

- Adaptive peak detection based on signal-to-noise ratio

- Removal of common contaminants:

  - Trypsin (planned: other restriction enzymes)
  - Keratin
  - Optional tags like GFP, RFP (planned: other common tags)
  - User specified peaks like other tags or contaminants which can be
    obtained using `search_msdigest()`

- Monoisotopic peak extraction

- Visualization using `ggplot2`

- Integration with external tools:

  - Mascot (PMF search)
  - MS-Fit (PMF search)
  - MS-Digest (in silico protein digest and m/z peak calculation)

------------------------------------------------------------------------

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("SpikeMurphy/MassSpectFPAutoTool")
```

------------------------------------------------------------------------

## Quick Start

### 1. Loading Package

``` r
library(msfingerprint)
```

### 2. Processing Spectrum

``` r
result <- run_processing(FILE = "~/dir/example.mzML")
```

### 3. Accessing Results

``` r
# Monoisotopic peaks
result$monoisotopic_peaks
result$monoisotopic_peaks_cleavage
result$monoisotopic_peaks_keratin
result$monoisotopic_peaks_tag
result$monoisotopic_peaks_custom

# Plots
result$plot_raw
result$plot_preprocessed
result$plot_peaks
result$plot_peaks_mono
result$plot_peaks_mono_contaminants
```

### 4. Visualizing Data

``` r
# Plots
print(result$plot_raw)
print(result$plot_preprocessed)
print(result$plot_peaks)
print(result$plot_peaks_mono)
print(result$plot_peaks_mono_contaminants)
```

### 5. Searching Database

``` r
peaklist <- result$monoisotopic_peaks$peaks

# Mascot
search_mascot(
  PEAKS = peaklist,
  USERNAME = "Firstname Lastname",
  USEREMAIL = "example@email.test"
)

# MS-Fit
search_msfit(PEAKS = peaklist)
```

------------------------------------------------------------------------

## Workflow Overview

The main processing pipeline:

1.  Import spectrum (`MALDIquantForeign`)
2.  Transform intensity
3.  Smooth signal
4.  Remove baseline
5.  Calibrate intensities
6.  Detect peaks
7.  Remove contaminants
8.  Extract monoisotopic peaks
9.  Generate plots
10. Perform database search

------------------------------------------------------------------------

## Contaminant Handling

The function `run_processing()` removes known contaminant peaks:

- **Trypsin autolysis peaks**

- **Keratin contamination**

- Tags

  - GFP tag

  - RFP tag

- Custom exclusion list

Tolerance-based matching is used to identify contaminant peaks.

------------------------------------------------------------------------

## External Search Integration

### Mascot

``` r
search_mascot(
  PEAKS = result$monoisotopic_peaks$peaks,
  USERNAME = "Firstname Lastname",
  USEREMAIL = "example@email.test"
)
```

### MS-Fit

``` r
search_msfit(
  PEAKS = result$monoisotopic_peaks$peaks
)
```

### MS-Digest

``` r
search_msdigest(
  SEQUENCE = "MKWVTFISLLFLFSSAYSRGVFRRDAHKSEVAHRFKDLGEENFKALVLIA"
)
```

------------------------------------------------------------------------

## Main Functions

| Function | Description |
|----|----|
| `run_processing()` | Main preprocessing and peak detection pipeline |
| `search_mascot()` | Protein identification *Matrix Science* **Mascot** Server |
| `search_msfit()` | Protein identification via *Protein Prospector* **MS-Fit** Server |
| `search_msdigest()` | Perform in silico digestion and peaklist calculation |

------------------------------------------------------------------------

## Output

The main function returns:

- Clean monoisotopic peaks
  - Target protein peaks
  - Restriction enzyme autolysis peaks
  - Keratin contamination peaks
  - Custom peaklist matches
- Multiple `ggplot2` visualizations
  - Raw spectrum
  - Preprocessed spectrum
  - Plot with detected monoisotopic and isotopic peaks for the target
    protein
  - Plot with detected monoisotopic peaks for the target protein
  - Plot with detected monoisotopic peaks for the target protein and the
    contaminants

``` r
file <- system.file("extdata", "example.mzML", package = "msfingerprint")

result <- run_processing(FILE = file)

names(result)
str(result$monoisotopic_peaks)
```

------------------------------------------------------------------------

## Dependencies

- `MALDIquant`
- `MALDIquantForeign`
- `ggplot2`

------------------------------------------------------------------------

## Development

Future improvements:

- Complete input validation helpers `preprocessing_check_input()`
  - Most of the important fields already are validated before server
    submission
- Unified pipeline function `run_analysis()`
  - Complete processing and database search pipeline

------------------------------------------------------------------------

## Bug Reports

Please report issues here:

<https://github.com/SpikeMurphy/MassSpectFPAutoTool/issues>

------------------------------------------------------------------------

## Author

**Spike Murphy Müller**

ORCID: 0009-0003-3561-7991

------------------------------------------------------------------------

## Citation

Müller, S. M. (2026). *MassSpectFPAutoTool (msfingerprint): Tools for
Automated Peptide Mass Fingerprint Analysis*. R package version 1.0.0,
<https://github.com/SpikeMurphy/MassSpectFPAutoTool>.

------------------------------------------------------------------------

## License

This package is available free of charge under the MIT License:

<https://github.com/SpikeMurphy/MassSpectFPAutoTool/LICENSE.md>
