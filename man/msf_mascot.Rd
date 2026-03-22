\name{msf_mascot}
\alias{msf_mascot}
\title{Perform a Matrix Science Mascot Peptide Mass Fingerprint Search from a Peaklist}
\description{This function takes as argument a peaklist and sends a request to 
    the Matrix Science Mascot server for peptide mass fingerprint (PMF) 
    database search with specified parameters. Results are opend in the user's 
    default Browser. 
    
    \strong{Important:} Mascot servers do not allow batch submissions. This 
    function should only be used for individual peak lists and must not be used 
    to automate the analysis of multiple peak lists.
}
\usage{
msf_mascot(
    PEAKS = c(...),
    
    USERNAME = "Firstname Middlename Lastname",
    USEREMAIL = "example@mail.test",

    SEARCH = "PMF",
    FORMVER = 1.01,

    DATABASE = "SwissProt",

    CLEAVAGE = "Trypsin",
    PFA = 1,

    CONSTMODS = NULL,
    VARMODS = "Oxidation (M)",

    TOL = 0.3,
    TOLU = "Da",

    MASS = "Monoisotopic",
    CHARGE = "1+",

    REPORT = "AUTO"
)
}
\arguments{
  \item{PEAKS}{Numeric vector. Mass-to-charge (m/z) values representing the peak list used for the peptide mass fingerprint search.}
  
  \item{USERNAME}{Character string. Name of the user submitting the Mascot search.}
  \item{USEREMAIL}{Character string. Valid email address required by the Mascot server. May need to be confirmed upon first use.}
  
  \item{SEARCH}{Character string. Title of the search. The default argument is \code{"PMF"}.}
  \item{FORMVER}{Numeric. Version of the Mascot HTML form specification to use.}
  
  \item{DATABASE}{Character string. Protein sequence database to search against. The default argument is \code{"SwissProt"}.}
  
  \item{CLEAVAGE}{Character string. Enzyme used for protein digestion. The default argument is \code{"Trypsin"}.}
  \item{PFA}{Integer. Number of allowed missed cleavages during enzymatic digestion. The default argument is \code{1}.}
  
  \item{CONSTMODS}{Character string. Fixed modifications. The default argument is \code{"NULL"}.}
  \item{VARMODS}{Character string. Variable modifications. The default argument is \code{"Oxidation (M)"}.}
    
  \item{TOL}{Numeric. Peptide mass tolerance value.}
  \item{TOLU}{Character string. Units for mass tolerance. The default argument is \code{"Da"} for Dalton with alternatives being \code{"ppm"} for parts per million, \code{"mmu"} for millimass unit, where 1 mmu = 0.001 Da, or \code{"\%"}.}
        
  \item{MASS}{Character string. Type of mass values used. The default argument is \code{"Monoisotopic"} and should not be changed due to the current lack of alternatives.}
  \item{CHARGE}{Character string. Charge state of the ions. The default argument is \code{"1+"} and should not be changed due to the current lack of alternatives.}
  
  \item{REPORT}{Character string. Number of results shown. The default argument is \code{"AUTO"}. The amount of results can still be changed afterwards.}
}
\value{
No return value. As a side effect, the function opens the user's default web browser 
and submits an HTML-form to the Mascot server to initiate the search.
}
\examples{
\dontrun{
msf_mascot(
    PEAKS = c(
      # human serum albumin
      802.4781, 809.4992, 817.4778, 854.4519, 875.5098, 876.4971, 880.4411, 890.5128,
      892.4371, 915.4175, 927.4934, 940.4483, 951.4418, 960.5625, 973.5214, 984.4884,
      1000.6037, 1002.5578, 1013.4244, 1013.599, 1017.5364, 1018.4775, 1019.5785,
      1024.455, 1055.5884, 1074.5426, 1083.5946, 1128.6987, 1138.5674, 1140.5354,
      1141.5194, 1141.6939, 1149.5759, 1149.615, 1157.6538, 1191.5748, 1226.6052,
      1252.6572, 1257.5238, 1296.7046, 1311.7419, 1320.4905, 1336.4854, 1341.7559,
      1342.6348, 1352.6661, 1352.7685, 1358.6297, 1381.5334, 1384.5355, 1386.6206,
      1432.7538, 1442.6104, 1467.8431, 1490.9053, 1511.8428, 1548.6821, 1600.7312,
      1623.7876, 1623.9581, 1627.7996, 1639.7825, 1639.9377, 1650.8949, 1657.7751,
      1676.7481, 1692.743, 1742.8941, 1779.8887, 1795.8836, 1840.915, 1853.9102,
      1856.9099, 1875.0157, 1884.9008, 1898.9952, 1898.9952, 1915.7731, 1924.0862,
      1939.9079, 1953.9231, 1959.7881, 1975.783, 2037.0844, 2045.0954, 2177.9698,
      2193.9647, 2203.0012, 2296.2199, 2300.1056, 2312.2148, 2316.1005, 2347.0039,
      2383.0541, 2404.1709, 2414.0533, 2420.1659, 2433.2635, 2488.1449, 2496.3438,
      2515.1327, 2542.2759, 2560.272, 2576.267, 2593.2425, 2609.2374, 2686.2269,
      2721.3375, 2737.3324, 2859.3474, 2917.323, 2933.3179, 3059.4999, 3075.4948,
      3362.5225, 3365.6875, 3378.5174, 3407.6109, 3514.673, 3530.6679, 3563.8607,
      3626.8087
    ),
    
    USERNAME = "Firstname Middlename Lastname",
    USEREMAIL = "example@mail.test",
)
  
msf_mascot(
    PEAKS = c(
      # human serum albumin
      802.4781, 809.4992, 817.4778, 854.4519, 875.5098, 876.4971, 880.4411, 890.5128,
      892.4371, 915.4175, 927.4934, 940.4483, 951.4418, 960.5625, 973.5214, 984.4884,
      1000.6037, 1002.5578, 1013.4244, 1013.599, 1017.5364, 1018.4775, 1019.5785,
      1024.455, 1055.5884, 1074.5426, 1083.5946, 1128.6987, 1138.5674, 1140.5354,
      1141.5194, 1141.6939, 1149.5759, 1149.615, 1157.6538, 1191.5748, 1226.6052,
      1252.6572, 1257.5238, 1296.7046, 1311.7419, 1320.4905, 1336.4854, 1341.7559,
      1342.6348, 1352.6661, 1352.7685, 1358.6297, 1381.5334, 1384.5355, 1386.6206,
      1432.7538, 1442.6104, 1467.8431, 1490.9053, 1511.8428, 1548.6821, 1600.7312,
      1623.7876, 1623.9581, 1627.7996, 1639.7825, 1639.9377, 1650.8949, 1657.7751,
      1676.7481, 1692.743, 1742.8941, 1779.8887, 1795.8836, 1840.915, 1853.9102,
      1856.9099, 1875.0157, 1884.9008, 1898.9952, 1898.9952, 1915.7731, 1924.0862,
      1939.9079, 1953.9231, 1959.7881, 1975.783, 2037.0844, 2045.0954, 2177.9698,
      2193.9647, 2203.0012, 2296.2199, 2300.1056, 2312.2148, 2316.1005, 2347.0039,
      2383.0541, 2404.1709, 2414.0533, 2420.1659, 2433.2635, 2488.1449, 2496.3438,
      2515.1327, 2542.2759, 2560.272, 2576.267, 2593.2425, 2609.2374, 2686.2269,
      2721.3375, 2737.3324, 2859.3474, 2917.323, 2933.3179, 3059.4999, 3075.4948,
      3362.5225, 3365.6875, 3378.5174, 3407.6109, 3514.673, 3530.6679, 3563.8607,
      3626.8087
    ),
    
    USERNAME = "Firstname Middlename Lastname",
    USEREMAIL = "example@mail.test",

    SEARCH = "PMF",
    FORMVER = 1.01,

    DATABASE = "SwissProt",

    CLEAVAGE = "Trypsin",
    PFA = 1,

    CONSTMODS = "",
    VARMODS = "Oxidation (M)",

    TOL = 0.3,
    TOLU = "Da",

    MASS = "Monoisotopic",
    CHARGE = "1+",

    REPORT = "AUTO"
)
}
}