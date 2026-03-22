\name{msf_digest}
\alias{msf_digest}
\title{Perform a Protein Prospector MS-Digest Search}
\description{This function takes a protein sequence or database entry as input 
    and submits a request to a Protein Prospector MS-Digest server to perform 
    anin silico enzymatic digestion and generate predicted peptide fragments 
    typically used for peptide mass fingerprinting (PMF).
    
    \strong{Important:} MS-Digest servers do not allow batch submissions. This
    function should only be used for individual protein sequences and must not 
    be used to automate the analysis of multiple sequences.
}
\usage{
msf_msdigest(
    SEQUENCE = "...",  
    
    DATABASE = "User Protein",
    ACCESS = "Accession Number",
    ENTRYDATA = NULL,
    
    MAXHITS = 50000,
    
    OUTPUT = "HTML",
    FILE = 0,
    FILENAME = "msdigest",
    
    CLEAVAGE = "Trypsin",
    MISSEDC = 1,
    
    AALIMIT = NULL,
    ENDTERM = 1,
    STRIPTERM = "N",
    STARTSTRIP = 0,
    ENDSTRIP = 4,
    
    MAXFRAGMASS = 500,
    MINFRAGMASS = 4000,
    MINFRAGLEN = 5,

    CONSTMODS = c(),
    VARMODS = c("Oxidation (M)")
)
}
\arguments{
  \item{SEQUENCE}{Character string. Protein sequence as one-letter amino acid code to digest.}

  \item{DATABASE}{Character string. Protein database. The default argument is \code{"User Protein"} to allow search of the entered proteine sequence. 
      Chosing a different database means that a database entry under \code{ENTRYDATA} needs to be input as well.}

  \item{ACCESS}{Character string. Access method whereby the query references into the chosen \code{DATABASE}. The default argument is \code{"Accession Number"} with an alternative being \code{"Index Number"}.}

  \item{ENTRYDATA}{Character string. \code{"Accession Number"} or \code{"Index Number"} referencing into the chosen \code{DATABASE}. The default argument is \code{"NULL"} as it needs to be provided for database searches.}

  \item{MAXHITS}{Integer. Maximum number of hits returned.}

  \item{OUTPUT}{Character string. Output format. The default argument is \code{"HTML"} with an alternative being \code{"XML"} or \code{"Tab delimited text"}.}

  \item{FILE}{Integer (0/1). Whether to write results to a file. Chose between \code{0} for FALSE or \code{1} for TRUE. The default argument is \code{0}.}

  \item{FILENAME}{Character string. Name of the output file. The extension name does not need to be given but is constructed from the \code{FILE} argument.}

  \item{CLEAVAGE}{Character string. Enzyme used for digestion. The default argument is \code{"Trypsin"}.}

  \item{MISSEDC}{Integer. Number of allowed missed cleavages. The default argument is \code{"1"}.}

  \item{AALIMIT}{Character string. Amino-terminal amino acid limit. The default argument is \code{"NULL"}.}

  \item{ENDTERM}{Integer. End terminus specificity flag. Enforces enzyme specificity at the peptide ends. Chose between \code{0} for FALSE or \code{1} for TRUE. The default argument is \code{1}.}

  \item{STRIPTERM}{Character string. Terminal stripping option. Specify which end of the peptide gets trimmed. Chose between \code{N} for amino terminus or \code{C} for carboxyl terminus. The default argument is \code{N}.}

  \item{STARTSTRIP}{Integer. Number of residues to strip from start.}

  \item{ENDSTRIP}{Integer. Number of residues to strip from end.}

  \item{MAXFRAGMASS}{Integer. Minimum fragment mass. The default argument is \code{"500"}.}
  
  \item{MINFRAGMASS}{Integer. Maximum fragment mass. The default argument is \code{"4000"}.}

  \item{MINFRAGLEN}{Integer. Minimum fragment length. The default argument is \code{"5"}.}

  \item{CONSTMODS}{Character vector. Fixed modifications.}

  \item{VARMODS}{Character vector. Variable modifications. The default argument is \code{"Oxidation of M"}.}
}
\value{
No return value. As a side effect, the function opens the user's default browser 
and submits an HTML-form to the MS-Fit server to initiate the search.
}
\examples{
\dontrun{
msf_msdigest()
    SEQUENCE = "MKWVTFISLLFLFSSAYSRGVFRRDAHKSEVAHRFKDLGEENFKALVLIAFAQYLQQCPFEDH
    VKLVNEVTEFAKTCVADESAENCDKSLHTLFGDKLCTVATLRETYGEMADCCAKQEPERNECFLQHKDDNPNLPR
    LVRPEVDVMCTAFHDNEETFLKKYLYEIARRHPYFYAPELLFFAKRYKAAFTECCQAADKAACLLPKLDELRDEG
    KASSAKQRLKCASLQKFGERAFKAWAVARLSQRFPKAEFAEVSKLVTDLTKVHTECCHGDLLECADDRADLAKYI
    CENQDSISSKLKECCEKPLLEKSHCIAEVENDEMPADLPSLAADFVESKDVCKNYAEAKDVFLGMFLYEYARRHP
    DYSVVLLLRLAKTYETTLEKCCAAADPHECYAKVFDEFKPLVEEPQNLIKQNCELFEQLGEYKFQNALLVRYTKK
    VPQVSTPTLVEVSRNLGKVGSKCCKHPEAKRMPCAEDYLSVVLNQLCVLHEKTPVSDRVTKCCTESLVNRRPCFS
    ALEVDETYVPKEFNAETFTFHADICTLSEKERQIKKQTALVELVKHKPKATKEQLKAVMDDFAAFVEKCCKADDK
    ETCFAEEGKKLVAASQAALGL" # human serum albumin
)

msf_msdigest(
    SEQUENCE = "MKWVTFISLLFLFSSAYSRGVFRRDAHKSEVAHRFKDLGEENFKALVLIAFAQYLQQCPFEDH
    VKLVNEVTEFAKTCVADESAENCDKSLHTLFGDKLCTVATLRETYGEMADCCAKQEPERNECFLQHKDDNPNLPR
    LVRPEVDVMCTAFHDNEETFLKKYLYEIARRHPYFYAPELLFFAKRYKAAFTECCQAADKAACLLPKLDELRDEG
    KASSAKQRLKCASLQKFGERAFKAWAVARLSQRFPKAEFAEVSKLVTDLTKVHTECCHGDLLECADDRADLAKYI
    CENQDSISSKLKECCEKPLLEKSHCIAEVENDEMPADLPSLAADFVESKDVCKNYAEAKDVFLGMFLYEYARRHP
    DYSVVLLLRLAKTYETTLEKCCAAADPHECYAKVFDEFKPLVEEPQNLIKQNCELFEQLGEYKFQNALLVRYTKK
    VPQVSTPTLVEVSRNLGKVGSKCCKHPEAKRMPCAEDYLSVVLNQLCVLHEKTPVSDRVTKCCTESLVNRRPCFS
    ALEVDETYVPKEFNAETFTFHADICTLSEKERQIKKQTALVELVKHKPKATKEQLKAVMDDFAAFVEKCCKADDK
    ETCFAEEGKKLVAASQAALGL", # human serum albumin
    
    DATABASE = "User Protein",
    
    MAXHITS = 50000,
    
    OUTPUT = "XML",
    FILE = 1,
    FILENAME = "msdigest",
    
    CLEAVAGE = "Trypsin",
    MISSEDC = 1,
    
    AALIMIT = "",
    ENDTERM = 1,
    STRIPTERM = "N",
    STARTSTRIP = 0,
    ENDSTRIP = 4,
    
    MAXFRAGMASS = 500,
    MINFRAGMASS = 4000,
    MINFRAGLEN = 5,

    CONSTMODS = c(),
    VARMODS = c("Oxidation (M)")
)
}
}
