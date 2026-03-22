# =================================================== #
# ===== MAIN FUNCTION =============================== #
# =================================================== #

msf_msdigest <- function(
    SEQUENCE = "",  
    
    # alternatively from database
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
    VARMODS = c("Oxidation (M)"),
    
    TEST = FALSE
  ){
  
  #check inputs
  if (TEST == FALSE){
    cat("Checking inputs")
    loading_animation()
  }
  
  msdigest_check_empty(SEQUENCE, DATABASE)
  msdigest_check_sequence(SEQUENCE, DATABASE)
  msdigest_check_database(DATABASE, ACCESS, ENTRYDATA)
  msdigest_check_maxhits(MAXHITS)
  msdigest_check_output (OUTPUT, FILE, FILENAME)
  msdigest_check_cleavage (CLEAVAGE, MISSEDC)
  msdigest_check_trim(AALIMIT, ENDTERM, STRIPTERM, STARTSTRIP, ENDSTRIP)
  msdigest_check_fragments(MAXFRAGMASS, MINFRAGMASS, MINFRAGLEN)
  msdigest_check_mods(CONSTMODS, VARMODS)
  
  # Create HTML inputs for contant modifications (CONSTMODS)
  const_mods_html <- if (length(CONSTMODS) > 0) {
    paste0('<input type="hidden" name="const_mod" value="', CONSTMODS, '"/>', collapse = "\n")
  } else {
    ""
  }
  
  # Create HTML inputs for variable modifications (VARMODS)
  var_mods_html <- if (length(VARMODS) > 0) {
    paste0('<input type="hidden" name="mod_AA" value="', VARMODS, '"/>', collapse = "\n")
  } else {
    ""
  }
  
  # create and submit HTML
  if (TEST == FALSE){
    cat("Creating HTML")
    loading_animation()
  }
  
  html <- paste0(
    '<!DOCTYPE html>
    <html>
    <body onload="document.forms[0].submit()">
    
    <form method="POST" action="https://prospector.ucsf.edu/prospector/cgi-bin/mssearch.cgi">
    <input type="hidden" name="search_name" value="msdigest"/>
    <input type="hidden" name="report_title" value="MS-Digest"/>
    <input type="hidden" name="version" value="6.8.3"/>
    
    <textarea name="user_protein_sequence">', SEQUENCE, '</textarea>

    <input type="hidden" name="max_hits" value="', MAXHITS, '"/>
    <input type="hidden" name="database" value="', DATABASE, '"/>
    <input type="hidden" name="output_type" value="', OUTPUT, '"/>
    <input type="hidden" name="results_to_file" value="', FILE, '"/>
    <input type="hidden" name="output_filename" value="', FILENAME, '"/>
    <input type="hidden" name="access_method" value="', ACCESS ,'"/>
    <input type="hidden" name="n_term_aa_limit" size="5" maxlength="5" value="', AALIMIT ,'"/>
    <input type="hidden" name="enzyme" value="',CLEAVAGE ,'"/>
    <input type="hidden" name="missed_cleavages" value="', MISSEDC, '" />
    <input type="hidden" name="end_terminus" value="', ENDTERM, '" />
    <input type="hidden" name="stripping_terminal" value="', STRIPTERM, '"/>
    <input type="hidden" name="start_strip" size="2" value="', STARTSTRIP, '"/>
    <input type="hidden" name="end_strip" size="2" value="', ENDSTRIP, '"/>
    
    <input type="hidden" name="max_digest_fragment_mass" size="2" value="', MINFRAGMASS, '"/>
    <input type="hidden" name="min_digest_fragment_mass" size="2" value="', MAXFRAGMASS, '"/>
    <input type="hidden" name="min_digest_fragment_length" size="2" value="', MINFRAGLEN, '"/>
    
    ', const_mods_html, '

    <textarea name="entry_data">', ENTRYDATA, '</textarea>

    ', var_mods_html, '

    </form>
    </body>
    </html>'
  )
  
  temp <- tempfile(fileext = ".html")
  writeLines(text = html, con = temp)
  
  if (TEST == FALSE){
    cat("Submitting HTML to Protein Prospector MS-Digest server.")
    loading_animation()
    
    browseURL(url = temp)
  }
  
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

msdigest_check_empty <- function(SEQUENCE, DATABASE){
  if (SEQUENCE == "" && DATABASE == ""){
    stop("Arguments `SEQUENCE` and `DATABASE` cannot both be empty.")
  }
}


msdigest_check_sequence <- function(SEQUENCE, DATABASE){
  if (DATABASE == "User Protein"){
    if (length(SEQUENCE) != 1){
      stop("Argument `SEQUENCE` should be one character string.'")
    } else if (is.na(SEQUENCE) || 
               !is.character(SEQUENCE) || 
               !grepl("^[a-zA-Z0-9.]+$", SEQUENCE)){
      stop("Argument `SEQUENCE` contains non permitted characters or is empty.")
    }
  }
}


msdigest_check_database <- function(DATABASE, ACCESS, ENTRYDATA){
  databases <- c("User Protein", "NCBInr.2013.6.17", "NCBInr.2013.6.17.random",
                 "NCBInr.refseq.2009.09.14", "NCBInr.refseq.2009.09.14.random",
                 "NCBInr.refseq.2009.09.14.random.sub",
                 "NCBInr.refseq.2009.09.14.sub", "nextprot_all",
                 "nextprot_all.random", "Pdefault.TAIR10_pep_20101214_updated",
                 "Pdefault.TAIR10_pep_20101214_updated.random",
                 "Refseq.2024.11.08.vertebrate_mammalian",
                 "Refseq.2024.11.08.vertebrate_mammalian.random",
                 "SwissProt.2016.9.6", "SwissProt.2016.9.6.random",
                 "SwissProt.2017.11.01", "SwissProt.2017.11.01.random",
                 "SwissProt.2020.09.02", "SwissProt.2020.09.02.random",
                 "SwissProt.2021.06.18", "SwissProt.2021.06.18.random",
                 "SwissProt.2025.02.26", "SwissProt.2025.02.26.random",
                 "SwissProt.Human.2024.02.02",
                 "SwissProt.Human.2024.02.02.random", "UniProtKB.2017.11.01",
                 "UniProtKB.2017.11.01.random", "UniProtKB.2020.09.02",
                 "UniProtKB.2020.09.02.random")
  
  if (DATABASE != "User Protein") {
    if (length(DATABASE) != 1){
      stop("Argument `DATABASE` should contains one database.")
    }else if (!(DATABASE %in% databases)){
      stop("Argument `DATABASE` contains unexpectet database.")
    } else if (!(ACCESS %in% c("Accession Number", "Index Number"))){
      stop("Argument `ACCESS` is not 'Accession Number' or 'Index Number'.")
    } else if (FALSE %in% grepl("^[0-9a-zA-Z]+$", ENTRYDATA)){
      stop("Argument `ENTRYDATA` contains non permitted characters or is empty.")
    }
  }
}


msdigest_check_maxhits <- function(MAXHITS){
  if (!is.numeric(MAXHITS)){
    warning("Argument `MAXHITS` should be an integer.")
  } else if (length(MAXHITS) > 1){
    warning("Argument `MAXHITS` should be an integer.")
  }
}


msdigest_check_output <- function(OUTPUT, FILE, FILENAME){
  if (length(OUTPUT) > 1){
    stop("Argument `OUTPUT` should be 'HTML', 'XML' or 'Tab delimited text'.")
  } else if (!(OUTPUT %in% c("HTML", "XML", "Tab delimited text"))){
    stop("Argument `OUTPUT` should be 'HTML', 'XML' or 'Tab delimited text'.")
  }else if (length(FILE) > 1) {
    warning("Argument `FILE` should be '0' for 'FALSE' or '1' for 'TRUE.'")
  } else if (!(is.numeric(FILE))){
    warning("Argument `FILE` should be numeric. Expected '0' for 'FALSE' or '1' for 'TRUE.'")
  } else if (!(FILE %in% c(0,1))){
    warning("Argument `FILE` should be '0' for 'FALSE' or '1' for 'TRUE.'")
  } else if (FILE == 1){
    if (length(FILENAME) != 1){
      warning("Argument `FILENAME` should be one character string.'")
    }else if (!is.character(FILENAME) || is.na(FILENAME) || !grepl("^[a-zA-Z0-9]+$", FILENAME)){
      warning("Argument `FILENAME` contains non permitted characters or is empty.")
    }
  }
}


msdigest_check_cleavage <- function(CLEAVAGE, MISSEDC){
  enzymes <- c("Trypsin", "TrypsinPro", "SlymotrypsinFYWKR", "Chymotrypsin", 
               "Chymotrypsin FYW", "ChymotrypsinFWYMEDLN", "V8 DE", "V8 E", 
               "Lys-C", "Lys-C-Pro", "Lys-N", "Arg-C", "Asp-N", "Asp-C", 
               "DE-N", "CNBr", "Pepsin(porcine gastric)", "Glu-C", "Tyr-C", 
               "Thermolysin", "Elastase", "a-Lytic Protease (aLP)", 
               "Full Protein", "Pro-C", "Pro-N", "Proteinase K", 
               "Rhizopuspepsin", "Cys-N", "KR-N", "Hydroxylamine", 
               "CNBr/Trypsin", "CNBr/V8 DE", "CNBr/V8 E", "CNBr/Lys-C", 
               "CNBr/Asp-N", "CNBr/Asp-C", "CNBr/Arg-C", "CNBr/Trypsin/V8 DE", 
               "CNBr/Trypsin/V8 E", "CNBr/Arg-C/V8 E", "CNBr/Lys-C/V8 E", 
               "Trypsin/Asp-N", "Trypsin/DE-N", "Trypsin/V8 DE", "Trypsin/V8 E", 
               "Trypsin/Glu-C", "Trypsin/Chymotrypsin", 
               "Trypsin/Pepsin(porcine gastric)", "Chymotrypsin/Arg-C", 
               "Lys-C/Trypsin", "Lys-C/V8 DE", "Lys-C/V8 E", "Lys-C/Asp-N", 
               "Lys-C/DE-N", "Lys-C/Glu-C", "V8 DE/Chymotrypsin", "Arg-C/V8 E", 
               "Glu-C/Asp-N", "Glu-C/Chymotrypsin", "DE-N/Cys-N", "Asp-N/Asp-C", 
               "Asp-C/Cys-N", "Lys-C/Lys-N", "Trypsin/KR-N", 
               "Trypsin/Hydroxylamine"
  )
  if (length(CLEAVAGE) != 1){
    stop("Argument `FILENAME` should contain one enzyme.")
  }else if (!(CLEAVAGE %in% enzymes)){
    stop("Argument `FILENAME` contains unexpected enzyme.")
  }
  
  if (length(MISSEDC) != 1){
    warning("Argument `MISSEDC` should be an single integer.")
  }else if (!is.numeric(MISSEDC)){
    warning("Argument `MISSEDC` should be an integer.")   
  }
}


msdigest_check_trim <- function(AALIMIT, ENDTERM, STRIPTERM, STARTSTRIP, ENDSTRIP){
  # TODO
}


msdigest_check_fragments <- function(MAXFRAGMASS, MINFRAGMASS, MINFRAGLEN){
  # TODO
}


msdigest_check_mods <- function(CONSTMODS, VARMODS){
  # TODO
}
