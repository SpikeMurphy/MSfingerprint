test_that("`search_msfit` works as intended.", {
  # TODO
  expect_no_warning(search_msfit(PEAKS = c(1, 2, 3), TEST = TRUE))
  expect_no_error(search_msfit(PEAKS = c(1, 2, 3), TEST = TRUE))
})




test_that("`msfit_check_peaks` works as intended.", {
  expect_no_warning(msfit_check_peaks(PEAKS = c(1,2,3)))
  expect_no_error(msfit_check_peaks(PEAKS = c(1,2,3)))
  
  expect_error(msfit_check_peaks(PEAKS = c("1","2","3")))
  expect_error(msfit_check_peaks(PEAKS = c(1)))
  expect_error(msfit_check_peaks(PEAKS = 1))
  expect_error(msfit_check_peaks(PEAKS = "string"))
  expect_error(msfit_check_peaks(PEAKS = c(1,2,"string")))
  expect_error(msfit_check_peaks(PEAKS = c(NA, NA, NA)))
})


test_that("`msfit_check_sequence` works as intended.", {
  expect_no_warning(msfit_check_sequence(SEQUENCE = ""))
  expect_no_error(msfit_check_sequence(SEQUENCE = ""))
  
  expect_no_warning(msfit_check_sequence(SEQUENCE = "MYSYSAAMEEKK"))
  expect_no_error(msfit_check_sequence(SEQUENCE = "MYSYSAAMEEKK"))
  
  expect_warning(msfit_check_sequence(SEQUENCE = c("MYSYSAAMEEKK", "MYSYSAAMEEKK")))
  expect_warning(msfit_check_sequence(SEQUENCE = "mysysaameekk"))
  expect_warning(msfit_check_sequence(SEQUENCE = "MYSYSAAMEEKKB"))
  expect_warning(msfit_check_sequence(SEQUENCE = "5"))
  expect_warning(msfit_check_sequence(SEQUENCE = 5))
  expect_warning(msfit_check_sequence(SEQUENCE = NA))
})


test_that("`msfit_check_database` works as intended.", {
  expect_no_warning(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = "All"))
  expect_no_error(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = "All"))
  
  expect_error(msfit_check_database(DATABASE = c("NCBInr.2013.6.17", "SwissProt.2025.02.26"), SPECIES = "All"))
  expect_error(msfit_check_database(DATABASE = "string", SPECIES = "All"))
  expect_error(msfit_check_database(DATABASE = NA, SPECIES = "All"))
  expect_error(msfit_check_database(DATABASE = 5, SPECIES = "All"))
  expect_error(msfit_check_database(DATABASE = "5", SPECIES = "All"))
  
  expect_error(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = ""))
  expect_error(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = c("All", "HOMO SAPIENS")))
  expect_error(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = "5"))
  expect_error(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = 5))
  expect_error(msfit_check_database(DATABASE = "SwissProt.2025.02.26", SPECIES = NA))
})


test_that("`msfit_check_frame` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_frame(FRAME, AALIMIT))
  expect_no_error(msfit_check_frame(FRAME, AALIMIT))
})


test_that("`msfit_check_output` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_output(OUTPUT, SORT, HOMOLOGUES))
  expect_no_error(msfit_check_output(OUTPUT, SORT, HOMOLOGUES))
})


test_that("`msfit_check_prerequisites` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_prerequisites(LOWMASS, HIGHMASS, LOWPI, HIGHPI, RANGEPI))
  expect_no_error(msfit_check_prerequisites(LOWMASS, HIGHMASS, LOWPI, HIGHPI, RANGEPI))
})


test_that("`msfit_check_digest` works as intended.", {
  expect_no_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = 1))
  expect_no_error(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = 1))
  
  expect_error(msfit_check_digest(CLEAVAGE = "", MISSEDC = 1))  
  expect_error(msfit_check_digest(CLEAVAGE = c("Trypsin", "Trypsin/P"), MISSEDC = 1))  
  expect_error(msfit_check_digest(CLEAVAGE = "string", MISSEDC = 1))  
  expect_error(msfit_check_digest(CLEAVAGE = 0, MISSEDC = 1))  
  expect_error(msfit_check_digest(CLEAVAGE = NA, MISSEDC = 1))
  
  expect_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = 12))  
  expect_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = -5))  
  expect_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = NA))
  expect_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = "string"))
  expect_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = c(1, 2)))
  expect_warning(msfit_check_digest(CLEAVAGE = "Trypsin", MISSEDC = c(1, "string")))
})


test_that("`msfit_check_mods` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_mods(CONSTMODS, VARMODS))
  expect_no_error(msfit_check_mods(CONSTMODS, VARMODS))
})


test_that("`msfit_check_tolerance` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_tolerance(MASS, TOL, TOLU))
  expect_no_error(msfit_check_tolerance(MASS, TOL, TOLU))
})


test_that("`msfit_check_matches` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_matches(MINMATCH, MINPMATCH, MAXMODS, MAXHITS))
  expect_no_error(msfit_check_matches(MINMATCH, MINPMATCH, MAXMODS, MAXHITS))
})


test_that("`msfit_check_display` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_display(GRAPH, MOWSE, MOWSEP))
  expect_no_error(msfit_check_display(GRAPH, MOWSE, MOWSEP))
})


test_that("`msfit_check_instrument` works as intended.", {
  # TODO
  expect_no_warning(msfit_check_instrument(INST, FORMAT))
  expect_no_error(msfit_check_instrument(INST, FORMAT))
})
