# =================================================== #
# ===== MAIN FUNCTION =============================== #
# =================================================== #

test_that("`search_mascot` works as intended.", {
  # TODO
  expect_no_warning(msf_mascot(PEAKS = c(1, 2, 3), USERNAME = "Firstname Middlename Lastname", USEREMAIL = "mail@example.com", TEST = TRUE))
  expect_no_error(msf_mascot(PEAKS = c(1, 2, 3), USERNAME = "Firstname Middlename Lastname", USEREMAIL = "mail@example.com", TEST = TRUE))
})


# =================================================== #
# ===== HELPER FUNCTIONS ============================ #
# =================================================== #




# =================================================== #
# ===== ENTRY FUNCTIONS ============================= #
# =================================================== #




# =================================================== #
# ===== TESTING FUNCTIONS =========================== #
# =================================================== #

test_that("`mascot_check_peaks` works as intended.", {
  expect_no_warning(mascot_check_peaks(PEAKS = c(1,2,3)))
  expect_no_error(mascot_check_peaks(PEAKS = c(1,2,3)))
  
  expect_error(mascot_check_peaks(PEAKS = c("1","2","3")))
  expect_error(mascot_check_peaks(PEAKS = c(1)))
  expect_error(mascot_check_peaks(PEAKS = 1))
  expect_error(mascot_check_peaks(PEAKS = "string"))
  expect_error(mascot_check_peaks(PEAKS = c(1,2,"string")))
  expect_error(mascot_check_peaks(PEAKS = c(NA, NA, NA)))
})


test_that("`mascot_check_auth` works as intended.", {
  expect_no_warning(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail.test"))
  expect_no_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail.test"))
  
  expect_error(mascot_check_auth(USERNAME = c(), USEREMAIL = "example@mail.test"))
  expect_error(mascot_check_auth(USERNAME = NA, USEREMAIL = "example@mail.test"))
  
  
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = ""))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = 1))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = NA))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = c("example@mail.test", "example@mail.test")))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "@mail.test"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@.test"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail."))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example?@mail.test"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail?.test"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail.test?"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "example@mail.test1"))
  expect_error(mascot_check_auth(USERNAME = "USERNAME", USEREMAIL = "examplemail.test"))
})


test_that("`mascot_check_database` works as intended.", {
  expect_no_warning(mascot_check_database(DATABASE = "SwissProt"))
  expect_no_error(mascot_check_database(DATABASE = "SwissProt" ))
  
  expect_error(mascot_check_database(DATABASE = ""))
  expect_error(mascot_check_database(DATABASE = c()))
  expect_error(mascot_check_database(DATABASE = c("SwissProt", "Human_EST") ))
  expect_error(mascot_check_database(DATABASE = 1 ))
  expect_error(mascot_check_database(DATABASE = NA ))
})


test_that("`mascot_check_digest` works as intended.", {
  expect_no_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = 1))
  expect_no_error(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = 1))
  
  expect_error(mascot_check_digest(CLEAVAGE = "", MISSEDC = 1))  
  expect_error(mascot_check_digest(CLEAVAGE = c("Trypsin", "Trypsin/P"), MISSEDC = 1))  
  expect_error(mascot_check_digest(CLEAVAGE = "string", MISSEDC = 1))  
  expect_error(mascot_check_digest(CLEAVAGE = 0, MISSEDC = 1))  
  expect_error(mascot_check_digest(CLEAVAGE = NA, MISSEDC = 1))
  
  expect_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = 12))  
  expect_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = -5))  
  expect_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = NA))
  expect_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = "string"))
  expect_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = c(1, 2)))
  expect_warning(mascot_check_digest(CLEAVAGE = "Trypsin", MISSEDC = c(1, "string")))
})


test_that("`mascot_check_mods` works as intended.", {
  # TODO
  expect_no_warning(mascot_check_mods(CONSTMODS, VARMODS))
  expect_no_error(mascot_check_mods(CONSTMODS, VARMODS))
})


test_that("`mascot_check_tolerance` works as intended.", {
  expect_no_warning(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = "Da"))
  expect_no_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = "Da"))
  
  expect_error(mascot_check_tolerance(MASS = "", CHARGE = "1+", TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "string", CHARGE = "1+", TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = NA, CHARGE ="1+", TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = 5, CHARGE = "1+", TOL = 0.3, TOLU = "Da"))
  
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "", TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "string", TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "2+", TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = 5, TOL = 0.3, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = NA, TOL = 0.3, TOLU = "Da"))
  
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = -5, TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = "string", TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = c(2, 3, 4), TOLU = "Da"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = NA, TOLU = "Da"))
  
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = "string"))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = ""))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = 5))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = NA))
  expect_error(mascot_check_tolerance(MASS = "Monoisotopic", CHARGE = "1+", TOL = 0.3, TOLU = c("Da", "ppm")))
})


test_that("`mascot_check_display` works as intended.", {
  # TODO
  expect_no_warning(mascot_check_display(REPORT))
  expect_no_error(mascot_check_display(REPORT))
})
