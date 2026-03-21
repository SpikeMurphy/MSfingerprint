test_that("`search_msdigest` works as intended.", {
  expect_no_warning(search_msdigest(SEQUENCE = "M", TEST = TRUE))
  expect_no_error(search_msdigest(SEQUENCE = "M", TEST = TRUE))
})




test_that("`msdigest_check_empty` works as intended.", {
  expect_no_warning(msdigest_check_empty(SEQUENCE = "MMM", DATABASE = ""))
  expect_no_error(msdigest_check_empty(SEQUENCE = "MMM", DATABASE = ""))
  expect_no_warning(msdigest_check_empty(SEQUENCE = "", DATABASE = "SwissProt.2025.02.26"))
  expect_no_error(msdigest_check_empty(SEQUENCE = "", DATABASE = "SwissProt.2025.02.26"))
  
  expect_error(msdigest_check_empty(SEQUENCE = "", DATABASE = ""))
})

test_that("`msdigest_check_sequence` works as intended.", {
  expect_no_warning(msdigest_check_sequence(SEQUENCE = "MMM", DATABASE = "User Protein"))
  expect_no_error(msdigest_check_sequence(SEQUENCE = "MMM", DATABASE = "User Protein"))
  
  expect_error(msdigest_check_sequence(SEQUENCE = "MMM\\MMM", DATABASE = "User Protein"))
  expect_error(msdigest_check_sequence(SEQUENCE = "", DATABASE = "User Protein"))
})

test_that("`msdigest_check_database` works as intended.", {
  expect_no_warning(msdigest_check_database(DATABASE = "User Protein", ACCESS = "", ENTRYDATA = ""))
  expect_no_error(msdigest_check_database(DATABASE = "User Protein", ACCESS = "", ENTRYDATA = ""))
  expect_no_warning(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "Accession Number", ENTRYDATA = "aaAA0099"))
  expect_no_error(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "Accession Number", ENTRYDATA = "aaAA0099"))
  expect_no_warning(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  expect_no_error(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  
  expect_error(msdigest_check_database(DATABASE = "SwissBASE", ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  expect_error(msdigest_check_database(DATABASE = c("SwissProt.2025.02.26", "SwissProt.2021.06.18"), ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  expect_error(msdigest_check_database(DATABASE = 1, ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  expect_error(msdigest_check_database(DATABASE = c(1,2), ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  expect_error(msdigest_check_database(DATABASE = c("string", "string"), ACCESS = "Index Number", ENTRYDATA = "aaAA0099"))
  
  expect_error(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "", ENTRYDATA = "aaAA0099"))
  expect_error(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "Accession Number", ENTRYDATA = "aa.AA.00.99"))
  expect_error(msdigest_check_database(DATABASE = "SwissProt.2025.02.26", ACCESS = "Accession Number", ENTRYDATA = "aa\\AA\\00\\99"))
})


test_that("`msdigest_check_maxhits` works as intended.", {
  expect_no_warning(msdigest_check_maxhits(MAXHITS = 1))
  expect_no_error(msdigest_check_maxhits(MAXHITS = 1))
  
  expect_warning(msdigest_check_maxhits(MAXHITS = "1"))
  expect_warning(msdigest_check_maxhits(MAXHITS = c(1,2,3)))
  expect_warning(msdigest_check_maxhits(MAXHITS = "string"))
  expect_warning(msdigest_check_maxhits(MAXHITS = "."))
  expect_warning(msdigest_check_maxhits(MAXHITS = ""))
  expect_warning(msdigest_check_maxhits(MAXHITS = NA))
})


test_that("`msdigest_check_output` works as intended.", {
  expect_no_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 0, FILENAME = "msdigest"))
  expect_no_error(msdigest_check_output(OUTPUT = "HTML", FILE = 0, FILENAME = "msdigest"))
  expect_no_warning(msdigest_check_output(OUTPUT = "XML", FILE = 0, FILENAME = "msdigest"))
  expect_no_error(msdigest_check_output(OUTPUT = "XML", FILE = 0, FILENAME = "msdigest"))
  expect_no_warning(msdigest_check_output(OUTPUT = "Tab delimited text", FILE = 0, FILENAME = "msdigest"))
  expect_no_error(msdigest_check_output(OUTPUT = "Tab delimited text", FILE = 0, FILENAME = "msdigest"))
  
  expect_no_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = "msdigest"))
  expect_no_error(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = "msdigest"))
  expect_no_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = "msdigest1"))
  expect_no_error(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = "msdigest1"))
  
  expect_error(msdigest_check_output(OUTPUT = "", FILE = 0, FILENAME = "msdigest"))
  expect_error(msdigest_check_output(OUTPUT = "string", FILE = 0, FILENAME = "msdigest"))
  expect_error(msdigest_check_output(OUTPUT = "111", FILE = 0, FILENAME = "msdigest"))
  expect_error(msdigest_check_output(OUTPUT = 1, FILE = 0, FILENAME = "msdigest"))
  expect_error(msdigest_check_output(OUTPUT = c(1,2,3), FILE = 0, FILENAME = "msdigest"))
  
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = "1", FILENAME = "msdigest"))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = c(1,2,3), FILENAME = "msdigest"))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = "string", FILENAME = "msdigest"))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = "_", FILENAME = "msdigest"))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 5, FILENAME = "msdigest"))
  
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = ""))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = "msdigest.html"))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = 12))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = c("string", "string")))
  expect_warning(msdigest_check_output(OUTPUT = "HTML", FILE = 1, FILENAME = c(1,2)))
})


test_that("`msdigest_check_cleavage` works as intended.", {
  expect_no_warning(msdigest_check_cleavage(CLEAVAGE = "Trypsin", MISSEDC = 1))
  expect_no_error(msdigest_check_cleavage(CLEAVAGE = "Trypsin", MISSEDC = 1))
  
  expect_error(msdigest_check_cleavage(CLEAVAGE = "string", MISSEDC = 1))
  expect_error(msdigest_check_cleavage(CLEAVAGE = "1", MISSEDC = 1))
  expect_error(msdigest_check_cleavage(CLEAVAGE = 1, MISSEDC = 1))
  expect_error(msdigest_check_cleavage(CLEAVAGE = 0, MISSEDC = 1))
  expect_error(msdigest_check_cleavage(CLEAVAGE = c("Trypsin", "TrypsinPro"), MISSEDC = 1))
  
  expect_warning(msdigest_check_cleavage(CLEAVAGE = "Trypsin", MISSEDC = ""))
  expect_warning(msdigest_check_cleavage(CLEAVAGE = "Trypsin", MISSEDC = "string"))
  expect_warning(msdigest_check_cleavage(CLEAVAGE = "Trypsin", MISSEDC = c(1,2)))
  expect_warning(msdigest_check_cleavage(CLEAVAGE = "Trypsin", MISSEDC = "1"))
})
