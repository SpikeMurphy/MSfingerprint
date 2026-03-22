test_that("`check_lists` works as intended.", {
  expect_no_warning(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  expect_no_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = c(), MSFIT_ARGS = list()))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = "", MSFIT_ARGS = list()))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = 5, MSFIT_ARGS = list()))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = "string", MSFIT_ARGS = list()))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = "5", MSFIT_ARGS = list()))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = NA, MSFIT_ARGS = list()))
  
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = c()))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = ""))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = 5))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = "string"))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = "5"))
  expect_error(check_lists(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = NA))
})


test_that("`check_peaks` works as intended.", {
  expect_no_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  expect_no_error(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  
  expect_warning(check_peaks(PROCESSING_ARGS = list(FILE = NA), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  expect_warning(check_peaks(PROCESSING_ARGS = list(FILE = c(1, 2, 3)), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  expect_warning(check_peaks(PROCESSING_ARGS = list(FILE = "string"), MASCOT_ARGS = list(), MSFIT_ARGS = list()))
  expect_warning(check_peaks(PROCESSING_ARGS = list(FILE = 5), MASCOT_ARGS = list(), MSFIT_ARGS = list()))

  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(PEAKS = NA), MSFIT_ARGS = list()))
  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(PEAKS = c(1, 2, 3)), MSFIT_ARGS = list()))
  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(PEAKS = "sting"), MSFIT_ARGS = list()))
  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(PEAKS = 5), MSFIT_ARGS = list()))

  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list(PEAKS = NA)))
  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list(PEAKS = c(1, 2, 3))))
  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list(PEAKS = "string")))
  expect_warning(check_peaks(PROCESSING_ARGS = list(), MASCOT_ARGS = list(), MSFIT_ARGS = list(PEAKS = 5)))
})
