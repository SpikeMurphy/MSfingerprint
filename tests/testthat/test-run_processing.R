test_that("`processing_check_file` works as intended.", {
  # TODO
  expect_no_warning(processing_check_file(FILE))
  expect_no_error(processing_check_file(FILE))
})


test_that("`processing_check_cleavage` works as intended.", {
  # TODO
  expect_no_warning(processing_check_cleavage(CLEAVAGE))
  expect_no_error(processing_check_cleavage(CLEAVAGE))
})


test_that("`processing_check_contaminants` works as intended.", {
  # TODO
  expect_no_warning(processing_check_contaminants(KERATIN, TAG, EXCLUDE))
  expect_no_error(processing_check_contaminants(KERATIN, TAG, EXCLUDE))
})


test_that("`processing_check_prerequisites` works as intended.", {
  # TODO
  expect_no_warning(processing_check_prerequisites(SNR, PEAKS))
  expect_no_error(processing_check_prerequisites(SNR, PEAKS))
})


test_that("`processing_check_output` works as intended.", {
  # TODO
  expect_no_warning(processing_check_plots(PLOTS))
  expect_no_error(processing_check_plots(PLOTS))
})


test_that("`processing_check_processing` works as intended.", {
  # TODO
  expect_no_warning(processing_check_processing(TRANSFORM, SMOOTH, BASELINE, CALIBRATE))
  expect_no_error(processing_check_processing(TRANSFORM, SMOOTH, BASELINE, CALIBRATE))
})
