test_that("evaluate_performance_continuous", {
  perf = evaluate_performance_continuous(test_data = test_gen_cont_data(),
                                         model = test_model_cont( data = test_gen_cont_data()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)

  int_data = test_gen_cont_data()
  int_data$int_est = TRUE
  test_data = test_gen_cont_data()
  test_data$int_est = FALSE
  test_data <- rbind(int_data, test_data)
  perf = evaluate_performance_continuous(test_data = test_data,
                                         model = test_model_cont( data = test_gen_cont_data()),
                                         new_studies = TRUE)
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)

  perf = evaluate_performance_continuous_new_studies(test_data = test_data,
                                         model = test_model_cont( data = test_gen_cont_data()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)


})


test_that("evaluate_performance_binary", {
  perf = evaluate_performance_binary(test_data = test_gen_bin_data(),
                                         model = test_model_bin( data = test_gen_bin_data()))
  expect_equal(ncol(perf), 3)
  expect_equal(nrow(perf), 3)
})

