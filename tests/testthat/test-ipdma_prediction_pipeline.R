
test_that("ipdma_prediction_pipeline_test_data", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()

  cont_model <- test_model_cont(train_data)
  cont_results <- ipdma_prediction_pipeline_test_data(
    data = train_data, 
    model_function = test_model_cont,
    evaluate_performance = evaluate_performance_continuous,
    test_data = test_data,
    study_var = "studyid")
  expect_equal(nrow(cont_results), 3)

  train_data_bin <- test_gen_bin_data()
  test_data_bin <- test_gen_bin_data()
  binary_results <- ipdma_prediction_pipeline_test_data(
    data = train_data_bin, 
    model_function = test_model_bin,
    evaluate_performance = evaluate_performance_continuous,
    test_data = test_data_bin,
    study_var = "studyid")
  expect_equal(nrow(cont_results), 3)

})

# To do: work on how to make predictions in 'new studies' automaticly
# 
# test_that("ipdma_prediction_pipeline_IECV", {
#   train_data <- test_gen_cont_data()
#   test_data <- test_gen_cont_data()
#   
#   cont_model <- test_model_cont(train_data)
#   cont_results <- ipdma_prediction_pipeline_IECV(
#     data = train_data, 
#     model_function = test_model_cont,
#     evaluate_performance = evaluate_performance_continuous_new_studies,
#     study_var = "studyid")
#   expect_equal(nrow(cont_results), 3)
# })

