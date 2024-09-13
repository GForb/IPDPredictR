test_that("get_predictions_by_study_test_data", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  cont_preds<- get_predictions(
    test_data = test_data, 
    model = cont_model, 
    predict_function = predict, 
    out_var_name = "y",
    study_var_name = "studyid")
  
  expect_equal(nrow(cont_preds), nrow(test_data))
  expect_equal(ncol(cont_preds), 3)
  expect_equal(length(cont_preds$actual), length(cont_preds$pred))
  
  
})

test_that("get_performance_by_study", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  cont_preds<- get_predictions(
    test_data = test_data, 
    model = cont_model, 
    predict_function = predict, 
    out_var_name = "y",
    study_var_name = "studyid")
  
  performance <- get_performance_by_study(cont_preds, evaluate_performance_cont_obs_pred, study_var_name = "studyid")
  
  expect_equal(nrow(cont_preds), nrow(test_data))
  expect_equal(ncol(cont_preds), 3)
  
})

test_that("meta_analyse_predictions_cont", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  cont_preds<- get_predictions(
    test_data = test_data, 
    model = cont_model, 
    predict_function = predict, 
    out_var_name = "y",
    study_var_name = "studyid")
  
results <- meta_analyse_predictions_cont(predictions = cont_preds, study_var_name = "studyid")

expect_equal(nrow(results$results_df), 4)
expect_equal(length(results$results_list), 4)
})





test_that("by_study_predictions", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  
  by_study_predictions_df <- get_predictions(test_data, cont_model, predict, "y", "studyid")
  expect_equal(sum(!is.na(by_study_predictions_df$actual)), nrow(test_data))
  expect_equal(sum(!is.na(by_study_predictions_df$pred)), nrow(test_data))
  expect_equal(sum(!is.na(by_study_predictions_df$study)), nrow(test_data))
  
  
})

test_that("meta_analyse_predictions", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  
  by_study_predictions_df <- get_predictions(test_data, cont_model, predict, "y", "studyid")
  analysed_predictions <- meta_analyse_predictions(by_study_predictions_df, evaluate_performance_cont_obs_pred, study_var_name = "studyid")

  
  expect_equal(analysed_predictions$results_df |> ncol(), 8)
  expect_equal(analysed_predictions$results_df |> nrow(), 4)
  expect_equal(length(analysed_predictions$results_list), 4)

  
})

test_that("ipdma_prediction_pipeline_test_data", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()

  
  
  cont_results <- ipdma_prediction_pipeline_test_data(
    data = train_data, 
    model_function = test_model_cont,
    evaluate_performance = evaluate_performance_cont_obs_pred,
    out_var_name = "y",
    study_var = "studyid",
    test_data = test_data)

  expect_equal(cont_results$results_df |> ncol(), 8)
  expect_equal(cont_results$results_df |> nrow(), 4)
  expect_equal(length(cont_results$results_list), 4)
  
  
})






