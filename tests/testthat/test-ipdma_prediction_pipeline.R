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
  
  performance <- get_performance_by_study(cont_preds, evaluate_performance_cont_obs_pred)
  
  expect_equal(nrow(cont_preds), nrow(test_data))
  expect_equal(ncol(cont_preds), 3)
  
})

get_performance_by_study

test_that("ipdma_prediction_pipeline", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  cont_results <- ipdma_prediction_pipeline(
    data = train_data, 
    model_function = test_model_cont,
    evaluate_performance = evaluate_performance_cont_obs_pred,
    test_data = test_data,
    out_var_name = "y",
    study_var = "studyid",
    InternalExternalCV = FALSE)
  expect_equal(nrow(cont_results), 3)
  
  
})






