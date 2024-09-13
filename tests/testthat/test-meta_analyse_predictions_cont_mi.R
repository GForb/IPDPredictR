test_that("meta_analyse_predictions_cont_mi", {
  train_data <- test_gen_cont_data()
  test_data <- test_gen_cont_data()
  
  cont_model <- test_model_cont(train_data)
  cont_preds<- get_predictions(
    test_data = test_data, 
    model = cont_model, 
    predict_function = predict, 
    out_var_name = "y",
    study_var_name = "studyid") 
  cont_preds$new_study = 1:5
  cont_preds$imp_no = 1:5 |> rep(5) |> sort()
  
  results <- meta_analyse_predictions_cont_mi(predictions = cont_preds, study_var_name = "new_study", imp_indicator_name = "imp_no")

  expect_equal(nrow(results$results_df), 5)
  expect_equal(length(results$results_list), 5)
})