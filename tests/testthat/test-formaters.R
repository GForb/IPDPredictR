
test_that("results_flextable", {
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
    InternalExternalCV = FALSE) |> 
    results_flextable()
})