  # test_that("IPDMA_predict_multiple_models", {
  #   train_data <- test_gen_cont_data()
  #   test_data <- test_gen_cont_data()
  #   
  #   model_function_list <- list(model1 = test_model_cont, test_model_cont, test_model_cont)
  #   
  #   cont_results <- IPDMA_predict_multiple_models(
  #     model_function_list = model_function_list,
  #     data = train_data, 
  #     evaluate_performance = evaluate_performance_cont_obs_pred,
  #     test_data = test_data,
  #     out_var_name = "y",
  #     study_var = "studyid",
  #     InternalExternalCV = FALSE)
  #   expect_equal(nrow(cont_results), 9)
  #   
  #   model_labels = c("Model 1", "Model 2", "Model 3")
  #   cont_results <- IPDMA_predict_multiple_models(
  #     model_function_list = model_function_list,
  #     model_labels = model_labels,
  #     data = train_data, 
  #     evaluate_performance = evaluate_performance_cont_obs_pred,
  #     test_data = test_data,
  #     out_var_name = "y",
  #     study_var = "studyid",
  #     InternalExternalCV = FALSE)
  #   expect_equal(cont_results$model, c(rep("Model 1", 3), rep("Model 2", 3), rep("Model 3", 3)))
  #   
  #  
  # })
