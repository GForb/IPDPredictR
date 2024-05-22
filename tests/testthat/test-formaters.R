
# test_that("results_flextable", {
#   train_data <- test_gen_cont_data()
#   test_data <- test_gen_cont_data()
#   
#   cont_model <- test_model_cont(train_data)
#   cont_results <- ipdma_prediction_pipeline_test_data(
#     data = train_data, 
#     model_function = test_model_cont,
#     evaluate_performance = evaluate_performance_cont_obs_pred,
#     out_var_name = "y",
#     study_var = "studyid",
#     test_data = test_data) 
#   
#     flex_table <- cont_results$results_df |>
#       results_flextable()
# })