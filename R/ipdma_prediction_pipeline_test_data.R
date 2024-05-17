


#' Pipeline for developing and validating a prediction model by study for an IPD Meta-analysis with training and test data.
#'
#' @param data data used to fit the model
#' @param test_data data used to validate the model
#' @param evaluate_performance a function that evaluates the performance of the model for a single study
#' @param fit_model a function that fits the model to be used
#'
#' @return
#' @export
#'
#' @examples
ipdma_prediction_pipeline_test_data <- function(
    data, 
    model_function, 
    predict_function = predict, 
    evaluate_performance, 
    test_data, 
    out_var_name, 
    study_var_name = "studyid") {
  

    model <- model_function(data)
    by_study_predictions_df <- get_predictions(test_data, model, predict_function, out_var_name, study_var_name)

  
  results_df <- meta_analyse_predictions(by_study_predictions_df, evaluate_performance)
  
  return(results_df)
}


get_predictions <- function(test_data, model, predict_function, out_var_name, study_var_name) {
  actual <- test_data[,out_var_name]
  predictions <- predict_function(model, newdata = test_data)
  results_df <- data.frame(actual = actual, pred = predictions)
  results_df$study <- test_data[,study_var_name]
  return(results_df)
}

