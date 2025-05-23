
ipdma_prediction_pipeline_model_pred_cont <- function(data, model_pred_fun, out_var_name, study_var_name) {
  by_study_predictions_df <- model_pred_fun(data)
  evaluate_performance <-  evaluate_performance_cont_obs_pred
  results_df <- meta_analyse_predictions(by_study_predictions_df, evaluate_performance, study_var_name)
  return(results_df)
}



#' Meta-anlyse predictions of a continuous outcome
#'
#' @param predictions A data frame containing individual level predictions in the 'pred' column, observed values in the 'actual' column and a column indicating study
#' @param study_var_name A string containing the name of the column indicating study
#'
#' @returns The results of the meta analysis
#' @export
#'
#' @examples
meta_analyse_predictions_cont <- function(predictions, study_var_name) {
  evaluate_performance <-  evaluate_performance_cont_obs_pred
  results <-  meta_analyse_predictions(predictions = predictions, evaluate_performance = evaluate_performance, study_var_name = study_var_name)

  return(results)
}

#' Pipeline for developing and validating a prediction model by study for an IPD Meta-analysis with training and test data.
#'
#' @param data data used to fit the model
#' @param evaluate_performance a function that evaluates the performance of the model for a single study
#' @param model_function a function that fits the model to be used
#'
#' @return
#' @export
#'
#' @examples
ipdma_prediction_pipeline <- function(
    data, 
    model_function, 
    predict_function = predict, 
    evaluate_performance, 
    out_var_name, 
    study_var_name = "studyid") {

  by_study_predictions_df <- get_predictions_by_study_IECV(data, model_function, predict_function, study_var_name, out_var_name) 
  
  
  results_df <- meta_analyse_predictions(by_study_predictions_df, evaluate_performance)

  return(results_df)
}



get_predictions_by_study_IECV <- function(data, model_function, predict_function, study_var_name, out_var_name) {
  study_col <- data[,study_var_name]
  studies <- study_col |> unique()
  
  predictions_list <- lapply(studies, get_IECV_prediction_for_a_study, 
                             data = data, model_function = model_function,  study_col = study_col, predict_function = predict_function, out_var_name = out_var_name)
  predictions_df <- do.call(rbind, predictions_list)
  rownames(predictions_df) <- NULL
  return(predictions_df)
}


get_IECV_prediction_for_a_study <- function(study, data, model_function, study_col, predict_function, out_var_name, study_var_name) {
  train_data <- data[!study_col==study,]
  test_data <- data[study_col==study,]
  model <- model_function(train_data)
  
  get_predictions(test_data, model, predict_function, out_var_name, study_var_name)
}




meta_analyse_predictions <- function(predictions, evaluate_performance, study_var_name) {
  by_study_performance <- get_performance_by_study(predictions, evaluate_performance, study_var_name)
  ma <- meta_analyse_performance_df(by_study_performance)
  
  return(c(ma, list(by_study = by_study_performance)))

}


#' Title
#'
#' @param by_study_predictions_df A dataframe containing individaul level predictions in the 'pred' column, observed values in the 'actual' column and a column indcating study
#' @param evaluate_performance The function used to evaluate performance
#' @param study_var_name The name of the study column
#'
#' @returns A data.frame containing the performance of the model for each study
#' @export
#'
#' @examples
get_performance_by_study <- function(by_study_predictions_df, evaluate_performance, study_var_name) {
  by_study_predictions_df <-  by_study_predictions_df |> as.data.frame()  # added to aviod bug that can occur if by_study_predictions_df is a tibble
  study_col <- by_study_predictions_df[,study_var_name]
  studies <- unique(study_col)
  
  results_list <- lapply(
    studies, 
    get_performance_for_a_study,
    by_study_predictions_df = by_study_predictions_df, 
    evaluate_performance = evaluate_performance,
    study_var_name = study_var_name)
  
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL

  return(results_df)
}

get_performance_for_a_study <- function(study, by_study_predictions_df, evaluate_performance, study_var_name) {
  predictions_df <- by_study_predictions_df[by_study_predictions_df[,study_var_name]==study, ]
  performance <- evaluate_performance(actual = predictions_df$actual, predicted = predictions_df$pred)
  performance[,study_var_name] <- study
  return(performance)
}



meta_analyse_performance_df <- function(by_study_performacne_df) {
  metrics <- unique(by_study_performacne_df$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne_df)
  
  results_df <- lapply(results_list, function(x) x[[1]]) |> dplyr::bind_rows()
  results_list <- lapply(results_list, function(x) x[-1])
  
  results_list <- results_list |> unlist(recursive = FALSE)
  names(results_list) <- results_df$metric
  
  return(list(results_df = results_df, results_list = results_list))
}





meta_analyse_performance <- function(metric, by_study_performacne) {
  results_df <- data.frame(metric,
             est = NA,
             se = NA,
             tau2 = NA,
             ci.lb = NA,
             ci.ub = NA,
             pi.lb = NA,
             pi.ub = NA)
  try({
    data <- by_study_performacne[by_study_performacne$metric == metric,]
    coefs <- data$coef
    ses <- data$se

    if(metric == "cstat"){
        results <- metamisc::valmeta(cstat = coefs, cstat.se = ses)
        results_df <- data.frame(metric,
                   est = results$est,
                   se = results$fit$se,
                   tau2 = results$fit$tau2,
                   ci.lb = results$ci.lb,
                   ci.ub = results$ci.ub,
                   pi.lb = results$pi.lb,
                   pi.ub = results$pi.ub)
    } else {
        results <- metafor::rma(yi = coefs, sei = ses, method = "REML", test = "knha")
        results_vector <- predict(results)
        results_df <- data.frame(metric,
                   est = results_vector$pred,
                   se = results_vector$se,
                   tau2 = results$tau2,
                   ci.lb = results_vector$ci.lb,
                   ci.ub = results_vector$ci.ub,
                   pi.lb = results_vector$pi.lb,
                   pi.ub = results_vector$pi.ub)
    }
  }, silent = FALSE)

  return(list(results_df, results))
}




