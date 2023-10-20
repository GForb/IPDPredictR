



#' Pipeline for developing and validating a prediction model by study for an IPD Meta-analysis with training and test data.
#'
#' @param train_data data used to fit the model
#' @param test_data data used to validate the model
#' @param evaluate_performance a function that evaluates the performance of the model for a single study
#' @param fit_model a function that fits the model to be used
#'
#' @return
#' @export
#'
#' @examples
ipdma_prediction_pipeline <- function(data, model_function, InternalExternalCV = TRUE, predict_function = predict, evaluate_performance, test_data = NULL, out_var_name, study_var_name = "studyid") {
  check_IECV_test_data(InternalExternalCV, test_data)
  
  if(InternalExternalCV){
    predictions_df <- get_predictions_by_study_IECV(data, model_function, predict_function, study_var_name, out_var_name) 
  } else {
    model <- model_function(data)
    by_study_predictions_df <- get_predictions(test_data, model, predict_function, out_var_name, study_var_name)
  }
  
  results_df <- meta_analyse_predictions(by_study_predictions_df, evaluate_performance)

  return(results_df)
}

check_IECV_test_data <- function(InternalExternalCV, test_data) {
  if(InternalExternalCV & !is.null(test_data)){
    warning("Test data will not be used. 
            Performance will be estiamted using Internal-External Cross validaiton. 
            To use test_data to evaluate performance set InternalExternalCV=False")
  }
  if (!InternalExternalCV  & is.null(test_data)) {
    stop("If InternalExternalCV is set to False, test_data must be provided")
  }
}

meta_analyse_predictions <- function(predictions, evaluate_performance) {
  by_study_performance <- get_performance_by_study(predictions, evaluate_performance)
  results_df <- meta_analyse_performance_df(by_study_performance)
  return(results_df)
}

get_predictions_by_study_IECV <- function(data, model_function, predict_function, study_var_name, out_var_name) {
  study_col <- data[,study_var_name]
  studies <- study_col |> unique()
  
  predictions_list <- lapply(studies, get_IECV_prediction_for_a_study, 
                             data = data, model_function = model_function,  study_col = study_col, predict_function = predict_function, out_var_name = out_var_name)
  predictions_df <- do.call(rbind, results_list)
  rownames(predictions_df) <- NULL
  return(predictions_df)
}


get_IECV_prediction_for_a_study <- function(study, data, model_function, study_col, predict_function, out_var_name, study_var_name) {
  train_data <- data[!study_col==study,]
  test_data <- data[study_col==study,]
  model <- model_function(train_data)
  
  get_predictions(test_data, model, predict_function, out_var_name, study_var_name)
  return(results_df)
}

get_predictions <- function(test_data, model, predict_function, out_var_name, study_var_name) {
  actual <- test_data[,out_var_name]
  predictions <- predict_function(model, newdata = test_data)
  results_df <- data.frame(actual = actual, pred = predictions)
  results_df$study <- test_data[,study_var_name]
  return(results_df)
}

get_performance_by_study <- function(by_study_predictions_df, evaluate_performance) {

  study_col <- by_study_predictions_df$study
  studies <- unique(study_col)
  
  results_list <- lapply(studies, get_performance_for_a_study,
                         by_study_predictions_df = by_study_predictions_df, evaluate_performance = evaluate_performance)
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL

  return(results_df)
}

get_performance_for_a_study <- function(study, by_study_predictions_df, evaluate_performance) {
  predictions_df <- by_study_predictions_df[by_study_predictions_df$study==study, ]
  performance <- evaluate_performance(actual = predictions_df$actual, predicted = predictions_df$pred)

  return(performance)
}



meta_analyse_performance_df <- function(by_study_performacne_df) {
  metrics <- unique(by_study_performacne_df$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne_df)
  results_df <- dplyr::bind_rows(results_list)
  return(results_df)
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

  return(results_df)
}




