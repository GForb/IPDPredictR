
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
ipdma_prediction_pipeline <- function(data, model_function, predict_function = NULL, evaluate_performance, InternalExternalCV = TRUE, test_data = NULL, study_var = "studyid") {
  if(InternalExternalCV & !is.null(test_data)){
    warning("Test data will not be used. Performance will be estiamted using Internal-External Cross validaiton. To use test_data to evaluate performance set InternalExternalCV=False")
  }
  if (!InternalExternalCV  & is.null(test_data)) {
    stop("If InternalExternalCV is set to False, test_data must be provided")
  }
  
  if(InternalExternalCV){
    results_df <- ipdma_prediction_pipeline_IECV
  } else {
    results_df <- ipdma_prediction_pipeline_test_data
  }
  
  return(results_df)
}

ipdma_prediction_pipeline_test_data<- function(
    data, 
    model_function, 
    predict_function = NULL, 
    evaluate_performance, 
    test_data, 
    study_var = "studyid") {
  
  model <- model_function(data)
  by_study_performacne <- get_performance_by_study(test_data, model, evaluate_performance, study_var = study_var)
  results_df <- meta_analyse_performance_df(by_study_performacne)
  return(results_df)
}

ipdma_prediction_pipeline_IECV <- function(data, model_function,  evaluate_performance, study_var = "studyid") {
  by_study_performacne <- get_performance_by_study_IECV(data, model_function, evaluate_performance, study_var)
  results_df <- meta_analyse_performance_df(by_study_performacne)
  return(results_df)
}

meta_analyse_performance_df <- function(by_study_performacne_df) {
  metrics <- unique(by_study_performacne_df$metric)
  results_list <- lapply(metrics, meta_analyse_performance, by_study_performacne = by_study_performacne_df)
  results_df <- dplyr::bind_rows(results_list)
  return(results_df)
}

get_performance_by_study_IECV <- function(data, model_function, evaluate_performance, study_var) {
  study_col <- data[,study_var]
  studies <- study_col |> unique()
  
  results_list <- lapply(studies, IECV_for_a_study, 
                         data = data, model_function = model_function,
                         evaluate_performance = evaluate_performance,
                         study_col = study_col)
  results <- do.call(rbind, results_list)
  rownames(results) <- NULL
  return(results)
}

IECV_for_a_study <- function(study, data, model_function, evaluate_performance, study_col) {
  train_data <- data[!study_col==study,]
  test_data <- data[study_col==study,]
  
  model <- model_function(train_data)
  results <- evaluate_performance(test_data, model)
  results$studyid <- study
  return(results)
}

get_performance_by_study <- function(test_data, model, evaluate_performance, study_var) {
  if (!(study_var %in% colnames(test_data))) {
    stop("test data must include varaible called study_var")
  }
  study_col_test <- test_data[,study_var]
  studies <- unique(study_col_test)
  
  results_list <- lapply(
    studies, 
    performance_by_study, 
    model = model, test_data = test_data, study_col_test = study_col_test, evaluate_performance = evaluate_performance)
  results <- do.call(rbind, results_list)
  rownames(results) <- NULL
  return(results)
}

performance_by_study <- function(study, model, test_data, study_col_test, evaluate_performance) {
  study_test_data <- test_data[study_col_test == study,]
  results <- evaluate_performance(study_test_data, model)
  results$studyid <- study
  return(results)
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




