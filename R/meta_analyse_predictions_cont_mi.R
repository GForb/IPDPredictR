meta_analyse_predictions_cont_mi <- function(predictions, study_var_name, imp_indicator_name) {
  # 1. Estimate model performance in each imp_rep for each study
  predictions <- predictions |> as.data.frame()
  
  predictions$study_imp = paste0(predictions[,study_var_name], "_imp", predictions[,imp_indicator_name])

  by_study_imp <- get_performance_by_study(predictions, evaluate_performance_cont_obs_pred, "study_imp")
  by_study_imp$study <- sub("_imp.*$", "", by_study_imp$study_imp)
  by_study_imp$imp_no    <- sub("^.*_imp", "", by_study_imp$study_imp)
  # 2. Within each study, pool model estimates to get. y study performance
  by_study_performance <- pool_performance_estimates(by_study_imp, study_var_name = "study")
  
  # Meta-analyse by study performance
  ma <- meta_analyse_performance_df(by_study_performance)
  results <- c(ma, list(by_study = by_study_performance))
  return(results)
  
  # Pool
  # Meta-analyse pooled performance.
}

pool_performance_estimates <- function(performance_estimates, study_var_name) {
  studies <- unique(performance_estimates[,study_var_name])
  pooled_results <- lapply(
    studies, 
    pool_performance_estimates_for_study, 
    performance_estimates = performance_estimates, 
    study_var_name = study_var_name) |> 
    dplyr::bind_rows()
}

pool_performance_estimates_for_study <- function(study, performance_estimates, study_var_name){
  metrics <- unique(performance_estimates$metric)
  study_estimates <- performance_estimates[performance_estimates[,study_var_name] == study,]
  pooled_results <- lapply(metrics, pool_performance_estimates_for_metric, performance_estimates = study_estimates) |> 
    dplyr::bind_rows()
  pooled_results[,study_var_name] <- study
  
  return(pooled_results)
}

pool_performance_estimates_for_metric <- function(metric, performance_estimates){
  metric_estimates <- performance_estimates[performance_estimates$metric == metric,]
  ests <- metric_estimates$coef 
  se <- metric_estimates$se

  pooled_estiamtes <- miceafter::pool_scalar_RR(est = ests, se = se, dfcom = 500) # dfcom set to arbitrary value
  pooled_results <- data.frame(coef = pooled_estiamtes$pool_est,
                               se = pooled_estiamtes$pool_se,
                               metric = metric)
  return(pooled_results)
}
