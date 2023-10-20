IPDMA_predict_model_rows_study_cols <- function(model_factories, mdoel_lables=NULL, 
                                                outcome_var_names, outcome_labels=NULL, 
                                                data, 
                                                InternalExternalCV = 
                                                  TRUE, 
                                                predict_function, 
                                                evaluate_performance, 
                                                test_data = NULL, 
                                                study_var_name = "studyid") {
  if(!is.null(model_labels)){
    if(!label_check(model_function_list, model_labels)) {
      stop("Model function list must be same length as model_labels")
    }
  }
  if(!is.null(outcome_labels)){
    if(!label_check(outcome_var_names, outcome_labels)) {
      stop("Model function list must be same length as model_labels")
    }
  }
  
  df_list <- lapply(outcome_var_names, 
         get_results_for_outcome, 
         model_factories = model_factories,
         mdoel_lables=mdoel_lables, 
         data = data,
         InternalExternalCV = InternalExternalCV,
         predict_function = predict_function, 
         evaluate_performance= evaluate_performance, 
         test_data = test_data, 
         study_var_name = study_var_name)
  
  results_df <- bind_rows(df_list)
  
  return(results_df)
  
}

  
get_results_for_outcome <- function(outcome_var_name, model_factories, ...) {
  model_function_list <- get_model_function_list(model_factories, outcome_var_name)
  results_df <-  IPDMA_predict_multiple_models(model_function_list, outcome_var_name = outcome_var_name, ...) 
  results_df$outcome <- outcome_var_name
  return(results_df)
}


get_model_function_list <- function(model_factory_list, outcome) {
  lapply(model_factory_list, function(factory) factory(outcome)) 
}



