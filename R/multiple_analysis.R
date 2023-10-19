model_rows_study_cols <- function(variables) {
  
}

IPDMA_predict_multiple_models <- function(model_function_list, model_labels = NULL, data , InternalExternalCV = TRUE, predict_function = predict, evaluate_performance, test_data = NULL, out_var_name, study_var_name = "studyid") {
  if(!is.null(model_labels)){
    check_model_labels(model_function_list, model_labels)
  }
  check_IECV_test_data(InternalExternalCV, test_data)
  
  if(InternalExternalCV){
    
  } else {
    results_list <- lapply(model_function_list, 
        function(model_function)  ipdma_prediction_pipeline(
                                    data = data, 
                                    model_function = model_function,
                                    InternalExternalCV = FALSE,
                                    predict_function = predict_function,
                                    evaluate_performance = evaluate_performance,
                                    test_data = test_data,
                                    study_var_name = study_var_name,
                                    out_var_name = out_var_name)
        )
  }
  results_df <- dplyr::bind_rows(results_list, .id = "model") |> 
    dplyr::select(model, dplyr::everything())
  
  if(!is.null(model_labels)){
    results_df <-  results_df |> add_names_to_results(model_labels)
  }
  
  return(results_df)
}

check_model_labels <- function(model_function_list, model_labels) {
  if(length(model_function_list) != length(model_labels)){
    stop("The number of model labels provided must be the same as the number of models")
  }
  
}

add_names_to_results <- function(results_df, model_labels) {
  reference <- data.frame(model = unique(results_df$model), model_labels = model_labels)
  results_df <-  results_df |> 
    dplyr::left_join(reference, by = dplyr::join_by(model)) |> 
    dplyr::mutate(model = model_labels) |> 
    dplyr::select(-model_labels)
  
}

multiple_models <- function(results_list) {
  
}