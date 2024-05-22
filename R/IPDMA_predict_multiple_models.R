IPDMA_predict_multiple_models <- function(model_function_list, 
                                          model_labels = NULL, 
                                          data , 
                                          InternalExternalCV = TRUE, 
                                          predict_function = predict, 
                                          evaluate_performance, 
                                          test_data = NULL, 
                                          out_var_name, 
                                          study_var_name = "studyid") {
  if(!is.null(model_labels)){
    if(!label_check(model_function_list, model_labels)) {
      stop("Model function list must be same length as model_labels")
    }
  }

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

label_check <- function(list, labels) {
  if(length(list) != length(labels)){
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}

add_names_to_results <- function(results_df, model_labels) {
  reference <- data.frame(model = unique(results_df$model), model_labels = model_labels)
  results_df <-  results_df |> 
    dplyr::left_join(reference, by = dplyr::join_by(model)) |> 
    dplyr::mutate(model = model_labels) |> 
    dplyr::select(-model_labels)
  
}

