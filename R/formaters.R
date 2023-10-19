
results_flextable <- function(results_df, est_digits = 2) {
  n_rows = nrow(results_df)
  hlines_at = 3* (1:((n_rows -1)/3))
  ft <- results_df |> 
    compress_columns(digits = est_digits) |> 
    flextable::flextable() |> 
    flextable::hline(i = hlines_at) |> 
    flextable::colformat_double(j = "est", digits = est_digits) |> 
    flextable::colformat_double(j = "se", digits = 3) |> 
    flextable::colformat_double(j = "tau2", digits = 4)
  if("model" %in% colnames(results_df)){
    ft <-  ft |> flextable::merge_v(j = "model") 
  }
  return(ft)
}

compress_columns <- function(results_df, digits = 2) {
  results_df |> dplyr::mutate(ci = form_interval(ci.lb, ci.ub, digits)) |> 
    dplyr::mutate(pi = form_interval(pi.lb, pi.ub, digits)) |> 
    dplyr::select(est, ci, pi, se, tau2)
  
}

form_interval <- function(ll, ub, digits) {
  paste0("(", formatC(ll, digits = digits, format = "fg"), ", ", formatC(ub, digits = digits, format = "fg"), ")")
}
