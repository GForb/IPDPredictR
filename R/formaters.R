
results_flextable <- function(results_df, est_digits = 2, ci_column_width_in = 1.4) {
  n_rows = nrow(results_df)
  hlines_at = 3* (1:((n_rows -1)/3))
  ft <- results_df |> 
    compress_columns(digits = est_digits) |> 
    flextable::flextable() |> 
    flextable::hline(i = hlines_at) |> 
    flextable::colformat_double(j = "est", digits = est_digits) |> 
    flextable::colformat_double(j = "se", digits = 3) |> 
    flextable::colformat_double(j = "tau2", digits = 4) |> 
    flextable::width(j = c("ci", "pi"), width = ci_column_width_in)
  if("model" %in% colnames(results_df)){
    ft <-  ft |> flextable::merge_v(j = "model") 
  }
  return(ft)
}

compress_columns <- function(results_df, digits = 2) {
  results_df |> dplyr::mutate(ci = form_interval(ci.lb, ci.ub, digits)) |> 
    dplyr::mutate(pi = form_interval(pi.lb, pi.ub, digits)) |> 
    dplyr::select(-dplyr::starts_with("ci"), -dplyr::starts_with("pi"))
  
}

est_tau2 <- function(results_df, digits = 2) {
  results_df |> 
    dplyr::mutate(est_tau2 = paste0(
                          formatC(est, digits = digits, format = "fg"), 
                          " (tau2 = ", 
                          formatC(tau2, digits = digits, format = "fg"),
                          ")"
                        )) |> 
    dplyr::select(-est, -se, -dplyr::starts_with("ci"), -dplyr::starts_with("pi"))
}

form_interval <- function(ll, ub, digits) {
  paste0("(", formatC(ll, digits = digits, format = "fg"), ", ", formatC(ub, digits = digits, format = "fg"), ")")
}
