
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

#' Format results data.frame so it has text columns with intervals in
#'
#' @param results_df A data frame with meta-analysis results
#' @param digits The number of decimal places to use with prediciton and confidence intervals
#'
#' @returns A formatted data.frame
#' @export
#'
#' @examples
compress_columns <- function(results_df, digits = 2) {
  results_df |> dplyr::mutate(ci = form_interval(ci.lb, ci.ub, digits)) |> 
    dplyr::mutate(pi = form_interval(pi.lb, pi.ub, digits)) |> 
    dplyr::select(-dplyr::starts_with("ci."), -dplyr::starts_with("pi."))
  
}

est_tau2 <- function(results_df, digits = 2) {
  results_df |> 
    dplyr::mutate(est_tau2 = paste0(
                          round(est, digits = digits), 
                          " (tau2 = ", 
                          round(tau2, digits = digits),
                          ")"
                        )) |> 
    dplyr::select(-est, -se, -tau2, -dplyr::starts_with("ci"), -dplyr::starts_with("pi"))
}

form_interval <- function(ll, ub, digits) {
  paste0("(", round(ll, digits = digits), ", ", round(ub, digits = digits), ")")
}
