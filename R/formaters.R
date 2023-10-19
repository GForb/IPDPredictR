
results_flextable <- function(results_df) {
  n_rows = length(results_df)
  hlines_at = 3* (1:(n_rows -1)/3)
  results_df |> 
    compress_columns |> 
    flextable::flextable() |> 
    flextable::merge_v(j = "model") |> 
    flextable::hline(i = hlines_at) |> 
    flextable::colformat_double(j = "se", digits = 3) |> 
    flextable::colformat_double(j = "tau2", digits = 4)
}

compress_columns <- function(results_df, digits = 2) {
  results_df |> dplyr::mutate(ci = form_interval(ci.lb, ci.ub, digits))
  |> dplyr::mutate(pi = form_interval(pi.lb, pi.ub, digits)) |> 
    select(-ci.lb, -ci.ub, -pi.lb, -pi.ub)
  
}

form_interval <- function(ll, ub, digits) {
  paste0("(", formatC(ll, digits = digits, format = "fg"), ", ", formatC(ub, digits = digits, format = "fg"), ")")
}
