test_model_cont <- function(data) {
  ProfacSims:::model_lm_fixed_int(data)
}

test_model_bin <- function(data) {
  ProfacSims:::model_logistic_cbcl_test(data)
}