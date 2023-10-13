test_gen_cont_data <- function() {
  sigmas <- ProfacSims:::get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- ProfacSims:::generate_continuous(
    n_studies = 10,
    study_sample_size = 50,
    sigmas = sigmas
  )
  return(data)
}

test_gen_bin_data <- function() {
  data <- ProfacSims:::generate_cbcl()
  return(data)
}
