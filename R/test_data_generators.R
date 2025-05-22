# this probably introduces circular dependency with ProFacSims

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


test_gen_cont_random_x_data <- function(){
  n_predictors <- 12
  sigmas <- ProfacSims:::get_sigmas(n_predictors = n_predictors, ICC = 0.3, R2 = 0.7)
  sigma_beta_x = sigmas$beta_x/2
  
  n_studies = 10
  study_sample_size = 50
  total_n = 500
  
  rand_beta1s <-  rnorm(n_studies, sd = sigma_beta_x)
  
  study_intercepts <- rnorm(n_studies, sd = sigmas$u)
  intercepts = data.frame(studyid = 1:n_studies, study_intercept = study_intercepts)
  data <- intercepts[rep(seq_len(nrow(intercepts)), study_sample_size), ]
                     
  predictors <- ProfacSims:::generate_predictors(n = total_n, n_predictors = n_predictors, 
                                    intercepts = 0, beta_int = sigmas$beta_int)
  
  data <- cbind(data, predictors)
  data$error <- rnorm(total_n, sd = sigmas$e)
  
  betas <-  rep(sigmas$beta_x, n_predictors)
  
  predictor_matrix <- predictors |> as.matrix()
  betas_matrix <- as.matrix(betas) 
  
  predictor_matrix %*% betas_matrix
  
  for(j in 1:n_studies){
    data[data$studyid == j,]
    betas[1] <- betas[1] + rand_beta1s[j]
    betas_matrix <- as.matrix(betas)
    predictor_matrix <- predictors[data$studyid == j,] |> as.matrix()
    data[data$studyid == j,"lp"]<-  predictor_matrix %*%   betas 
  }
  
  data$y  <-  data$lp + data$error + data$study_intercept
  
  return(data)
  
}