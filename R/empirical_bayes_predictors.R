# Empirical Bayes predictiors:

# Post estimation commands for lme4::lmer

# Implements formula given in appendix A of Skrondel and Heskith p xx

# 1. Random intercept models

#' Returns empirical bayes predictions for random intercepts for new clusters for a random intercept model estimated with lme4
#'
#' @param model An lme4 model object
#' @param newdata A data frame with the same structure as the data used to fit the model
#'
#' @returns Returns a data.frame, the first column is the cluster variable and the second column is the empirical bayes prediction for the intercept.
#' @export
#'
#' @examples
get_rand_int <- function(model, newdata) {
  
  outcome <- names(stats::model.frame(model))[1]
  
  cluster_var <- names(model@cnms)
  varCorr <- lme4::VarCorr(model) |> as.data.frame()
  var_u <-  varCorr[1,4]
  var_e <- varCorr[2,4]
  
  fixed_pred <- predict(model, newdata = newdata, re.form = NA)
  total_error = newdata[,outcome] - fixed_pred
  by_cluster <-  stats::aggregate(total_error, list(newdata[,cluster_var]), FUN=mean)
  counts<- stats::aggregate(newdata$studyid, list(newdata[,cluster_var]), FUN=length)
  by_cluster$n <- counts[[2]]
  colnames(by_cluster) <-  c(cluster_var, "mean_error", "n")
  
  R  <-  var_u/(var_u + var_e/by_cluster$n)
  
  by_cluster$blup = by_cluster$mean_error*R
  prediction <- by_cluster[,c(cluster_var, "blup")]
  colnames(prediction) <- c(cluster_var, "pred_intercept")
  
  
  return(prediction)
}

# 2. Random slope

#' Empirical Bayes predictions for 2 level mixed effects models estimated using lme4 THIS FUNCTION DOES NOT WORK
#'
#' @inherit get_rand_int
#'
#' @returns
#'
#' @examples
eb <- function(model, newdata) {
  outcome <- names(stats::model.frame(model))[1]
  
  cluster_var <- names(model@cnms)
  clusters <- newdata[,cluster_var] |> unique()
  varCorr <- lme4::VarCorr(model) |> as.data.frame()
  varCorrRE <- varCorr[varCorr$grp == cluster_var,]
  varCorrRE[is.na(varCorrRE$var2),"var2"] <- varCorrRE[is.na(varCorrRE$var2),"var1"]
  random_effects_vars <- unique(varCorrRE$var1)
  Q <- length(random_effects_vars)
  newdata[,"(Intercept)"] <- 1

  PSI_hat <-  matrix(ncol = Q, nrow = Q, dimnames = list(random_effects_vars, random_effects_vars))
  for (i in 1:nrow(varCorrRE)) {
    row_name <- varCorrRE$var1[i]
    col_name <- varCorrRE$var2[i]
    PSI_hat[row_name, col_name] <- varCorrRE$vcov[i]
    # Fill the symmetric value for the covariance matrix
    if (row_name != col_name) {
      PSI_hat[col_name, row_name] <- varCorrRE$vcov[i]
    }
  }
  theta <- varCorr[varCorr$grp == "Residual",4]
  
  eb_preds <- data.frame(cluster = clusters)
  eb_preds[, random_effects_vars] <- NA
  
  for (j in clusters) {
    newdata_j <- newdata[ newdata[,cluster_var]==j, ]
    Z_j <- newdata_j[ , random_effects_vars] |> as.matrix()
    n_j <- nrow(Z_j)
  
    THETA_hat_j <- diag(theta, nrow = n_j) 
    SIGMA_hat_j <- Z_j %*% PSI_hat %*% t(Z_j) + THETA_hat_j # estimated residual covariance matrix of j
    
    fixed_pred_j <- predict(model, newdata = newdata_j, re.form = NA)
    total_error_j = (newdata_j[,outcome] - fixed_pred_j) |> as.matrix()
    
    eb_pred <- PSI_hat %*% t(Z_j) %*% solve(SIGMA_hat_j) %*% total_error_j
    eb_preds[eb_preds$cluster == j, random_effects_vars] <- eb_pred
  }   
  return(eb_preds)
}



