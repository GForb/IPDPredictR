#' Prediction of cluster intercepts
#'
#' For lmerMod models it returns empirical Bayes predicitons of intercepts. For lm models it returns maximum liklihood estaitmes.
#' @inherit predict_fixed
#' @param cluster_var The name of the cluster variable as a string. Note for lm models it assumes other predictors start with "x"
#'
#' @returns A data.frame with the first column being the cluster variable and the second column being the predicted intercept.
#' @export
#'
#' @examples
predict_intercepts <- function(model, newdata, cluster_var = "studyid") {
  if(class(model)[1]== "lmerMod"){
    intercepts <- get_rand_int(model, newdata)
  } else if(class(model)[1]== "lm"){
    intercepts <- get_fixed_int_offset(model, newdata, cluster_var)
  }
}

#' Predictions from a model excluding the cluster variable
#' 
#' Provides predictions for linear predictor of a model excluding the study var for random intercept or linear regression models. 
#' For random intercept models it returns the predictions with random intercepts set to zero. 
#' For linear regression models it returns the predictions based only on variables that start with the letter "x"
#'
#' @param model A model, either lm or lmerMod
#' @param newdata A data frame with the same structure as the data used to fit the model
#'
#' @returns A vector of predicted values
#' @export 
#'
#' @examples
predict_fixed <- function(model, newdata) {
  if(class(model)[1]== "lmerMod"){
    pred  <- predict(model, newdata = newdata, re.form = NA, allow.new.levels = TRUE)
  } else if(class(model)[1]== "lm"){
    pred <- get_x_prediction(model, newdata)
  }
}

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

# This implements A framework for developing, implementing, and evaluating clinical prediction models in an individual participant data meta-analysis section 2.2.4 intercept estimation from new data
# This will probably need to be different for binary data



get_fixed_int_offset <- function(model, newdata, cluster_var) {
  outcome <- names(stats::model.frame(model))[1]

  pred <- get_x_prediction(model, newdata)
  total_error = newdata[,outcome] - pred
  by_cluster = stats::aggregate(total_error, list(newdata[,cluster_var]), FUN=mean)
  colnames(by_cluster) <-  c(cluster_var, "mean_error")

  prediction <-  by_cluster[,c(cluster_var, "mean_error")]
  colnames(prediction) <- c(cluster_var, "pred_intercept")

  return(prediction)
}

get_x_prediction <- function(model, newdata) {
  predictors <- newdata |> dplyr::select(dplyr::starts_with("x"))
  predictor_names <- colnames(predictors)
  betas <- model$coef[predictor_names]
  pred_fixed <- as.matrix(predictors)%*%betas
  return(pred_fixed)
}
