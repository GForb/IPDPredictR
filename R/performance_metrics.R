metric_calib_slope <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- stats::glm(observed_outcome ~ predicted_lp, data=data, family="binomial")
  coef <- fit$coef[2]
  se <- sqrt(diag(vcov(fit)))[2]
  return(data.frame(metric = "calib_slope", coef = coef, se = se))
}

metric_calib_itl <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- stats::glm(observed_outcome ~ 1, offset=predicted_lp, data=data, family="binomial")
  coef <- fit$coef[1]
  se <- sqrt(diag(vcov(fit)))[1]
  return(data.frame(metric = "calib_itl", coef = coef, se = se))
}

metric_cstat <- function(predicted_lp, observed_outcome) {
  predicted_prob <- exp(predicted_lp)/(1+exp(predicted_lp))
  roc_ci <- pROC::roc(observed_outcome, predicted_prob,ci=T)$ci[1:3]
  coef <- roc_ci[1]
  se <- (roc_ci[3] - coef)/qnorm(0.975)
  return(data.frame(metric = "cstat", coef = coef, se = se))
}


evaluate_performance_binary <- function(test_data, model) {
  predicted_lp <- predict(model, newdata = test_data)
  outcome <- names(stats::model.frame(model))[1]
  observed_outcome <- test_data[, outcome]

  rbind(metric_calib_slope(predicted_lp, observed_outcome),
        metric_calib_itl(predicted_lp, observed_outcome),
        metric_cstat(predicted_lp, observed_outcome))
}

metric_calib_slope_cont <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- lm(observed_outcome ~ predicted_lp, data=data)
  coef <- fit$coef[2]
  se <- sqrt(diag(vcov(fit)))[2]
  return(data.frame(metric = "calib_slope", coef = coef, se = se))
}

metric_calib_itl_cont <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  fit <- lm(observed_outcome ~ 1, offset=predicted_lp, data=data)
  coef <- fit$coef[1]
  se <- sqrt(diag(vcov(fit)))[1]
  return(data.frame(metric = "calib_itl", coef = coef, se = se))
}


rsq_oosse <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]

  mse = mean((predicted_lp-observed_outcome)^2)
  var_outcome <- var(observed_outcome)
  oosse::RsquaredSE(MSE = mse, margVar = var_outcome, SEMSE = 1, n = length(predicted_lp), corMSEMST = 0.5)["R2"]
}

rsq2 <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]
  n <- length(observed_outcome)
  mse <- sum((predicted_lp - observed_outcome)^2)/n
  mst <- var(observed_outcome)*(n+1)/n
  rsq <- 1 - (mse/mst)
  return(rsq)
}

rsq <- function(data) {
  predicted_lp <- data[,1]
  observed_outcome <- data[,2]
  n <- length(observed_outcome)
  rss <- sum((predicted_lp - observed_outcome)^2)
  tss <- sum((observed_outcome - mean(observed_outcome)) ^ 2)
  rsq <- 1 - (rss/tss)
  return(rsq)
}


metric_rsqared <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  coef <- rsq(data)
  # Use a bootstrap to estimate the variance
  se <- sqrt(var(replicate(20, rsq(data[sample(1:nrow(data), replace = TRUE),]))))
  return(data.frame(metric = "r-squared", coef = coef, se = se))
}

metric_rsqared_old <- function(predicted_lp, observed_outcome) {
  data <- data.frame(predicted_lp = predicted_lp, observed_outcome = observed_outcome)
  coef <- rsq(data)
  # Use a bootstrap to estimate the variance
  se <- sqrt(var(replicate(20, rsq(data[sample(1:nrow(data), replace = TRUE),]))))
  return(data.frame(metric = "r-squared old", coef = coef, se = se))
}


evaluate_performance_continuous_old <- function(test_data, model) {
  outcome <- names(stats::model.frame(model))[1]
  observed_outcome <- test_data[, outcome]
  predicted_lp <- predict(model, newdata = test_data)
  rbind(
    metric_calib_slope_cont(predicted_lp, observed_outcome),
    metric_calib_itl_cont(predicted_lp, observed_outcome),
    metric_rsqared(predicted_lp, observed_outcome),
    metric_rsqared_old(predicted_lp, observed_outcome),
    make.row.names = FALSE
   )
}


evaluate_performance_continuous <- function(test_data, model, new_studies = FALSE) {
  outcome <- names(stats::model.frame(model))[1]


  if(new_studies){
    intercept_data <- test_data |> dplyr::filter(int_est == TRUE)
    intercepts <- predict_intercepts(model,intercept_data , cluster_var = "studyid")

        # merge intercepts onto test data
    test_data <- test_data |> dplyr::filter(int_est == FALSE)
    test_data <- dplyr::left_join(test_data, intercepts, by = "studyid")
    observed_outcome <- test_data[, outcome]
    predicted_lp <- predict_fixed(model, newdata = test_data)
    predicted_lp <- predicted_lp + test_data$pred_intercept
  } else {
    observed_outcome <- test_data[, outcome]
    if("glm" %in% class(model)) {
      predicted_lp <- predict(model, newdata = test_data, type = "response")
    } else {
      predicted_lp <- predict(model, newdata = test_data)
    }
  }
  evaluate_performance_cont_obs_pred(observed_outcome, predicted_lp)


}

evaluate_performance_cont_obs_pred <- function(observed, predicted) {
  rbind(
    metric_calib_slope_cont(predicted, observed),
    metric_calib_itl_cont(predicted, observed),
    metric_rsqared(predicted, observed),
    make.row.names = FALSE
  )
}


evaluate_performance_continuous_new_studies <- function(test_data, model) {
  evaluate_performance_continuous(test_data, model, new_studies = TRUE)
}


