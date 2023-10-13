test_that("get_rand_int", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                      data = data)

  pred_re <- lme4::ranef(model)
  my_re <- get_rand_int(model, newdata = data)
  diff <- pred_re$studyid - my_re$pred_intercept
  expect_equal(length(pred_re$studyid [[1]]), length(my_re$pred_intercept))

  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)

  my_re <- predict_intercepts(model, newdata = data, cluster_var = "studyid")
  my_re <- get_rand_int(model, newdata = data)
  diff <- pred_re$studyid - my_re$pred_intercept
  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)


})



test_that("get_x_prediction", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lm("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12",
              data = data)

 pred <- predict(model)
 fixed_pred <- pred - model$coef["(Intercept)"]

  pred_x <- get_x_prediction(model, newdata = data)
  diff <- sum(abs(pred_x - fixed_pred))
  expect_equal(diff, 0, tol = 0.0001)


})

test_that("get_fixed_int_offset", {
  set.seed(1234)
  sigmas <- get_sigmas(n_predictors = 12, ICC = 0.3, R2 = 0.7)
  data <- generate_continuous(10,100, sigmas= sigmas)

  model <- lm("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+studyid",
                      data = data)

  my_re <- get_fixed_int_offset(model, newdata = data, cluster_var = "studyid")
  expect_equal(nrow(my_re), 10)
  expect_equal(ncol(my_re), 2)

  my_re <- predict_intercepts(model, newdata = data, cluster_var = "studyid")
  expect_equal(nrow(my_re), 10)
  expect_equal(ncol(my_re), 2)


})


