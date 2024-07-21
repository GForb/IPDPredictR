test_that("get_rand_int", {
  set.seed(1234)
  data <- test_gen_cont_data()
  
  model <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                      data = data)
  
  pred_re <- lme4::ranef(model)
  my_re <- get_rand_int(model, newdata = data)
  chatGPT_re <- predict_random_effects(model, newdata = data)
  diff <- pred_re$studyid - my_re$pred_intercept
  expect_equal(length(pred_re$studyid [[1]]), length(my_re$pred_intercept))
  
  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)
  
  my_re <- predict_intercepts(model, newdata = data, cluster_var = "studyid")
  my_re <- get_rand_int(model, newdata = data)
  diff <- pred_re$studyid - my_re$pred_intercept
  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)
  
  
})



test_that("eb", {
  set.seed(1234)
  data <- test_gen_cont_random_x_data()
  
  model1 <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (1|studyid)",
                       data = data)
  
  pred_re <- lme4::ranef(model1)$studyid
  my_re <- eb(model1, newdata = data)[,"(Intercept)"]
  diff <- pred_re - my_re
  expect_equal(length(pred_re[[1]]), length(my_re))
  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)
  
  model2 <- lme4::lmer("y ~ x1 + x2 + x3+ x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12+ (x1|studyid)",
                       data = data)
  
  pred_re <- lme4::ranef(model2)$studyid
  my_re <- eb(model2, newdata = data)[,-1]
  diff <- pred_re - my_re
  expect_equal(length(pred_re), length(my_re))
  expect_equal(sum(abs(diff)), 0, tolerance = 0.0001)
  
  
  
})