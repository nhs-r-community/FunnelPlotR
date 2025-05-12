test_that("outliers highlights correctly", {
  set.seed(12345)
  dt <- data.frame(org = factor(letters[seq(1:10)])
                   , rr = c(1, 0.95, 1.05, 0.8, 1.2, 0.6, 1.4, 0.4, 1.6, 0)
                   , UCL95 = 1.1
                   , LCL95 = 0.9
                   , OD95UCL = 1.21
                   , OD95LCL = 0.79
                   , UCL99 = 1.41
                   , LCL99 = 0.59
                   , OD99UCL = 1.62
                   , OD99LCL = 0.38
                  )


  # 95 unadjusted
  a <- outliers_func(dt, draw_adjusted = FALSE, draw_unadjusted=TRUE, limit = 95, multiplier=1)
  expect_identical(a$outlier, c(0,0,0,1,1,1,1,1,1,1))

  # 95 adjusted
  b <- outliers_func(dt, draw_adjusted = TRUE, draw_unadjusted=FALSE, limit = 95, multiplier=1)
  expect_identical(b$outlier, c(0,0,0,0,0,1,1,1,1,1))

  # 99 unadjusted
  c <- outliers_func(dt, draw_adjusted = FALSE, draw_unadjusted=TRUE, limit = 99, multiplier=1)
  expect_identical(c$outlier, c(0,0,0,0,0,0,0,1,1,1))

  # 99 adjusted
  d <- outliers_func(dt, draw_adjusted = TRUE, draw_unadjusted=FALSE, limit = 99, multiplier=1)
  expect_identical(d$outlier, c(0,0,0,0,0,0,0,0,0,1))


  # Check multiplier works
  e <- outliers_func(dt, draw_adjusted = FALSE, draw_unadjusted=TRUE, limit = 95, multiplier=100)
  expect_identical(e$outlier, c(1,1,1,1,1,1,1,1,1,1))

  # correct type of output
  expect_s3_class(e, "data.frame")
  expect_length(b, 11)
  expect_length(b$org, 10)


})
