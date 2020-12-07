test_that("`Poisson limits are right", {
  lims <- poisson_limits(data.frame(denominator= c(15,20,30)), target=1)
  expect_length(lims,5)
  expect_length(lims[,1],3)
  expect_equal(round(lims$LCL95,7), c(0.5596924, 0.6108260, 0.6746958))
  expect_equal(round(lims$UCL95,6), c(1.649348, 1.544419, 1.427562))
  expect_equal(round(lims$LCL99,7), c(0.3862650, 0.4479107, 0.5289724))
  expect_equal(round(lims$UCL99,6), c(2.082907, 1.902094, 1.702771))
  
  expect_s3_class(funnel_grey(), "theme")
})