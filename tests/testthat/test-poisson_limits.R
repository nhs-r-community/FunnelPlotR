test_that("`Poisson limits are right", {
  lims <- poisson_limits(data.frame(denominator= c(15,20,30)), target=1)
  expect_length(lims,5)
  expect_length(lims[,1],3)
  expect_equal(round(lims$LCL95,7), c(0.5846246, 0.6303630, 0.6883856))
  expect_equal(round(lims$UCL95,6), c(1.565975, 1.483543, 1.388295))
  expect_equal(round(lims$LCL99,7), c(0.4065417, 0.4643860, 0.5409763))
  expect_equal(round(lims$UCL99,6), c(1.990102, 1.835049,1.660121))
  
  expect_s3_class(funnel_grey(), "theme")
})