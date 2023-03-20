test_that("`build limits funcion` works with input and returns expected data.frame", {
  z<-build_limits_lookup(10, 250, 0.65, 1.51, draw_adjusted=TRUE, tau2=0.02,
                         data_type="SR", sr_method="SHMI", multiplier=1, target=1)
 
  
  expect_s3_class(z, "data.frame")
  expect_length(z, 10)
  expect_length(z$number.seq, 1000)
  #expect_equal(round(b$rr,6), c(1.069652, 1.071795, 1.022727))
  
  
  y<-build_limits_lookup(10, 250, 0.2, 1.0, draw_adjusted=FALSE, tau2=0,
                         data_type="PR", sr_method="SHMI", multiplier=1, target=0.5)
  
  
  expect_s3_class(y, "data.frame")
  expect_length(y, 10)
  expect_length(y$number.seq, 1000)
  expect_lt(y$ul95[1], 1)
  expect_gt(y$ll95[1], 0)
  

  x<-build_limits_lookup(10, 250, 0.65, 1.51, draw_adjusted=TRUE, tau2=0.02,
                         data_type="SR", sr_method="CQC", multiplier=100, target=1)
  
  
  expect_s3_class(x, "data.frame")
  expect_length(x, 10)
  expect_length(x$number.seq, 1000)
  expect_lt(x$ll95[1], 100)
  expect_gt(x$ul95[1], 100)
  
  #expect_equal(round(b$rr,6), c(1.069652, 1.071795, 1.022727))
  
  
})
