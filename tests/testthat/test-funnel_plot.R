test_that("`funnel_plot()` works with input and returns expected list", {
  a<-funnel_plot(c(100, 150,180,80,120, 225), c(108, 112, 165,95,100, 220),
                 factor(c("a","b","c", "d","e","f")), return_elements=c("plot", "data", "limits")
                 )
  expect_type(a, "list")
  expect_type(a[[1]], "list")
  expect_s3_class(a[[2]], "data.frame")
  expect_s3_class(a[[3]], "data.frame")
  expect_length(a[[3]]$group,6)
  expect_length(a[[3]],21)

  b<-funnel_plot(numerator=c(100, 150,180,80,120, 225), denominator=c(108, 112, 165,95,100, 220),
                 group=factor(c("a","b","c", "d","e","f")), OD_adjust = FALSE,
                 title="My test Funnel Plot", multiplier = 100, x_label = "Expected Values",
                 y_label = "Standardised Ratio Test", label_outliers = TRUE, limit=95)
  expect_type(b, "list")
  expect_type(b[[1]], "list")
  expect_s3_class(b[[2]], "data.frame")
  expect_s3_class(b[[3]], "data.frame")
  expect_length(b[[3]]$group,6)
  expect_length(b[[3]],21)
  expect_gt(b[[3]]$LCL95[5], a[[3]]$OD95LCL[5])

  c<-funnel_plot(numerator=c(100, 150,180,80,120, 225), denominator=c(108, 112, 165,95,100, 220),
                 group=factor(c("a","b","c", "d","e","f")), OD_adjust = TRUE, sr_method="CQC", trim_by = 0.05,
                 title="My test Funnel Plot", multiplier = 100, x_label = "Expected Values",
                 y_label = "Standardised Ratio Test", label_outliers = TRUE, limit=95)
  expect_type(c, "list")
  expect_type(c[[1]], "list")
  expect_s3_class(c[[2]], "data.frame")
  expect_s3_class(c[[3]], "data.frame")
  expect_length(c[[3]]$group,6)
  expect_length(c[[3]],19)
  expect_gt(b[[3]]$OD95LCL[5], c[[3]]$OD95LCL[5])

  d<-funnel_plot(numerator=c(100, 150,180,80,120, 225), denominator=c(108, 112, 165,95,100, 220),
                 group=factor(c("a","b","c", "d","e","f")), OD_adjust = FALSE, sr_method="CQC", trim_by = 0.05,
                 title="My test Funnel Plot", multiplier = 100, x_label = "Expected Values",
                 y_label = "Standardised Ratio Test", label_outliers = TRUE, limit=95, xrange=c(5,250)
                 , yrange=c(0, 200))
  expect_type(d, "list")
  expect_type(d[[1]], "list")
  expect_s3_class(d[[2]], "data.frame")
  expect_s3_class(d[[3]], "data.frame")
  expect_length(d[[3]]$group,6)
  expect_length(d[[3]],19)


})



