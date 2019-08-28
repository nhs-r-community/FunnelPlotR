test_that("`funnel_plot()` works with input and returns expected list", {
  a<-funnel_plot(c(100, 150,180,80,120, 225), c(108, 112, 165,95,100, 220),
                 factor(c("a","b","c", "d","e","f"))
                 )
  expect_type(a, "list")
  expect_type(a[[1]], "list")
  expect_s3_class(a[[2]], "data.frame")
  expect_s3_class(a[[3]], "data.frame")
  expect_length(a[[2]]$group,6)
  expect_length(a[[2]],22)

  b<-funnel_plot(numerator=c(100, 150,180,80,120, 225), denominator=c(108, 112, 165,95,100, 220),
                 group=factor(c("a","b","c", "d","e","f")), OD_adjust = FALSE,
                 title="My test Funnel Plot", multiplier = 100, x_label = "Expected Values",
                 y_label = "Standardised Ratio Test", label_outliers = 95)
  expect_type(b, "list")
  expect_type(b[[1]], "list")
  expect_s3_class(b[[2]], "data.frame")
  expect_s3_class(b[[3]], "data.frame")
  expect_length(a[[2]]$group,6)
  expect_length(a[[2]],22)
  expect_gt(b[[2]]$LCL95[5], a[[2]]$OD95LCL[5])

  c<-funnel_plot(numerator=c(100, 150,180,80,120, 225), denominator=c(108, 112, 165,95,100, 220),
                 group=factor(c("a","b","c", "d","e","f")), OD_adjust = TRUE, method="CQC", Winsorise_by = 0.05,
                 title="My test Funnel Plot", multiplier = 100, x_label = "Expected Values",
                 y_label = "Standardised Ratio Test", label_outliers = 95)
  expect_type(c, "list")
  expect_type(c[[1]], "list")
  expect_s3_class(c[[2]], "data.frame")
  expect_s3_class(c[[3]], "data.frame")
  expect_length(c[[2]]$group,6)
  expect_length(c[[2]],22)
  expect_gt(c[[2]]$OD95LCL[5], b[[2]]$OD95LCL[5])

})



