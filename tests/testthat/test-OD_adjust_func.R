test_that("`OD_adjust_func()` works with input and returns expected list", {
  mod_plot_agg<-data.frame(group=factor(c("a","b","c", "d","e","f")), numerator=c(100, 150,180,80,120, 225),
                           denominator=c(108, 112, 165,95,100, 220),
                           rr= c(100, 150,180,80,120, 225)/ c(108, 112, 165,95,100, 220))

  f<-OD_adjust_func(mod_plot_agg, method="SHMI", Winsorise_by= 0.1, bypass=FALSE)

  expect_type(f, "list")
  expect_s3_class(f[[1]], "data.frame")
  expect_type(f[[2]], "double")
  expect_type(f[[3]], "double")
  expect_length(f[[1]]$group,6)
  expect_length(f[[1]],22)

  g<-OD_adjust_func(mod_plot_agg, method="CQC", Winsorise_by= 0.1, bypass=FALSE)

  expect_type(g, "list")
  expect_s3_class(g[[1]], "data.frame")
  expect_type(g[[2]], "double")
  expect_type(g[[3]], "double")
  expect_length(g[[1]]$group,6)
  expect_length(g[[1]],22)


})



