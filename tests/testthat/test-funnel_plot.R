test_that("`funnel_plot()` works with input and returns expected list", {
  a<-funnel_plot(c(100, 150,180,89,120, 225), c(102, 135, 165,95,100, 220),
                 factor(c("a","b","c", "d","e","f"))
                 )
  expect_type(a, "list")
  expect_type(a[[1]], "list")
  expect_s3_class(a[[2]], "data.frame")
  expect_s3_class(a[[3]], "data.frame")
  expect_length(a[[2]]$group,6)
  expect_gt(length[])
})
