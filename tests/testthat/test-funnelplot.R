test_that("output is in right format", {
  #funnel_plot(c(100,150,120), c(89, 156, 995), factor(c("a","b","c")))
  expect_is(funnel_plot(c(100,150,120), c(89, 156, 995), factor(c("a","b","c"))), "list")
})

