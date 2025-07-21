test_that("`funnel_plot()` works with input and returns expected list", {
  dt <-
    data.frame(
      num = c(100, 150, 180, 80, 120, 225)
      , denom = c(108, 112, 165, 95, 100, 220)
      , group = factor(c("a", "b", "c", "d", "e", "f"))
    )

  a <- funnel_plot(dt, num, denom, group)
  expect_type(a, "list")
  expect_true(is_ggplot(a[[1]]))
  expect_s3_class(a[[2]], "data.frame")
  expect_s3_class(a[[3]], "data.frame")
  expect_length(a[[3]]$group, 6)
  expect_length(a[[3]], 22)

  b <- funnel_plot(dt, num, denom, group, draw_adjusted = FALSE,
                   title = "My test Funnel Plot", multiplier = 100
                   , x_label = "Expected Values"
                   , y_label = "Standardised Ratio Test"
                   , label = "outlier", limit = 95)
  expect_type(b, "list")
  expect_true(is_ggplot(b[[1]]))
  expect_s3_class(b[[2]], "data.frame")
  expect_s3_class(b[[3]], "data.frame")
  expect_length(b[[3]]$group, 6)
  expect_length(b[[3]], 22)
  expect_gt(b[[3]]$LCL95[5], a[[3]]$OD95LCL[5])

  c <- funnel_plot(dt, num, denom, group, draw_adjusted = TRUE
                   , sr_method = "CQC", trim_by = 0.05
                   , title = "My test Funnel Plot", multiplier = 100
                   , x_label = "Expected Values"
                   , y_label = "Standardised Ratio Test"
                   , label = "highlight", limit = 95, highlight = "a")
  expect_type(c, "list")
  expect_true(is_ggplot(c[[1]]))
  expect_s3_class(c[[2]], "data.frame")
  expect_s3_class(c[[3]], "data.frame")
  expect_length(c[[3]]$group, 6)
  expect_length(c[[3]], 20)
  expect_lt(b[[3]]$OD95LCL[5], c[[3]]$OD95LCL[5])
  expect_equal(source_data(c)$highlight[1], "1")

  d <- funnel_plot(dt, num, denom, group, draw_adjusted = FALSE
                   , sr_method = "CQC", trim_by = 0.05
                   , title = "My test Funnel Plot", multiplier = 100
                   , x_label = "Expected Values"
                   , y_label = "Standardised Ratio Test", label = "both"
                   , limit = 95, x_range = c(5, 250)
                   , y_range = c(0, 200), highlight = "a")
  expect_type(d, "list")
  expect_true(is_ggplot(d[[1]]))
  expect_s3_class(d[[2]], "data.frame")
  expect_s3_class(d[[3]], "data.frame")
  expect_length(d[[3]]$group, 6)
  expect_length(d[[3]], 20)


})
