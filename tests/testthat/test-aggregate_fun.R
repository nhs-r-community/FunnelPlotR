test_that("`aggreagate_fun` works with input and returns expected list", {
  b <- aggregate_func(
    data.frame(
      numerator = c(100, 150, 180, 89, 120, 225),
      denominator = c(102, 135, 165, 95, 100, 220),
      group = factor(c("a", "a", "a", "b", "b", "c"))
    )
  )
  expect_s3_class(b, "data.frame")
  expect_length(b, 4)
  expect_length(b$group, 3)
  expect_equal(round(b$rr, 6), c(1.069652, 1.071795, 1.022727))

})
