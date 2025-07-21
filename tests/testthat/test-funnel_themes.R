test_that("`themes are not broken", {
  expect_s3_class(funnel_clean(), "theme")
  expect_s3_class(funnel_grey(), "theme")
})
