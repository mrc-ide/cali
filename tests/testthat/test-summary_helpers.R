test_that("pfpr 2-10 helper works", {
  x <- data.frame(n_detect_730_3650 = c(0, 50, 100),
                  n_730_3650 = 100)
  expect_equal(summary_pfpr_2_10(x), c(0, 0.5, 1))
})
