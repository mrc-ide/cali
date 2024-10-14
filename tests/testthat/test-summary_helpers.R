test_that("pfpr 2-10 helper works", {
  x <- data.frame(n_detect_lm_730_3650 = c(0, 50, 100),
                  n_age_730_3650 = 100)
  expect_equal(summary_mean_pfpr_2_10(x), mean(c(0, 0.5, 1)))
})
