test_that("objective works works", {
  x <- data.frame(n_detect_730_3650 = c(0, 50, 100),
                  n_730_3650 = 100)
  p <- malariasimulation::get_parameters(list(human_population = 1000))
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(1, p, target = 0.5, 2, summary_pfpr_2_10, 0), 0)
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(1, p, target = 0.5, 1, summary_pfpr_2_10, 0), -0.5)
})
