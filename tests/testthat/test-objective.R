test_that("objective works", {
  x <- data.frame(n_detect_730_3650 = c(0, 50, 100),
                  n_730_3650 = 100)
  p <- malariasimulation::get_parameters(list(human_population = 1000))
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(x = 1, parameters = p, target = 0.5,
                         summary_function = summary_mean_pfpr_2_10, tolerance = 0,
                         weights = 1, elimination_penalty = NULL), 0)
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(x = 1, parameters = p, target = 1,
                         summary_function = summary_mean_pfpr_2_10, tolerance = 0,
                         weights = 1, elimination_penalty = NULL), -0.5)
})
