test_that("objective works", {
  x <- data.frame(n_detect_730_3650 = c(0, 50, 100),
                  n_730_3650 = 100)
  p <- malariasimulation::get_parameters(list(human_population = 1000))
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(x = 1, parameters = p,
                         summary_function = summary_mean_pfpr_2_10,
                         target = 0.5,
                         weights = 1,
                         tolerance = 0,
                         elimination_check = FALSE), 0)
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(x = 1, parameters = p,
                         summary_function = summary_mean_pfpr_2_10,
                         target = 0,
                         weights = 1,
                         tolerance = 0,
                         elimination_check = FALSE), 0.5)
  
  x <- data.frame(n_detect_730_3650 = c(0, 0, 0),
                  n_730_3650 = 100)
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(x = 1,
            parameters = p,
            summary_function = summary_mean_pfpr_2_10,
            target = 0.5,
            weights = 1,
            tolerance = 0.02,
            elimination_check = TRUE), -1e6)
  
  x <- data.frame(n_detect_730_3650 = c(0, 0, 0),
                  n_730_3650 = 100)
  mockery::stub(objective, "malariasimulation::run_simulation", x)
  expect_equal(objective(x = 1,
                         parameters = p,
                         summary_function = summary_mean_pfpr_2_10,
                         target = 0.001,
                         weights = 1,
                         tolerance = 0.02,
                         elimination_check = TRUE), -1e6)
})
