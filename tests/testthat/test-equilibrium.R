test_that("Equilibrium starting point errors informatively", {
  expect_error(
    get_eq_eir(target_pfpr = 0),
    "eq_pfpr_2_10 must be between 0 and 0.9"
  )
  expect_error(
    get_eq_eir(target_pfpr = 0.91),
    "eq_pfpr_2_10 must be between 0 and 0.9"
  )
  expect_error(
    get_eq_eir(target_pfpr = 0.1, ft = -0.1),
    "ft must be between 0 and 1"
  )
  expect_error(
    get_eq_eir(target_pfpr = 0.1, ft = 1.1),
    "ft must be between 0 and 1"
  )
})
