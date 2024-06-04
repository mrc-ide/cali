test_that("Elimination check works", {
  target <- c(0.1, 0.1, 0)
  expect_false(
    check_elimination(
      c(0.1, 0.1, 0.1),
      target
    )
  )
  expect_true(
    check_elimination(
      c(0, 0.1, 0.1),
      target
    )
  )
  expect_false(
    check_elimination(
      output = c(0.1, 0.1, 0),
      target = target
    )
  )
  expect_true(
    check_elimination(
      output = c(0, 0.1, 0),
      target = target
    )
  )
  
  expect_equal(
    linear_interpolate(1:2, c(-0.1, 0.1)),
    linear_interpolate(1:2, c(0.1, -0.1))
  )
  
  expect_equal(
    linear_interpolate(1:2, c(-0.1, 0.1)),
    mean(1:2)
  )
  expect_equal(
    linear_interpolate(1:2, c(0, 0)),
    mean(1:2)
  )
  
})
