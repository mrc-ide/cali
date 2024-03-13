test_that("Bounded to unbounded", {
  # Transformations are reversible
  input <- 1
  limits <- c(0, 100)
  expect_equal(
    from_real(
      to_real(
        input, 
        limits
      ),
      limits
    ),
    input
  )
  
  # Transformations are reversible
  input <- 1
  limits <- c(0, 100)
  expect_equal(
    to_real(
      from_real(
        input, 
        limits
      ),
      limits
    ),
    input
  )
})

test_that("Proposal works", {
  input <- 1
  limits <- c(0, 100)
  
  # Direction is correct
  expect_gt(
    proposal(input, limits, "increase"),
    input
  )
  expect_lt(
    proposal(input, limits, "decrease"),
    input
  )
  
  # Limits are respected
  expect_equal(
    proposal(input, limits,  "increase", 500),
    limits[2]
  )
  expect_gt(
    proposal(input, limits, "decrease", 500),
    limits[1]
  )
})
