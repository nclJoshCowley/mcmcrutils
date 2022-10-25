# TODO: Unit test for sigma (scalar) ...
# TODO: Unit test for beta (2 dimensional) ...

test_that("add_truth with expected input", {
  expect_equal(
    draws_add_truth(draws_example$alpha, alpha = 1:2)$.truth,
    unname(unlist(term::tindex(draws_example$alpha$.term)))
  )
})

test_that("add_truth with name mismatch", {
  expect_warning(
    expect_equal(
      draws_add_truth(draws_example$alpha, a = 1:2)$.truth,
      rep(NA_real_, nrow(draws_example$alpha))
    )
  )
})

test_that("add_truth with dimension mismatch", {
  expect_warning(
    expect_equal(
      draws_add_truth(draws_example$alpha, alpha = 1)$.truth,
      rep(NA_real_, nrow(draws_example$alpha))
    )
  )
})
