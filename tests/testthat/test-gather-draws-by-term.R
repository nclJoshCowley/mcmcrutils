test_that("gather_draws for single mcmcarray", {
  alpha <- mcmcr::mcmcr_example$alpha
  out <- gather_draws_by_term(alpha, "alpha")

  expect_equal(
    with(out, .value[.chain == 1 & .iteration == 50 & .term == "alpha[1]"]),
    alpha[1, 50, 1]
  )

  expect_equal(
    with(out, .value[.chain == 2 & .iteration == 60 & .term == "alpha[1]"]),
    alpha[2, 60, 1]
  )

  expect_equal(
    with(out, .value[.chain == 1 & .iteration == 400 & .term == "alpha[2]"]),
    alpha[1, 400, 2]
  )
})

test_that("gather_draws for mcmcr elements", {
  mcmcr_eg <- mcmcr::mcmcr_example
  out <- gather_draws_by_term(mcmcr_eg)


  expect_equal(
    with(out$alpha, .value[.chain == 1 & .iteration == 5 & .term == "alpha[2]"]),
    mcmcr_eg$alpha[1, 5, 2]
  )

  expect_equal(
    with(out$beta, .value[.chain == 2 & .iteration == 400 & .term == "beta[2,1]"]),
    mcmcr_eg$beta[2, 400, 2, 1]
  )

  expect_equal(
    with(out$sigma, .value[.chain == 1 & .iteration == 75 & .term == "sigma"]),
    mcmcr_eg$sigma[1, 75, 1]
  )
})
