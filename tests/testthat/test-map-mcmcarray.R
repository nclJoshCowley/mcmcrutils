test_that("map_mcmcarray works on simple function", {
  beta <- mcmcr::mcmcr_example$beta

  test_value <- map_mcmcarray(beta, ~ .x %*% c(1, 0))

  expect_equal(
    object = test_value[1, 100, 1:2, 1],
    expected = drop(beta[1, 100, , ] %*% c(1, 0))
  )
})

test_that("map2_mcmcarray works on simple function", {
  alpha <- mcmcr::mcmcr_example$alpha
  beta <- mcmcr::mcmcr_example$beta

  test_value <-
    map2_mcmcarray(
      mcmcr::mcmcr_example$beta,
      mcmcr::mcmcr_example$alpha,
      ~ .x %*% .y
    )

  expect_equal(
    object = test_value[1, 100, , ],
    expected = drop(beta[1, 100, , ] %*% alpha[1, 100, ])
  )
})

test_that("pmap_mcmcarray works on simple function", {
  l <- mcmcr::mcmcr_example
  f_test <- function(alpha, beta, sigma, a) a * (sigma %*% alpha %*% beta)

  test_value <- pmap_mcmcarray(l, f_test, a = 10)

  expect_equal(
    object = test_value[1, 100, , ],
    expected = drop(
      f_test(l$alpha[1, 100, ], l$beta[1, 100, , ], l$sigma[1, 100, ], a = 10)
    )
  )
})

test_that("map_mcmcarray function(s) work on droppable vector", {
  alpha <- mcmcr::mcmcr_example$alpha

  # Convert `alpha` from column vector (2-by-1) to row vector (1-by-2)
  dim(alpha) <- c(2, 400, 1, 2)

  f_test <- function(alpha) {
    if (is.null(dim(alpha))) stop("Alpha has no dimension")
    alpha %*% matrix(1:10, nrow = 2, ncol = 5)
  }

  test_value <- pmap_mcmcarray(list(alpha), f_test)

  expect_equal(
    object = drop(unclass(subset(test_value, chains = 1, iters = 100))),
    expected = drop(f_test(alpha[1, 100, , , drop = FALSE]))
  )
})
