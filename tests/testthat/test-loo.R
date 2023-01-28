test_that("LOO methods dispatch correctly", {
  x <-
    structure(
      array(dnorm(runif(20000)), dim = c(2, 1000, 10)),
      class = "mcmcarray"
    )

  x2 <-
    structure(
      array(dnorm(runif(20000), sd = 3), dim = c(2, 1000, 10)),
      class = "mcmcarray"
    )

  # Best model ('good') should be at the top of table
  testthat::expect_equal(
    loo_compare_summary(list(good = x, bad = x2))$`Chain 1`$model,
    c("good", "bad")
  )

})
