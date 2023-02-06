test_that("LOO methods dispatch correctly", {
  loglik_list <-
    list(
      bad = structure(
        array(dnorm(runif(20000), sd = 3), dim = c(2, 1000, 10)),
        class = "mcmcarray"
      ),

      good = structure(
        array(dnorm(runif(20000)), dim = c(2, 1000, 10)),
        class = "mcmcarray"
      )
    )

  lcs <- expect_no_error(loo_compare_summary(loglik_list, r_eff = "auto")[[1]])

  expect_equal(lcs$model, c("good", "bad"))

  expect_no_error(loo_compare_summary_print(lcs))
})

test_that("LOO autoplot method runs (output not checked)", {
  expect_no_error(
    ggplot2::autoplot(loo::loo(loo::example_loglik_array(), r_eff = NA))
  )
})
