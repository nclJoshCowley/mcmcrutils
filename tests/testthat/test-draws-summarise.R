expected_mean <- as.vector(mcmcr::estimates(mcmcr::mcmcr_example$beta, mean))


test_that("draws_summarise, convetional input", {
  x <- draws_example$beta

  expect_equal(NROW(draws_summarise(x)), 8) # 4 terms, across 2 chains

  expect_equal(
    draws_summarise(x, collapse_chains = TRUE)$.value, # (Chains 1 and 2)
    expected_mean, tolerance = 0.01
  )

  expect_equal(
    draws_summarise(dplyr::ungroup(x))$.value[1:4], # (Chain 1)
    expected_mean, tolerance = 0.01
  )

  expect_equal(
    draws_summarise(x)$.value[5:8], # (Chain 2)
    expected_mean, tolerance = 0.01
  )
})


test_that("draws_summarise, extra (ungrouped) column", {
  x <-
    dplyr::mutate(draws_example$beta, label = letters[dplyr::cur_group_id()])

  expect_equal(
    draws_summarise(x, collapse_chains = TRUE)$.value,
    expected_mean, tolerance = 0.01
  )
})


test_that("draws_summarise, extra (ungrouped) column", {
  x <-
    dplyr::group_by(draws_example$beta, label = letters[dplyr::cur_group_id()])

  out <- draws_summarise(x, collapse_chains = TRUE)

  expect_true("label" %in% names(out), label = "Extra column in output")

  expect_equal(out$.value, expected_mean, tolerance = 0.01)
})
