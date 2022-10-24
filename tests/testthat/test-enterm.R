test_that("enterm works for scalar", {
  expect_equal(
    enterm(1),
    tibble::tibble(.term = term::term("x"), .value = 1)
  )
})

test_that("enterm works for vector", {
  expect_equal(
    enterm(1:5),
    tibble::tibble(.term = term::term(sprintf("x[%s]", 1:5)), .value = 1:5)
  )
})

test_that("enterm works for matrix", {
  expect_equal(
    enterm(diag(2)),
    tibble::tibble(
      .term = term::term(sprintf("x[%s,%s]", c(1, 2, 1, 2), c(1, 1, 2, 2))),
      .value = c(1, 0, 0, 1)
    )
  )
})

test_that("enterm works for array", {
  expected_term <-
    term::term(sprintf(
      "x[%s,%s,%s]",
      rep(1:4, 6),
      rep(1:3, each = 4, times = 2),
      rep(1:2, each = 12)
    ))

  expect_equal(
    enterm(array(1:24, dim = c(4, 3, 2))),
    tibble::tibble(.term = expected_term, .value = 1:24)
  )
})

test_that("enterm works for different name", {
  expect_equal(
    enterm(1:5, "y"),
    tibble::tibble(.term = term::term(sprintf("y[%s]", 1:5)), .value = 1:5)
  )
})

test_that("enterm works for different .value", {
  expect_equal(
    enterm(1:5, .value = ".truth"),
    tibble::tibble(.term = term::term(sprintf("x[%s]", 1:5)), .truth = 1:5)
  )
})
