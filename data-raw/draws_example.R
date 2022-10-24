draws_example <- gather_draws_by_term(mcmcr::mcmcr_example)
usethis::use_data(draws_example, overwrite = TRUE)
