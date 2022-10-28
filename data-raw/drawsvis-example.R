drawsvis_example <-
  mcmcrutils::draws_example %>%
  lapply(dplyr::group_by, .data$.term) %>%
  lapply(mcmcrutils::drawsvis, "trace")

usethis::use_data(drawsvis_example, overwrite = TRUE)
