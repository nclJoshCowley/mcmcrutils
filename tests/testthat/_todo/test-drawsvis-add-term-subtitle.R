test_add_term_subtitle <- function(...) {
  drawsvis_example$beta %>%
    dplyr::filter(.data$.chain == 1) %>%
    mcmcrutils::drawsvis_add_term_subtitle(...) %>%
    dplyr::pull(.data$.plot) %>%
    patchwork::wrap_plots(nrow = 2, byrow = FALSE)
}

# Ambiguous second argument (FAIL)
test_add_term_subtitle(letters[1:2])

# Missing second argument, (dropped)
test_add_term_subtitle(letters[1:2], )

# NULL second argument, (asis)
test_add_term_subtitle(letters[1:2], NULL)

# Missing first argument
test_add_term_subtitle(, LETTERS[1:2])

# Function update of second argument
test_add_term_subtitle(letters[1:2], sqrt)

# Formula update of second argument
test_add_term_subtitle(letters[1:2], ~ sprintf("K = %s", .x))
