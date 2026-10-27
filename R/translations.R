if (!file.exists(here::here("data", "figure_translations.csv"))) {
  
  stop(
    paste0(
      "Translations of text data in figures text cannot be generated. The ",
      "file 'data/figure_translations.csv' cannot be found."
    )
  )
  
}

# ------------------------------------------------------------------------------
# Function to flexibly translate the terms in the figures, based on the language
# context indicated by `params$lang`.
#
# "terms" a character vector with the terms (in English) to translate.
# "src"   the list to search for the translation.
# "to"    the language into which "terms" should be translated. The default is
#         to use the language context set by `params$lang`.
#
# If at least on term cannot be found in the "src", the function throw an error.
# This function is designed to work easily with "src = fig_labels" and
# "to = params$lang".
# ------------------------------------------------------------------------------
translate <- function(terms, src, to = params$lang) {
  term_without_par <- stringr::str_replace_all(terms, "[\\(\\)]", "_par_")
  ok_terms <- term_without_par %in% names(src)
  
  if (!all(ok_terms)) {
    stop(
      paste0(
        c(
          "The following term(s) cannot be found in `",
          deparse(substitute(src)), "`: \"",
          paste0(terms[!ok_terms], collapse = "\", \""), "\"."
        )
      )
    )
  }
  
  terms <- purrr::map_chr(
    term_without_par,
    \(x) src[[match(x, names(src))]][[to]]
  )
  
  return(terms)
}

# Create a list with all the terms to be translated. The elements in the list
# are named after the English version of the term. Each element has three
# elements, named "en", "de", and "fr", allowing easily to access any
# translation of a term using the auto-completion with "$".
fig_labels <- readr::read_csv(here::here("data", "figure_translations.csv")) |>
  dplyr::select(!Figure) |>
  dplyr::distinct() |>
  dplyr::group_by(en) |>
  dplyr::group_split() |>
  purrr::map(\(x) list(en = x[["en"]], de = x[["de"]], fr = x[["fr"]]))

names(fig_labels) <- purrr::map_chr(fig_labels, \(x) {
  stringr::str_replace_all(x$en, "[\\(\\)]", "_par_")
})
