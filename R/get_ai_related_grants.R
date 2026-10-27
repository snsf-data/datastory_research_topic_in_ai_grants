#==============================================================================|
#                                ---- Setup ----
#==============================================================================|

# Default packages
library(cld2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(here)
library(conflicted)
library(yaml)

# Conflict preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("here", "here")

params <- read_yaml("params.yaml")

# The period covered in the data story (based on `CallDecisionYearReporting`)
period <- params[["period_start"]]:params[["period_end"]]
download_grants <- params[["download_grants"]]

#==============================================================================|
#                         ---- Read & prepare data ----
#==============================================================================|

## Keywords --------------------------------------------------------------------

# Read keywords from the SNSF
snsf_keywords <- read_csv(here("data/snsf_ai_keywords.csv"))

# Get for each keyword the position of other keywords they are embedded in (see
# before for more details).
snsf_embedded <- snsf_keywords$keyword |>
  map(
    \(x) {
      which(str_detect(snsf_keywords$keyword, x) & x != snsf_keywords$keyword)
    }
  )

# Create a vector of keyword with regex of stuff to ignore before/after some
# keywords embedded in others.
snsf_excl_embedded <- map(
  seq_along(snsf_embedded),
  \(x) {
    if (length(snsf_embedded[[x]]) == 0) {
      txt <- snsf_keywords$keyword[x]
    } else {
      emb <- snsf_keywords$keyword[unlist(snsf_embedded[x])]
      loc <- stringr::str_locate(emb, snsf_keywords$keyword[x])
      txt <- paste0(
        "(?<!(",
        paste0(stringr::str_sub(emb, 1, loc[, "start"] - 1), collapse = "|"),
        "))",
        snsf_keywords$keyword[x],
        "(?!(",
        paste0(stringr::str_sub(emb, loc[, "end"] + 1), collapse = "|"),
        "))"
      ) |>
        str_replace(fixed("(?!(|))"), "")
    }
    txt
  }
) |>
  unlist()

# Add a new column with the new keywords that include regex to deal with
# embedded keywords.
snsf_keywords <- snsf_keywords |>
  mutate(keyword_excl_embedded = snsf_excl_embedded) |>
  add_row(
    keyword = c(
      "large language model",
      "generative ai"
    ),
    Ambiguous = rep(FALSE, 2),
    keyword_excl_embedded = c(
      "(large language model)|(\\bllm[s]?\\b)",
      "(generative ai\\b)|(\\bgen\\s?ai\\b)"
    )
  ) |>
  arrange(keyword)

## Application -----------------------------------------------------------------

if (download_grants) {
  # Read the grants data (with abstracts) from the SNSF Data Portal
  snsf_grants <- read_csv2(
    "https://data.snf.ch/datasets/grants_with_abstracts.csv",
    col_select = c(
      GrantNumber,
      CallDecisionYear,
      Title,
      TitleEnglish,
      Abstract,
      Keywords,
      EffectiveGrantStartDate,
      FundingInstrumentLevel1
    ),
    col_types = cols(
      GrantNumber = col_integer(),
      CallDecisionYear = col_integer(),
      EffectiveGrantStartDate = col_datetime(),
      .default = col_character()
    )
  ) |>
    # Keep only grants decided between 2011 and 2025 in Projects and Careers
    # instruments.
    filter(
      CallDecisionYear %in% period,
      FundingInstrumentLevel1 %in% c("Projects", "Careers"),
      EffectiveGrantStartDate <= "2026-06-30"
    ) |>
    # Check in what language are the abstracts and titles written
    mutate(
      lang_abs = cld2::detect_language(Abstract),
      lang_title = cld2::detect_language(Title),
      lang_title_en = cld2::detect_language(TitleEnglish)
    ) |>
    # If the abstract and any of the two titles are both in English, then we
    # keep the grant for later analysis.
    filter(lang_abs == "en", (lang_title == "en" | lang_title_en == "en")) |>
    mutate(Title = if_else(lang_title == "en", Title, TitleEnglish)) |>
    # Remove unnecessary variables
    select(!c(starts_with("lang_"), TitleEnglish)) |>
    # Collapse the texts into a single variable (title, abstract, and keywords)
    mutate(
      grant_text = pmap_chr(
        list(Title, Abstract, Keywords),
        \(x, y, z) {
          # After combining the different textual parts into a single string, we
          # standardized the string by setting it to lower case, replacing dash
          # characters to space, and replacing all spaces with a single space.
          paste0(c(x, y, z), collapse = ". ") |>
            str_to_lower() |>
            str_replace_all("[‒–—-]", " ") |>
            str_squish()
        }
      )
    )

  # Separate the file in 3 parts so it can be pushed to Github
  lapply(
    0:2,
    \(x) {
      write_csv(
        filter(
          snsf_grants,
          row_number() > nrow(snsf_grants) * (x / 3),
          row_number() <= nrow(snsf_grants) * ((x + 1) / 3)
        ),
        here(
          "data",
          paste0(
            "snsf_grants_PF_CAR_",
            params[["period_start"]],
            "_",
            params[["period_end"]],
            paste0("_PART", x + 1, ".csv")
          )
        )
      )
      invisible()
    }
  )
} else {
  snsf_grants <- lapply(
    1:3,
    \(x) {
      read_csv(
        here(
          "data",
          paste0(
            "snsf_grants_PF_CAR_",
            params[["period_start"]],
            "_",
            params[["period_end"]],
            paste0("_part", x, ".csv")
          )
        )
      )
    }
  ) |>
    purrr::list_rbind()
}


#==============================================================================|
#                      ---- Filter AI-related grants ----
#==============================================================================|

# Counting the occurrence of AI keywords in the text of each grant. We go over
# each AI keyword (with `map()`) and check for each grants if it is detected in
# its text (returning `TRUE` or `FALSE` when the keyword is detected or not,
# respectively), and sum this over all keywords for each grant. The sum of
# logical value is automatically coerced to 0 (FALSE) and 1 (TRUE). The returned
# vector is the number of unique keywords in the grants in `snsf_grants`.
ai_keywords_in_grant_text_unique_count <- map(
  snsf_keywords$keyword_excl_embedded,
  \(x) str_detect(snsf_grants$grant_text, x),
  .progress = TRUE
) |>
  reduce(`+`)

# Extracting from each grant text the unique occurrence of AI keywords. We
# proceed as above, except that for each grant the returned vector is a logical
# vector indicating which keyword was detected (or not) in the grant. We then
# use that vector for each grant to index the keywords detected in the list of
# AI keywords and concatenate them with ";".
unique_ai_keywords_in_grant_text <- map(
  snsf_grants$grant_text,
  \(x) str_detect(x, snsf_keywords$keyword_excl_embedded),
  .progress = TRUE
) |>
  map_chr(\(x) paste0(sort(snsf_keywords$keyword[x]), collapse = ";"))

# Add new variables to the collected grants: the number of unique keywords
# identified, whether it is a false alarm (i.e. if the only keyword detected
# is an ambiguous keyword "neural network"), and the list of keywords.
grants_with_ai_kwd_count <- snsf_grants |>
  mutate(
    # Unique counts AI keywords in title/abstract/keywords
    n_unique_ai_keywords = ai_keywords_in_grant_text_unique_count,
    # List of unique AI keywords
    unique_ai_keywords = unique_ai_keywords_in_grant_text
  ) |>
  mutate(
    # When there is only a single unique keyword detected, check if it is an
    # "ambiguous" keyword. If yes, this is a false alarm.
    is_false_alarm = str_detect(
      unique_ai_keywords,
      paste0(snsf_keywords$keyword[snsf_keywords$Ambiguous], collapse = "|")
    ) &
      n_unique_ai_keywords == 1
  )

# Filter the grants to keep only those AI-related (excluding false alarm) and
# saving the data for topic modelling analysis.
grants_with_ai_kwd_count |>
  filter(!is_false_alarm, n_unique_ai_keywords > 0) |>
  select(!c(is_false_alarm)) |>
  write_csv(here("data/ai_related_grants_2011_2025.csv"))