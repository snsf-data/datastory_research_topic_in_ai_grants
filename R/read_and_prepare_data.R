#==============================================================================|
#                              ---- Read data ----
#==============================================================================|

# Read the data prepared in the script `R/get_ai_related_grants.R`
dat <- read_csv(here("data/ai_related_grants_2011_2025.csv"))

# Load the best structural topic model based on the analysis conducted in
# "R/topic_modelling.R".
stm_model <- load_topic_models_local(
  path = here("models"),
  base_name = "snsf_grants_ai_2011_2025",
  range = c(25L, 25L),
  dfm = dfm
)[["k_025"]]

# Load the csv with the topic which were labelled with the help of an LLM using
# the `top_terms`, and then validated and modified accordingly by the authors
# of the data story.
labelled_topics <- read_csv(
  here(
    "data/top_terms_snsf_grants_ai_2011_2025_k_025_with_labels_validated.csv"
  )
)

#==============================================================================|
#                            ---- Prepare data ----
#==============================================================================|

# Turn the data frame to a corpus with the "grant_text" variable as the text
# field.
corpus_dat <- corpus(
  dat,
  docid_field = "GrantNumber",
  text_field = "grant_text"
)

# Tokenize the text field in the corpus with a custom tokenizer (mix of
# quanteda options + some in-house cleaning).
tokens_dat <- tokens_snsf(corpus_dat)
# Create a document-feature matrix with the SNSF grant texts
dfm <- dfm(tokens_dat)

# Extracting the 10 top terms per topic based on the terms probability
top_terms <- get_stm_top_terms(
  stm_model,
  n = 10L,
  format = "wide",
  score = "prob"
)

# We extract and tidy the terms prevalence (gamma values) from the topic model
top_terms_prevalence <- tidy(stm_model, "gamma") |>
  left_join(labelled_topics, by = join_by(topic)) |>
  mutate(
    grant = dfm@docvars$docid_[document],
    .after = document,
    topic = label,
    category = label_category,
    CallDecisionYear = dfm@docvars$CallDecisionYear[document],
  ) |>
  select(!c(document, label, label_category))


# Extract the estimated effect (covariates) from the structural topic model. The
# estimated effect is "CallDecisionYear". This means that the model was create
# with call decision year as a covariate allowing the topics to have different
# prevalence over time.
stm_year_effects <- tidy_stm_effects(
  stm_model,
  cov = "CallDecisionYear",
  dfm = dfm,
  nsims = 100
) |>
  left_join(labelled_topics, by = join_by(topic)) |>
  mutate(topic = label, category = label_category) |>
  select(category, topic, CallDecisionYear, prevalence = mean)

# Prepare the data to plot the absolute ranking of the topics over year, based
# on their prevalence.
year_evol_dat <- stm_year_effects |>
  mutate(
    # Create time periods of three years and make it a factor
    period = fct(
      case_when(
        CallDecisionYear %in% 2011:2013 ~ "2011–2013",
        CallDecisionYear %in% 2014:2016 ~ "2014–2016",
        CallDecisionYear %in% 2017:2019 ~ "2017–2019",
        CallDecisionYear %in% 2020:2022 ~ "2020–2022",
        CallDecisionYear %in% 2023:2025 ~ "2023–2025"
      ),
      levels = c(
        "2011–2013",
        "2014–2016",
        "2017–2019",
        "2020–2022",
        "2023–2025"
      )
    ),
    topic = fct(as.character(topic))
  ) |>
  # Summarise the prevalence for each topic and time period
  summarise(prevalence = mean(prevalence), .by = c(topic, period)) |>
  # Rank the topic year by year (the use of "-mean" is to get descending rank
  # instead of ascending).
  mutate(rank = rank(-prevalence), .by = period)
