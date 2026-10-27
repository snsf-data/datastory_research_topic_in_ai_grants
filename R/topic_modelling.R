#==============================================================================|
#                                ---- Setup ----
#==============================================================================|

# Required packages
library(stm)
library(quanteda)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(job)
library(ggplot2)
library(tidytext)
library(snf.datastory)
library(here)
library(conflicted)
library(yaml)

# Conflict preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("here", "here")

# Custom functions to manipulate objects returned by the `stm` package more
# easily. See the code in `R/tm_utils.R` for documentation.
source("R/tm_utils.R")
params <- read_yaml("params.yaml")

#==============================================================================|
#                             ---- Parameters ----
#
#==============================================================================|
# Parameters influencing the behaviour of the code in this script

# Controls whether topic models should be ran again
run_models <- params[["run_models"]]
# Controls whether evaluation of topic models should be performed again
eval_models <- params[["eval_models"]]
# The period of call decision year covered in the AI-related grants dataset
period <- c(params[["period_start"]], params[["period_end"]])
# The size of the topic models to build
model_steps <- params[["model_size_step"]]
model_sizes <- seq(
  params[["model_size_min"]],
  params[["model_size_max"]],
  model_steps
)


#==============================================================================|
#                         ---- Read & prepare data ----
#==============================================================================|

# Read the data prepared in the script `R/get_ai_related_grants.R`
dat <- read_csv(here("data/ai_related_grants_2011_2025.csv"))

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


#==============================================================================|
#                            ---- Topic models ----
#==============================================================================|

## Build models ---------------------------------------------------------------

# Folder where to save the models
model_folder <- "models"
# Base name used when saving topic models
model_base_name <- paste0("snsf_grants_ai_", min(period), "_", max(period))
# Information about all models size used add to file with models diagnostic
k_range <- paste0("_k_", min(model_sizes), "_to_", max(model_sizes))
# Name of the file with models diagnostic
evaluated_models_name <- here(
  model_folder,
  paste0("evaluated_models_", model_base_name, k_range, ".rds")
)


#                              ------------------
#                     /!\ NOTE ON BUILDING TOPIC MODELS /!\
#
# When a large number of topic is modelled, building a topic model can take
# some time. In order to save time, several models with different number of
# topics are build in parallel using background jobs.
#
# Be aware that running all models (as set by default in `model_sizes`) might
# be too demanding for your computer. It is advised to try with partitioning
# the computation using the `parts` parameter (see below). For example,
# setting `parts` to 6 means that only 1/6 of all the models will be build
# in parallel. It is thus the responsibility of the user to run all the parts
# one after each other. The `part` (singular) parameter can be used for that.
#
# Each topic model created is saved as an .rds file.
#                              ------------------

# Create topic models with different number of topics by walking across all the
# sizes in `model_sizes`.
if (run_models) {
  parts <- 3
  part <- NULL
  walk(
    seq(model_sizes[0 + part], max(model_sizes), parts * model_steps),
    \(k) {
      # To make the computation faster, we run each model computation as jobs
      # performed in different sessions (i.e. background jobs).
      job(import = c(dfm, k, model_sizes, model_base_name, model_folder), {
        # Creating the topic model
        tm <- stm(
          dfm,
          # The number of topics
          K = k,
          # Setting `CallDecisionYear` as a factor covariate avoid the
          # issue that it is treated as a numeric covariate that varies
          # linearly (which is not what we want).
          prevalence = ~ factor(CallDecisionYear),
          # To see the progress of the computation
          verbose = TRUE
        )
        # Append 0s to the model size in the model file name so when they are
        # sorted the order follow model size. In that case, 005 will appear
        # before 100.
        name_to_append <- paste0(
          "_k_",
          paste0(rep("0", nchar(max(model_sizes)) - nchar(k)), collapse = ""),
          k
        )
        # Create the name of the model just created
        model_name <- paste0(model_base_name, name_to_append, ".rds")
        # Save the model just created
        write_rds(tm, here(model_folder, model_name))
      })
    }
  )
}

# We load all the topic models created into the session
tm_models <- load_topic_models_local(
  path = here(model_folder),
  base_name = model_base_name,
  dfm = dfm
)


## Model diagnostics -----------------------------------------------------------

# Evaluate the models or load the results of already evaluated models.
# The evaluation is based on four metrics:
#    - Exclusivity: Inform on the extent to which terms in the topics are
#      frequent and exclusive to the topics.
#    - Semantic coherence: Inform on the extent to which words in the topics are
#      semantically coherent (words which are most likely under a topic should
#      co-occur within the same document).
#    - Held-out likelihood: Inform on the capacity of the model to predict
#      hold-out part of the dataset using the document completion method (see
#      `?stm::eval.heldout()` for more details).
#    - Residuals: Inform on the multinomial dispersions of the model (see
#      `?stm::checkResiduals()` for more details).

if (eval_models) {
  tm_models_evaluated <- eval_stm_model(
    models = tm_models,
    dfm = dfm,
    n_held_out = 100L,
    file = evaluated_models_name
  )
} else {
  tm_models_evaluated <- read_rds(evaluated_models_name)
}


## Analyse diagnostics --------------------------------------------------------

# Averaging each diagnostic metric for each number of topic
tm_models_eval_summary <- tm_models_evaluated |>
  summarise(
    heldout_likelihood = map_dbl(heldout_likelihood, mean),
    exclusivity = map_dbl(exclusivity, mean),
    semantic_coherence = map_dbl(semantic_coherence, mean),
    residuals = map_dbl(residuals, "dispersion"),
    .by = k
  ) |>
  # Normalize the metrics per model. The approach is for each model to take the
  # difference between the metric's value and the best values across all models
  # (the best may be the maximum or the minimum, depending on the metric), and
  # then divide by the difference between the best and worst values across all
  # models. The results is a distance between 0 and 1 relative to the best
  # score (0 being the best and 1 the worst).
  mutate(
    max_sem = max(semantic_coherence),
    max_excl = max(exclusivity),
    max_res = max(residuals),
    max_he = max(heldout_likelihood),
    min_sem = min(semantic_coherence),
    min_excl = min(exclusivity),
    min_he = min(heldout_likelihood),
    min_res = min(residuals),
    sem_norm = ((max_sem - semantic_coherence) / (max_sem - min_sem)),
    excl_norm = ((max_excl - exclusivity) / (max_excl - min_excl)),
    he_norm = ((min_he - heldout_likelihood) / (max_he - min_he)),
    res_norm = ((min_res - residuals) / (max_res - min_res))
  ) |>
  # Compute the Euclidean distance between the normalized scores
  mutate(
    euclidean_dist = sqrt(sem_norm^2 + excl_norm^2 + he_norm^2 + res_norm^2),
    .by = k
  ) |>
  # Define which model is the best (i.e. with the shortest Euclidean distance)
  mutate(is_best = euclidean_dist == min(euclidean_dist)) |>
  # Remove columns not necessary anymore
  select(!starts_with(c("max", "min")) & !ends_with("norm"))

# Visualize the models diagnostics
tm_models_eval_summary |>
  pivot_longer(
    !c(k, euclidean_dist, is_best),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(metric = str_to_sentence(str_replace(metric, "_", " "))) |>
  ggplot() +
  aes(k, value, group = metric, color = "") +
  geom_vline(
    aes(xintercept = tm_models_eval_summary$k[tm_models_eval_summary$is_best]),
    linewidth = 5,
    color = datastory_grays[1],
    alpha = 0.5
  ) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  scale_color_datastory_1() +
  scale_size_identity() +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    x = "K (number of topics)",
    title = "Model diagnostics by number of topics"
  ) +
  get_datastory_theme(title_axis = "x", gridline_axis = "y")

## Extract top terms from best model -------------------------------------------

# Get the number of topics in the best model
best_k <- tm_models_eval_summary[["k"]][tm_models_eval_summary[["is_best"]]]
# Format the name of the best model
best_model_name <- paste0(
  "k_",
  paste0(rep("0", nchar(max(model_sizes)) - nchar(best_k)), collapse = ""),
  best_k
)
# Subset `tm_models` to get only the best model
best_tm_model <- tm_models[[best_model_name]]

# Save the top terms in long format with the weights so it can be read by an LLM
if (params[["write_top_terms"]]) {
  # Extracting and saving the top terms. The weight are included to the string
  # with the term to guide the LLM used to label the topics.
  get_stm_top_terms(
    best_tm_model,
    n = 10L,
    format = "long",
    score = "prob"
  ) |>
    # Extract the probability (beta) that each word belongs to a topic the
    # `tidytext::tidy()` function .
    left_join(tidy(best_tm_model, "beta"), by = join_by(topic, term)) |>
    # Combine the terms and their weights in a single string
    mutate(term = paste0(term, " (", round(beta, 3), ")")) |>
    # Just keep the topic and term string
    select(!beta) |>
    # Save the top terms so they can be passed to an LLM in order to extract a
    # meaningful interpretation of the terms in each topic.
    write_csv(
      here(
        "data",
        paste0("top_terms_", model_base_name, "_", best_model_name, ".csv")
      )
    )
}
