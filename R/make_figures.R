make_mobile_figure_1 <- function(height = NULL) {
  table <- top_terms_prevalence |>
    summarise(Share = mean(gamma), .by = c(category, topic)) |>
    arrange(desc(Share)) |>
    rename_with(str_to_sentence) |>
    relocate(Topic, Category, Share)

  if (params$lang %in% c("fr", "de")) {
    table <- mutate(
      table,
      "new_cat" = translate(Category, fig_labels),
      .before = Share
    ) |>
      rename_with(
        \(x) {
          paste0(
            translate("Category", fig_labels),
            " (",
            toupper(params$lang),
            ")"
          )
        },
        new_cat
      )
  }

  table |>
    gt() |>
    tab_options(table.font.names = font) |>
    fmt_percent(
      columns = "Share",
      locale = params$lang
    ) |>
    cols_label(
      "Topic" = translate("Topic", fig_labels),
      "Category" = translate("Category", fig_labels),
      "Share" = translate("Share", fig_labels)
    )
}

make_desktop_figure_1 <- function(height = NULL) {
  make_mobile_figure_1()
}

make_mobile_figure_2 <- function(height = 5.5) {
  # Summarize topic data for the plot
  topic_dat <- top_terms_prevalence |>
    # Summarise topic prevalence across all grant texts
    summarise(
      topic_prevalence = mean(gamma),
      .by = c(topic, category)
    ) |>
    #
    mutate(
      cat_prevalence = sum(topic_prevalence),
      .by = category
    ) |>
    mutate(
      topic = fct_reorder(topic, topic_prevalence),
      category = fct_reorder(category, cat_prevalence),
      # fmt: skip
      tooltip = fct_reorder(
        paste0(
          translate("Topic", fig_labels), ": ", topic, "<br>",
          translate("Prevalence", fig_labels), ": ",
          print_num(round(topic_prevalence * 100, 1), params$lang), "%<br>",
          translate("Rank across all topics", fig_labels), ": ",
          as.numeric(fct_rev(topic))
        ),
        as.numeric(topic)
      ),
      data_id = paste0("topic", topic)
    )

  # Summarize (topic) category data for the plot
  category_dat <- topic_dat |>
    distinct(cat_prevalence, category) |>
    mutate(
      category = fct_reorder(category, cat_prevalence),
      # fmt: skip
      tooltip = paste0(
        translate("Category", fig_labels), ": ", category, "<br>",
        translate("Prevalence", fig_labels), ": ",
        print_num(round(cat_prevalence * 100, 1), params$lang), "%"
      ),
      data_id = paste0("cat", as.numeric(category))
    )

  # Prepare the plot data
  plot <- ggplot(topic_dat) +
    aes(
      x = topic_prevalence,
      y = category,
      fill = category,
      tooltip = tooltip,
      data_id = data_id
    ) +
    # Large bars representing the prevalence of categories
    geom_col_interactive(
      aes(x = cat_prevalence, y = category),
      dat = category_dat,
      alpha = 0.5,
      show.legend = FALSE
    ) +
    # Smaller bars representing the prevalence of topic (nested within the
    # category bars).
    geom_col_interactive(
      alpha = 0.75,
      position = position_dodge2(preserve = "single", padding = 0.3),
      width = 0.9,
      show.legend = FALSE
    ) +
    # Topic label next to nested bars
    geom_text(
      aes(
        label = topic,
        x = topic_prevalence + 0.001,
        color = if_else(category == "AI", "black", "black")
      ),
      position = position_dodge2(
        preserve = "single",
        width = 0.9,
        padding = 0.3
      ),
      hjust = 0,
      size = 2
    ) +
    scale_x_continuous(
      n.breaks = 11,
      expand = expansion(c(0, 0.05)),
      labels = \(x) scales::percent(x, accuracy = 1)
    ) +
    scale_y_discrete(labels = \(x) str_wrap(x, 16)) +
    scale_fill_datastory(reverse = TRUE) +
    scale_color_identity() +
    labs(x = translate("Topic and category prevalence (%)", fig_labels)) +
    get_datastory_theme(
      title_axis = "x",
      text_axis = c("x", "y"),
      gridline_axis = "x",
      legend_key_size = c(3, 3)
    ) +
    theme(
      axis.text.y = element_text(hjust = 0.5, lineheight = 1.1, size = 10),
      axis.text.x = element_text(size = 10)
    )

  plot
}

make_desktop_figure_2 <- function(height = 5.5) {
  make_ggiraph(make_mobile_figure_2(height), sw = 0, h = height)
}

make_mobile_figure_3 <- function(height = 3.75) {
  plot <- top_terms_prevalence |>
    summarise(
      gamma = sum(gamma),
      .by = c(grant, CallDecisionYear, category)
    ) |>
    slice_max(gamma, n = 1, by = grant) |>
    summarise(
      is_core_ai = sum(category == "Fundamental AI research"),
      is_ai = n(),
      .by = CallDecisionYear
    ) |>
    pivot_longer(c(is_core_ai, is_ai), names_to = "type", values_to = "n") |>
    mutate(
      # fmt: skip
      tooltip = paste0(
        translate("Call decision year", fig_labels),
        ": ", CallDecisionYear, "<br>",
        translate("Number of AI-related grants", fig_labels),
        ": ", n[type == "is_ai"], "<br>",
        translate("Number of fundamental AI research grants", fig_labels),
        ": ", n[type != "is_ai"], "<br>"
      ),
      data_id = CallDecisionYear,
      type = if_else(type == "is_ai", "All AI-related", "Fundamental AI"),
      .by = CallDecisionYear
    ) |>
    arrange(CallDecisionYear) |>
    ggplot() +
    aes(x = CallDecisionYear, y = n, fill = type, color = type) +
    geom_area(
      data = \(x) filter(x, grepl("^All", type)),
      alpha = 0.65,
      color = NA,
    ) +
    geom_area(
      data = \(x) filter(x, !grepl("^All", type)),
      alpha = 0.65,
      color = NA,
    ) +
    geom_line(show.legend = FALSE) +
    geom_point_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(2011, 2025, 2),
      expand = expansion(c(0.01, 0.05))
    ) +
    scale_fill_datastory(
      labels = \(x) {
        recode_values(
          x,
          "All AI-related" ~ translate("All AI-related", fig_labels),
          "Fundamental AI" ~ translate("Fundamental AI", fig_labels)
        )
      }
    ) +
    scale_color_datastory() +
    labs(y = translate("Number of grants", fig_labels, params[["lang"]])) +
    get_datastory_theme(title_axis = "y") +
    theme(
      axis.title.y = element_text(lineheight = 1.15, size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.text = element_text(size = 10)
    )

  plot
}

make_desktop_figure_3 <- function(height = 3.75) {
  make_ggiraph(make_mobile_figure_3(height), h = height)
}

make_mobile_figure_4 <- function(height = 3.5) {
  # Params to create the figure
  period_highlight <- "2011–2013"
  n_topics <- n_distinct(year_evol_dat$topic)
  n_highlight <- 7

  # The topics to highlight in the figure
  topic_highlight <- year_evol_dat |>
    filter(rank <= n_highlight, period == period_highlight) |>
    pull(topic)

  # The data to plot for the topics to highlight
  bump_colored_dat <- year_evol_dat |>
    filter(topic %in% topic_highlight) |>
    mutate(
      # fmt: skip
      tooltip_point = paste0(
        translate("Topic", fig_labels), ": ", topic, "<br>",
        translate("Period", fig_labels), ": ", period, "<br>",
        translate("Rank", fig_labels), ": ", rank, "<br>",
        translate("Prevalence", fig_labels), ": ",
        print_num(round(prevalence * 100, 1), params$lang), "%"
      ),
      data_id_point = row_number()
    )

  # The data to plot for the topic labels on the left of the figure
  label_dat <- filter(
    year_evol_dat,
    topic %in% topic_highlight,
    period == max(levels(period))
  )

  x <- seq(0, 1, length.out = 100)[-1]
  smooth <- cbind(x, scales::rescale(1 / (1 + exp(-(x * 35 - 17.5)))))

  # Make the plot
  ggplot(year_evol_dat) +
    aes(x = period, y = rank, group = topic) +
    geom_path(
      stat = "connect",
      linewidth = 1.5,
      alpha = 0.2,
      connection = smooth
    ) +
    geom_path(
      data = bump_colored_dat,
      aes(period, rank, color = topic),
      stat = "connect",
      connection = smooth,
      linewidth = 1.5,
      show.legend = FALSE,
    ) +
    geom_point_interactive(
      data = bump_colored_dat,
      aes(
        period,
        rank,
        group = topic,
        fill = topic,
        tooltip = tooltip_point,
        data_id = data_id_point
      ),
      size = 2,
      shape = 21,
      colour = "white",
      stroke = 1,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    geom_text(
      dat = \(x) filter(x, rank <= n_highlight, period == period_highlight),
      aes(label = topic, y = rank, x = as.numeric(period) - 0.1),
      size = 2.5,
      hjust = 1
    ) +
    labs(y = paste0(translate("Rank", fig_labels), "\n")) +
    scale_y_reverse(breaks = seq(1, n_topics), position = "right") +
    scale_x_discrete(expand = expansion(mult = c(0.4, 0.015))) +
    scale_color_datastory(guide = guide_legend(ncol = 2)) +
    scale_fill_datastory(guide = guide_legend(ncol = 2)) +
    get_datastory_theme(title_axis = "y") +
    theme(
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 10)
    )
}

make_desktop_figure_4 <- function(height = 3.5) {
  make_ggiraph(make_mobile_figure_4(height), h = height)
}

make_mobile_figure_5 <- function(height = 3.5) {
  # Params to create the figure
  period_highlight <- "2023–2025"
  n_topics <- n_distinct(year_evol_dat$topic)
  n_highlight <- 7

  # The topics to highlight in the figure
  topic_highlight <- year_evol_dat |>
    filter(rank <= n_highlight, period == period_highlight) |>
    pull(topic)

  # The data to plot for the topics to highlight
  bump_colored_dat <- year_evol_dat |>
    filter(topic %in% topic_highlight) |>
    mutate(
      # fmt: skip
      tooltip_point = paste0(
        translate("Topic", fig_labels), ": ", topic, "<br>",
        translate("Period", fig_labels), ": ", period, "<br>",
        translate("Rank", fig_labels), ": ", rank, "<br>",
        translate("Prevalence", fig_labels), ": ",
        print_num(round(prevalence * 100, 1), params$lang), "%"
      ),
      data_id_point = row_number()
    )

  # The data to plot for the topic labels on the right of the figure
  label_dat <- filter(
    year_evol_dat,
    topic %in% topic_highlight,
    period == max(levels(period))
  )

  x <- seq(0, 1, length.out = 100)[-1]
  smooth <- cbind(x, scales::rescale(1 / (1 + exp(-(x * 35 - 17.5)))))

  # Make the plot
  ggplot(year_evol_dat) +
    aes(x = period, y = rank, group = topic) +
    geom_path(
      stat = "connect",
      linewidth = 1.5,
      alpha = 0.2,
      connection = smooth
    ) +
    geom_path(
      data = bump_colored_dat,
      aes(period, rank, color = topic),
      stat = "connect",
      connection = smooth,
      linewidth = 1.5,
      show.legend = FALSE,
    ) +
    geom_point_interactive(
      data = bump_colored_dat,
      aes(
        period,
        rank,
        group = topic,
        fill = topic,
        tooltip = tooltip_point,
        data_id = data_id_point
      ),
      size = 2,
      shape = 21,
      colour = "white",
      stroke = 1,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    geom_text(
      dat = \(x) filter(x, rank <= n_highlight, period == period_highlight),
      aes(label = topic, y = rank, x = as.numeric(period) + 0.1),
      size = 2.5,
      hjust = 0
    ) +
    labs(y = paste0(translate("Rank", fig_labels), "\n")) +
    scale_y_reverse(breaks = seq(1, n_topics)) +
    scale_x_discrete(expand = expansion(mult = c(0.015, 0.4))) +
    scale_color_datastory(guide = guide_legend(ncol = 2)) +
    scale_fill_datastory(guide = guide_legend(ncol = 2)) +
    get_datastory_theme(title_axis = "y") +
    theme(
      axis.title.y = element_text(size = 10),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 10),
      plot.title.position = "plot"
    )
}

make_desktop_figure_5 <- function(height = 3.5) {
  make_ggiraph(make_mobile_figure_5(height), h = height)
}
