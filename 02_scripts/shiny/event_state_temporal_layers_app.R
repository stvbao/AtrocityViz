# event_state_temporal_layers_app.R
# Interactive Shiny app: one page per month, click through months
# Each month shows a layered bipartite (events above states) with conflict arcs

library(shiny)
library(tidyverse)
library(readxl)
library(igraph)
library(tidygraph)
library(ggraph)

script_context_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_match <- grep(paste0("^", file_arg), args)
  if (length(file_match) > 0) {
    return(normalizePath(sub(file_arg, "", args[file_match[1]]), mustWork = FALSE))
  }

  frames <- sys.frames()
  if (length(frames) > 0) {
    frame_files <- vapply(
      frames,
      function(frame) {
        if (!is.null(frame$ofile)) frame$ofile else NA_character_
      },
      character(1)
    )
    frame_files <- frame_files[!is.na(frame_files)]
    if (length(frame_files) > 0) {
      return(normalizePath(tail(frame_files, 1), mustWork = FALSE))
    }
  }

  normalizePath(getwd(), mustWork = FALSE)
}

project_root <- local({
  script_dir <- dirname(script_context_path())
  candidates <- unique(c(
    file.path(script_dir, "..", ".."),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "../..")
  ))

  for (candidate in candidates) {
    candidate <- normalizePath(candidate, mustWork = FALSE)
    if (dir.exists(file.path(candidate, "01_data")) &&
        dir.exists(file.path(candidate, "03_results"))) {
      return(candidate)
    }
  }

  normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
})

data_file <- Sys.getenv(
  "ATROCITYVIZ_DATA",
  unset = file.path(project_root, "01_data", "GEDEvent_v25_01_25_12.xlsx")
)

# --- 1. Pre-compute data (runs once at app start) -------------------------

raw <- read_excel(data_file) |>
  mutate(
    date          = as.Date(date_start),
    month_num     = as.numeric(format(date, "%m")),
    month_lab     = format(date, "%B %Y"),
    violence_type = factor(type_of_violence,
                           levels = c(1, 2, 3),
                           labels = c("State-based", "Non-state", "One-sided"))
  )

all_states <- sort(unique(raw$country))

# State positions: sorted A-Z, with A at the top (highest y-index)
state_order <- sort(all_states, decreasing = TRUE) # Z=1 ... A=n_states
n_states <- length(state_order)
state_x <- setNames(seq_len(n_states) * 2, state_order) # 2 units per country

# Conflict linkages (same across all months)
conf_state <- raw |>
  distinct(conflict_new_id, conflict_name, country)

state_pairs <- conf_state |>
  inner_join(conf_state,
    by = "conflict_new_id",
    suffix = c("_a", "_b"), relationship = "many-to-many"
  ) |>
  filter(country_a < country_b) |>
  group_by(country_a, country_b) |>
  summarise(
    weight           = n(),
    shared_conflicts = paste(unique(conflict_name_a), collapse = "; "),
    .groups          = "drop"
  )

conf_edges <- state_pairs |>
  mutate(
    x_from = state_x[country_a],
    x_to   = state_x[country_b],
    y_from = 0,
    y_to   = 0
  )

# Global weight max for consistent linewidth scale
global_weight_max <- max(state_pairs$weight, na.rm = TRUE)

# Pre-compute monthly conflict links (only conflicts active in that month)
monthly_conf_links <- function(month_num) {
  month_conflicts <- raw |>
    filter(month_num == !!month_num) |>
    distinct(conflict_new_id)

  month_conf_state <- conf_state |>
    semi_join(month_conflicts, by = "conflict_new_id")

  month_conf_state |>
    inner_join(month_conf_state,
      by = "conflict_new_id",
      suffix = c("_a", "_b"), relationship = "many-to-many"
    ) |>
    filter(country_a < country_b) |>
    group_by(country_a, country_b) |>
    summarise(
      weight           = n(),
      shared_conflicts = paste(unique(conflict_name_a), collapse = "; "),
      .groups          = "drop"
    ) |>
    mutate(
      x_from = state_x[country_a],
      x_to   = state_x[country_b],
      y_from = 0,
      y_to   = 0
    )
}

# --- 2. Function to build monthly plot -----------------------------------

build_month_plot <- function(month_num, show_all_conflicts = FALSE, highlight_country = NULL) {
  month_raw <- raw |> filter(month_num == !!month_num)

  if (nrow(month_raw) == 0) {
    return(ggplot() +
      theme_void() +
      labs(title = paste("No events in", month.name[month_num])))
  }

  month_label <- unique(month_raw$month_lab)[1]
  n_events <- nrow(month_raw)

  # Event positions: x = state + jitter, y = day of month (1-31 scaled)
  set.seed(42)
  evt_df <- month_raw |>
    distinct(id, .keep_all = TRUE) |>
    mutate(
      day = as.numeric(format(date, "%d")),
      y = state_x[country] + runif(n(), -0.7, 0.7),
      x = as.numeric(day) + 10,
      fatality_cat = cut(
        best,
        breaks = c(-Inf, 10, 25, 100, 1000, Inf),
        labels = c("0-10", "10-25", "25-100", "100-1000", "1000+"),
        right  = TRUE
      )
    )

  # State nodes
  state_df <- tibble(
    country = state_order,
    y       = state_x[state_order],
    x       = 0
  )

  # Conflict links for this month
  pairs_df   <- if (show_all_conflicts) state_pairs else monthly_conf_links(month_num)
  month_conf <- if (show_all_conflicts) conf_edges  else {
    pairs_df |> mutate(x_from = state_x[country_a], x_to = state_x[country_b], y_from = 0, y_to = 0)
  }

  # Split arcs and events by highlight
  use_highlight <- length(highlight_country) > 0 && any(highlight_country != "")
  if (use_highlight && nrow(pairs_df) > 0) {
    hi_pairs    <- pairs_df |> filter(country_a %in% highlight_country | country_b %in% highlight_country)
    grey_pairs  <- pairs_df |> filter(!country_a %in% highlight_country & !country_b %in% highlight_country)
    conf_hi     <- hi_pairs   |> mutate(x_from = state_x[country_a], x_to = state_x[country_b], y_from = 0, y_to = 0)
    conf_grey   <- grey_pairs |> mutate(x_from = state_x[country_a], x_to = state_x[country_b], y_from = 0, y_to = 0)
    linked_countries <- unique(c(hi_pairs$country_a, hi_pairs$country_b, highlight_country))
    evt_hi   <- evt_df |> filter(country %in% linked_countries)
    evt_grey <- evt_df |> filter(!country %in% linked_countries)
  } else {
    conf_hi   <- month_conf
    conf_grey <- month_conf[0, ]
    evt_hi    <- evt_df
    evt_grey  <- evt_df[0, ]
  }

  # Day gridlines at weekly intervals
  day_ticks <- c(1, 8, 15, 22, 29)
  day_y <- day_ticks + 10

  ggplot() +
    # Event-state edges — dimmed
    geom_segment(
      data = evt_grey,
      aes(x = x, y = y, xend = 0, yend = state_x[country]),
      colour = "grey70", alpha = 0.005, linewidth = 0.2
    ) +
    # Event-state edges — highlighted
    geom_segment(
      data = evt_hi,
      aes(x = x, y = y, xend = 0, yend = state_x[country]),
      colour = "grey70", alpha = 0.01, linewidth = 0.2
    ) +
    # Conflict arcs — grey
    geom_curve(
      data = conf_grey,
      aes(x = y_from, y = x_from, xend = y_to, yend = x_to),
      colour = "grey80", alpha = 0.4, linewidth = 0.3,
      curvature = 0.5, ncp = 20
    ) +
    # Conflict arcs — highlighted
    geom_curve(
      data = conf_hi,
      aes(x = y_from, y = x_from, xend = y_to, yend = x_to, linewidth = weight),
      colour = "#009E73", alpha = 0.7,
      curvature = 0.5, ncp = 20
    ) +
    scale_linewidth_continuous(range = c(0.4, 2), limits = c(1, global_weight_max), name = "Shared conflicts") +
    # Week gridlines
    geom_vline(
      xintercept = day_y, linetype = "dotted",
      colour = "grey85", linewidth = 0.3
    ) +
    # Event nodes — dimmed
    geom_point(
      data = evt_grey,
      aes(x = x, y = y, size = fatality_cat),
      colour = "grey80", alpha = 0.2
    ) +
    # Event nodes — highlighted
    geom_point(
      data = evt_hi,
      aes(x = x, y = y, size = fatality_cat, colour = violence_type),
      alpha = 0.75
    ) +
    scale_colour_manual(
      name   = "Violence type",
      values = c("State-based" = "#0072B2", "Non-state" = "#CC79A7", "One-sided" = "#E69F00"),
      drop   = FALSE
    ) +
    scale_size_manual(
      name   = "Fatalities",
      values = c("0-10" = 1, "10-25" = 2, "25-100" = 3, "100-1000" = 4, "1000+" = 5),
      drop   = FALSE
    ) +
    # State nodes
    geom_point(
      data = state_df,
      aes(x = x, y = y),
      colour = "#8B4513", size = 2, shape = 15, alpha = 0.8
    ) +
    # State labels
    geom_text(
      data = state_df,
      aes(x = x, y = y, label = country),
      size = 2.2, angle = 0, hjust = 0, vjust = 0.5,
      nudge_x = 0.4
    ) +
    # Day labels above top country row
    annotate("text",
      x = day_y, y = n_states * 2 + 2,
      label = paste("Day", day_ticks),
      size = 2, hjust = 0.5, vjust = 0, colour = "grey30"
    ) +
    coord_cartesian(
      clip = "off",
      ylim = c(-2, n_states * 2 + 6),
      xlim = c(-14, 45)
    ) +
    theme_minimal() +
    theme(
      panel.grid  = element_blank(),
      axis.text   = element_blank(),
      axis.title  = element_blank(),
      axis.ticks  = element_blank(),
      plot.margin = margin(10, 10, 40, 30),
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, colour = "grey30")
    ) +
    labs(
      title    = month_label,
      subtitle = paste0(n_events, " events | ", nrow(pairs_df), " conflict linkages between states")
    )
}

# --- 3. Shiny app --------------------------------------------------------

ui <- fluidPage(
  titlePanel("GED 2025: Monthly Event–State Bipartite Network"),
  fluidRow(
    column(
      3,
      div(
        style = "display: flex; align-items: center; margin-bottom: 15px;",
        actionButton("prev_month", "<"),
        h4(textOutput("current_month_text", inline = TRUE),
           style = "margin: 0 15px; min-width: 100px; text-align: center;"),
        actionButton("next_month", ">")
      ),
      br(),
      checkboxInput("all_conflicts", "Show all-year conflict links", FALSE),
      selectInput(
        "highlight_country", "Highlight country",
        choices  = c("(none — all coloured)" = "", sort(unique(raw$country))),
        selected = ""
      ),
      actionButton("clear_country", "Clear selection", style = "width:100%; margin-top:4px;"),
      hr(),
      h4("Summary"),
      verbatimTextOutput("summary_text")
    ),
    column(
      9,
      plotOutput("network_plot", height = "1200px", width = "100%")
    )
  )
)


server <- function(input, output, session) {
  current_month <- reactiveVal(1)

  observeEvent(input$prev_month, {
    if (current_month() > 1) current_month(current_month() - 1)
  })

  observeEvent(input$next_month, {
    if (current_month() < 12) current_month(current_month() + 1)
  })

  observeEvent(input$clear_country, {
    updateSelectInput(session, "highlight_country", selected = "")
  })

  output$current_month_text <- renderText(month.name[current_month()])

  output$network_plot <- renderPlot(
    { build_month_plot(current_month(), input$all_conflicts, input$highlight_country) },
    res = 120
  )

  output$summary_text <- renderText({
    m <- current_month()
    month_raw <- raw |> filter(month_num == m)
    n_evt <- nrow(month_raw)
    n_countries <- n_distinct(month_raw$country)
    total_fatalities <- sum(month_raw$best, na.rm = TRUE)

    top_by_events <- month_raw |>
      count(country, sort = TRUE) |>
      head(5) |>
      mutate(line = sprintf("  %-30s %d", country, n)) |>
      pull(line) |>
      paste(collapse = "\n")

    top_by_fat <- month_raw |>
      group_by(country) |>
      summarise(fatalities = sum(best, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(fatalities)) |>
      head(5) |>
      mutate(line = sprintf("  %-30s %d", country, fatalities)) |>
      pull(line) |>
      paste(collapse = "\n")

    paste0(
      "Events: ", n_evt, "\n",
      "Countries: ", n_countries, "\n",
      "Total fatalities: ", total_fatalities, "\n",
      "\nTop 5 by Events:\n", top_by_events, "\n",
      "\nTop 5 by Fatalities:\n", top_by_fat
    )
  })
}

shinyApp(ui = ui, server = server)
