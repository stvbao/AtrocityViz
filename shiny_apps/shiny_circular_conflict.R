library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

if (!exists("viz_data_file", mode = "function")) {
  for (candidate in c("AtrocityViz/paths.R", "paths.R", "../paths.R")) {
    if (file.exists(candidate)) {
      source(candidate)
      break
    }
  }
}

# --- 1. Constants -----------------------------------------------------------

inner_r    <- 50     # radius of country node ring
gap_r      <- 6      # gap between node ring and event band start
band_w     <- 35     # radial width of the event band
jitter_deg <- 2      # max angular jitter for events within a country's sector
label_r    <- inner_r + 1  # labels sit just outside the node ring

# Arc gap centred at 3 o'clock (0 deg), 8 deg above and 8 deg below = 16 deg total
gap_centre <- 0      # exactly 3 o'clock
gap_half   <- 8      # 8 degrees each side

deg2rad <- function(d) d * pi / 180

# --- 2. Load data -----------------------------------------------------------

raw <- read_excel(viz_data_file("GEDEvent_v25_01_25_12.xlsx")) |>
  mutate(
    date             = as.Date(date_start),
    month_num        = as.integer(format(date, "%m")),
    month_lab        = format(date, "%B %Y"),
    violence_type    = factor(type_of_violence,
                              levels = c(1, 2, 3),
                              labels = c("State-based", "Non-state", "One-sided"))
  )

# --- 3. Country positions (fixed for all months) ----------------------------

all_states <- sort(unique(raw$country))
n_states   <- length(all_states)

# Gap centred at 0 deg (3 o'clock): occupies [-gap_half, +gap_half]
# Arc occupies [+gap_half, 360-gap_half] going counter-clockwise (increasing angle)
# so the last node sits at 360-gap_half = 352 deg = -8 deg, giving a symmetric 16-deg gap at 0 deg
arc_span   <- 360 - 2 * gap_half   # 344 degrees
angles_deg <- seq(gap_centre + gap_half, gap_centre + 360 - gap_half, length.out = n_states)
names(angles_deg) <- all_states

country_nodes <- data.frame(
  country   = all_states,
  angle_deg = angles_deg,
  angle_rad = deg2rad(angles_deg),
  stringsAsFactors = FALSE
) |>
  mutate(
    cx = inner_r * cos(angle_rad),
    cy = inner_r * sin(angle_rad),
    lx = label_r * cos(angle_rad),
    ly = label_r * sin(angle_rad),
    hjust      = ifelse(cos(angle_rad) >= 0, 0, 1),
    text_angle = ifelse(cos(angle_rad) >= 0, angle_deg, angle_deg + 180)
  )

# --- 4. Conflict linkages ---------------------------------------------------

conf_state <- raw |>
  distinct(conflict_new_id, conflict_name, country)

monthly_conf_links <- function(m) {
  month_conflicts  <- raw |> filter(month_num == m) |> distinct(conflict_new_id)
  month_conf_state <- conf_state |> semi_join(month_conflicts, by = "conflict_new_id")

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
    )
}

all_year_pairs <- conf_state |>
  inner_join(conf_state,
    by = "conflict_new_id",
    suffix = c("_a", "_b"), relationship = "many-to-many"
  ) |>
  filter(country_a < country_b) |>
  group_by(country_a, country_b) |>
  summarise(weight = n(), .groups = "drop")

# --- 5. Bezier chord helpers ------------------------------------------------

bezier_path <- function(x0, y0, x2, y2, n_pts = 50) {
  # Quadratic bezier with control point at origin (curves inward through center)
  t  <- seq(0, 1, length.out = n_pts)
  data.frame(
    x = (1 - t)^2 * x0 + t^2 * x2,
    y = (1 - t)^2 * y0 + t^2 * y2
  )
}

make_chord_df <- function(pairs_df) {
  if (nrow(pairs_df) == 0) return(data.frame(x=numeric(), y=numeric(), group=integer(), weight=numeric()))
  rows <- vector("list", nrow(pairs_df))
  for (i in seq_len(nrow(pairs_df))) {
    p0  <- country_nodes[country_nodes$country == pairs_df$country_a[i], ]
    p2  <- country_nodes[country_nodes$country == pairs_df$country_b[i], ]
    pts <- bezier_path(p0$cx, p0$cy, p2$cx, p2$cy)
    pts$group  <- i
    pts$weight <- pairs_df$weight[i]
    rows[[i]]  <- pts
  }
  do.call(rbind, rows)
}

# Reference arcs span same 344 degrees as country nodes
make_arc <- function(r, start_deg = gap_centre + gap_half, end_deg = gap_centre + 360 - gap_half, n = 300) {
  t <- seq(deg2rad(start_deg), deg2rad(end_deg), length.out = n)
  data.frame(x = r * cos(t), y = r * sin(t), group = r)
}

# Pre-compute monthly chord dfs
monthly_chord_dfs   <- lapply(1:12, function(m) make_chord_df(monthly_conf_links(m)))
all_year_chord_df   <- make_chord_df(all_year_pairs)

# Global weight range — fixes linewidth scale across all selections
global_weight_max <- max(all_year_pairs$weight, na.rm = TRUE)

# Day reference arcs at weekly intervals; label days 1, 8, 15, 22, 29
ring_days   <- c(8, 15, 22)
label_days  <- c(1, 8, 15, 22, 29)
ring_radii  <- inner_r + gap_r + (ring_days  - 1) / 30 * band_w
label_radii <- inner_r + gap_r + (label_days - 1) / 30 * band_w
ring_df    <- do.call(rbind, lapply(seq_along(ring_days), function(i) {
  df       <- make_arc(ring_radii[i])
  df$day   <- ring_days[i]
  df
}))

# --- 6. Plot builder --------------------------------------------------------

build_month_plot <- function(m, show_all_conflicts = FALSE, highlight_country = NULL) {
  month_raw <- raw |> filter(month_num == m)

  if (nrow(month_raw) == 0) {
    return(ggplot() + theme_void() +
      labs(title = paste("No events in", month.name[m])))
  }

  month_label      <- unique(month_raw$month_lab)[1]
  active_countries <- unique(month_raw$country)

  # Event positions
  set.seed(42)
  evt_df <- month_raw |>
    distinct(id, .keep_all = TRUE) |>
    mutate(
      day          = as.integer(format(date, "%d")),
      base_angle   = angles_deg[country],
      jitter       = runif(n(), -jitter_deg, jitter_deg),
      angle_rad    = deg2rad(base_angle + jitter),
      radius       = inner_r + gap_r + (day - 1) / 30 * band_w,
      ex           = radius * cos(angle_rad),
      ey           = radius * sin(angle_rad),
      fatality_cat = cut(
        best,
        breaks = c(-Inf, 10, 25, 100, 1000, Inf),
        labels = c("0-10", "10-25", "25-100", "100-1000", "1000+"),
        right  = TRUE
      )
    ) |>
    left_join(country_nodes |> select(country, cx, cy), by = "country")

  # Chord data
  pairs_df <- if (show_all_conflicts) all_year_pairs else monthly_conf_links(m)
  chord_df <- if (show_all_conflicts) all_year_chord_df else monthly_chord_dfs[[m]]

  # Split chords into highlighted vs grey based on selected countries
  use_highlight <- length(highlight_country) > 0 && any(highlight_country != "")
  if (use_highlight && nrow(pairs_df) > 0) {
    hi_pairs   <- pairs_df |> filter(country_a %in% highlight_country | country_b %in% highlight_country)
    grey_pairs <- pairs_df |> filter(!country_a %in% highlight_country & !country_b %in% highlight_country)
    chord_hi   <- make_chord_df(hi_pairs)
    chord_grey <- make_chord_df(grey_pairs)
  } else {
    chord_hi   <- chord_df
    chord_grey <- data.frame(x = numeric(), y = numeric(), group = integer(), weight = numeric())
  }

  # Split events into highlighted vs dimmed based on selected countries
  if (use_highlight) {
    linked_countries <- unique(c(
      hi_pairs$country_a,
      hi_pairs$country_b,
      highlight_country
    ))
    evt_hi   <- evt_df |> filter(country %in% linked_countries)
    evt_grey <- evt_df |> filter(!country %in% linked_countries)
  } else {
    evt_hi   <- evt_df
    evt_grey <- evt_df[0, ]
  }

  # Node data — all countries always shown
  labels_df <- country_nodes   # label all countries

  ggplot() +

    # Reference rings
    geom_path(
      data = ring_df,
      aes(x = x, y = y, group = group),
      colour = "grey88", linewidth = 0.3, linetype = "dotted"
    ) +

    # Grey (non-highlighted) chords
    geom_path(
      data = chord_grey,
      aes(x = x, y = y, group = group),
      colour = "grey80", alpha = 0.4, linewidth = 0.3
    ) +

    # Highlighted chords
    geom_path(
      data = chord_hi,
      aes(x = x, y = y, group = group, linewidth = weight),
      colour = "#009E73", alpha = 0.8
    ) +
    scale_linewidth_continuous(range = c(0.3, 1.8), limits = c(1, global_weight_max), name = "Shared\nconflicts") +

    # Event spokes — dimmed
    geom_segment(
      data = evt_grey,
      aes(x = (inner_r + gap_r) * cos(angle_rad),
          y = (inner_r + gap_r) * sin(angle_rad),
          xend = ex, yend = ey),
      colour = "grey60", alpha = 0.03, linewidth = 0.15
    ) +

    # Event spokes — highlighted
    geom_segment(
      data = evt_hi,
      aes(x = (inner_r + gap_r) * cos(angle_rad),
          y = (inner_r + gap_r) * sin(angle_rad),
          xend = ex, yend = ey),
      colour = "grey60", alpha = 0.06, linewidth = 0.15
    ) +

    # Event points — dimmed
    geom_point(
      data = evt_grey,
      aes(x = ex, y = ey, size = fatality_cat),
      colour = "grey80", alpha = 0.2
    ) +

    # Event points — highlighted
    geom_point(
      data = evt_hi,
      aes(x = ex, y = ey, size = fatality_cat, colour = violence_type),
      alpha = 0.75
    ) +
    scale_colour_manual(
      name   = "Violence type",
      values = c("State-based" = "#0072B2", "Non-state" = "#CC79A7", "One-sided" = "#E69F00")
    ) +
    scale_size_manual(
      name   = "Fatalities",
      values = c("0-10" = 0.8, "10-25" = 1.6, "25-100" = 2.6, "100-1000" = 3.8, "1000+" = 5.2)
    ) +

    # All country nodes — solid blue filled squares
    geom_point(
      data  = country_nodes,
      aes(x = cx, y = cy),
      colour = "#8B4513", size = 2.2, shape = 15, alpha = 0.85
    ) +

    # Country labels (all countries, next to nodes)
    geom_text(
      data  = labels_df,
      aes(x = lx, y = ly, label = country,
          angle = text_angle, hjust = hjust),
      size   = 2.2,
      colour = "grey20"
    ) +

    # Day labels at exactly 3 o'clock (0 deg), reading rightward
    annotate("text",
      x     = label_radii,
      y     = 0,
      label = paste0("Day ", label_days),
      size  = 3.0, colour = "grey30", hjust = 0, vjust = 0.5
    ) +

    coord_fixed(
      ratio = 1, clip = "off",
      xlim  = c(-(inner_r + gap_r + band_w + 8), inner_r + gap_r + band_w + 18),
      ylim  = c(-(inner_r + gap_r + band_w + 8), inner_r + gap_r + band_w + 8)
    ) +
    labs(
      title    = month_label,
      subtitle = paste0(nrow(evt_df), " events | ", nrow(chord_df) / 50, " conflict linkages")
    ) +
    theme_void() +
    theme(
      legend.position  = "right",
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 10),
      plot.title       = element_text(size = 20, face = "bold", hjust = 0.5, colour = "grey15"),
      plot.subtitle    = element_text(size = 15, hjust = 0.5, colour = "grey50"),
      plot.margin      = margin(4, 0, 0, 0)
    )
}

# --- 7. Shiny app -----------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$script(HTML("
    $(document).ready(function() {
      function squarePlot() {
        var w = $('#network_plot').width();
        $('#network_plot').height(w);
      }
      squarePlot();
      $(window).resize(squarePlot);
    });
  "))),
  titlePanel("GED 2025: Monthly Event–State Bipartite Network"),
  fluidRow(
    column(
      2,
      br(),
      div(
        style = "display:flex; align-items:center; justify-content:center; gap:6px;",
        actionButton("prev_month", "\u25C4"),
        h5(textOutput("current_month_text", inline = TRUE),
           style = "margin:0; min-width:70px; text-align:center;"),
        actionButton("next_month", "\u25BA")
      ),
      br(),
      checkboxInput("all_conflicts", "Show all-year conflict links", FALSE),
      selectInput(
        "highlight_country", "Highlight country",
        choices  = c("(none)" = "", sort(unique(raw$country))),
        selected = ""
      ),
      actionButton("clear_country", "Clear selection", style = "width:100%; margin-top:4px;"),
      hr(),
      h5("Summary"),
      verbatimTextOutput("summary_text")
    ),
    column(
      10,
      plotOutput("network_plot", width = "100%", height = "auto")
    )
  )
)

server <- function(input, output, session) {
  current_month <- reactiveVal(1)

  observeEvent(input$prev_month, {
    m <- max(1, current_month() - 1)
    current_month(m)
  })

  observeEvent(input$next_month, {
    m <- min(12, current_month() + 1)
    current_month(m)
  })

  observeEvent(input$clear_country, {
    updateSelectInput(session, "highlight_country", selected = "")
  })

  output$current_month_text <- renderText(month.name[current_month()])


  output$network_plot <- renderPlot(
    { build_month_plot(current_month(), input$all_conflicts, input$highlight_country) },
    res = 80
  )

  output$summary_text <- renderText({
    m         <- current_month()
    month_raw <- raw |> filter(month_num == m)
    n_evt          <- nrow(month_raw)
    n_countries    <- n_distinct(month_raw$country)
    total_fat      <- sum(month_raw$best, na.rm = TRUE)

    top_evt <- month_raw |>
      count(country, sort = TRUE) |>
      head(5) |>
      mutate(line = sprintf("  %-28s %d", country, n)) |>
      pull(line) |> paste(collapse = "\n")

    top_fat <- month_raw |>
      group_by(country) |>
      summarise(f = sum(best, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(f)) |> head(5) |>
      mutate(line = sprintf("  %-28s %d", country, f)) |>
      pull(line) |> paste(collapse = "\n")

    paste0(
      "Events: ", n_evt, "\n",
      "Countries: ", n_countries, "\n",
      "Total fatalities: ", total_fat, "\n",
      "\nTop 5 by Events:\n", top_evt, "\n",
      "\nTop 5 by Fatalities:\n", top_fat
    )
  })
}

shinyApp(ui = ui, server = server)
