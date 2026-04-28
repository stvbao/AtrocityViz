# event_state_temporal_layers.R
# Layered bipartite with temporal y-axis:
#   x-axis = states (bottom row), events positioned above their state
#   y-axis = time in 15-day intervals (event date_start)
# Conflict-derived state–state arcs drawn below the state row

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

results_dir <- file.path(project_root, "03_results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# --- 1. Load data -------------------------------------------------------
raw <- read_excel(data_file)

# --- 2. Build nodes -----------------------------------------------------

# Date reference: earliest date as origin
date_origin <- min(as.Date(raw$date_start))
date_max    <- max(as.Date(raw$date_start))

events <- raw |>
  distinct(id, .keep_all = TRUE) |>
  transmute(
    name          = paste0("evt_", id),
    node_type     = "event",
    label         = paste0("E", id),
    best_est      = best,
    deaths_civ    = deaths_civilians,
    violence_type = type_of_violence,
    date          = as.Date(date_start),
    # Days since origin, scaled to 15-day units
    day_offset    = as.numeric(as.Date(date_start) - date_origin)
  )

all_states <- sort(unique(raw$country))
states <- tibble(
  name          = paste0("state_", all_states),
  node_type     = "state",
  label         = all_states,
  best_est      = NA_real_,
  deaths_civ    = NA_real_,
  violence_type = NA_real_,
  date          = as.Date(NA),
  day_offset    = NA_real_
)

nodes <- bind_rows(events, states)

# --- 3. Build edges -----------------------------------------------------

edges_evt_state <- raw |>
  distinct(id, country, .keep_all = TRUE) |>
  transmute(
    from      = paste0("evt_", id),
    to        = paste0("state_", country),
    edge_type = "event_state",
    weight    = best
  )

conf_state <- raw |>
  distinct(conflict_new_id, conflict_name, country)

edges_state_state <- conf_state |>
  inner_join(conf_state, by = "conflict_new_id",
             suffix = c("_a", "_b"), relationship = "many-to-many") |>
  filter(country_a < country_b) |>
  group_by(country_a, country_b) |>
  summarise(
    weight           = n(),
    shared_conflicts = paste(unique(conflict_name_a), collapse = "; "),
    .groups          = "drop"
  ) |>
  transmute(
    from      = paste0("state_", country_a),
    to        = paste0("state_", country_b),
    edge_type = "conflict_link",
    weight    = weight
  )

edges <- bind_rows(edges_evt_state, edges_state_state)

# --- 4. Create igraph ---------------------------------------------------

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

cat("=== Layered Temporal Bipartite ===\n")
cat("Nodes:", vcount(g), "\n")
cat("  Events:", sum(V(g)$node_type == "event"), "\n")
cat("  States:", sum(V(g)$node_type == "state"), "\n")
cat("Edges:", ecount(g), "\n")
cat("  Event-State:",   sum(E(g)$edge_type == "event_state"),   "\n")
cat("  Conflict-Link:", sum(E(g)$edge_type == "conflict_link"), "\n")
cat("Date range:", as.character(date_origin), "to", as.character(date_max), "\n")

# --- 5. Temporal layered layout -----------------------------------------
# x = state position (ordered by event count)
# y = 0 for states, day_offset for events (in 15-day units)

tg <- as_tbl_graph(g)
node_df <- tg |> activate(nodes) |> as_tibble()

# State x-positions ordered by event count
state_evt_count <- edges_evt_state |>
  count(to, name = "n_events") |>
  rename(name = to) |>
  arrange(desc(n_events))

state_order <- state_evt_count$name
missing_states <- setdiff(paste0("state_", all_states), state_order)
state_order <- c(state_order, missing_states)
n_states <- length(state_order)
state_x <- setNames(seq_len(n_states), state_order)

# Map events to their state
evt_state_map <- edges_evt_state |> select(from, to)

# Build layout
set.seed(42)
layout_df <- node_df |>
  mutate(
    x = case_when(
      node_type == "state" ~ state_x[name],
      TRUE ~ NA_real_
    ),
    y = case_when(
      node_type == "state" ~ 0,
      TRUE ~ NA_real_
    )
  )

# 15-day interval scale: y = day_offset / 15, so each unit = 15 days
for (i in seq_len(nrow(layout_df))) {
  if (layout_df$node_type[i] == "event") {
    evt_name <- layout_df$name[i]
    state_name <- evt_state_map$to[evt_state_map$from == evt_name]
    if (length(state_name) == 1) {
      sx <- state_x[state_name]
      layout_df$x[i] <- sx + runif(1, -0.35, 0.35)
      # y = month offset (fractional), starting at 1 (above state row)
      evt_date <- layout_df$date[i]
      month_offset <- (as.numeric(format(evt_date, "%m")) - 1) +
                      (as.numeric(format(evt_date, "%d")) - 1) / 30
      layout_df$y[i] <- 1 + month_offset
    }
  }
}

layout_manual <- as.matrix(layout_df[, c("x", "y")])

# --- 6. Monthly tick labels for y-axis ----------------------------------

tick_positions <- 1 + (0:11)  # 12 months: Jan=1, Feb=2, ..., Dec=12
tick_labels <- month.abb

# --- 7. Plot ------------------------------------------------------------

g_evt <- delete_edges(g, E(g)[E(g)$edge_type == "conflict_link"])
tg_evt <- as_tbl_graph(g_evt)

conf_edges <- edges_state_state |>
  mutate(
    x_from = state_x[from],
    x_to   = state_x[to],
    y_from = 0,
    y_to   = 0
  )

p <- ggraph(tg_evt, layout = "manual", x = layout_manual[, 1], y = layout_manual[, 2]) +
  geom_edge_link(colour = "grey70", alpha = 0.1, width = 0.2) +
  # Conflict arcs below state row
  geom_curve(
    data = conf_edges,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    colour = "#009E73", linewidth = 0.8, alpha = 0.7,
    curvature = -0.5, ncp = 20
  ) +
  # Monthly gridlines
  geom_hline(yintercept = tick_positions, linetype = "dotted",
             colour = "grey80", linewidth = 0.3) +
  # Nodes
  geom_node_point(aes(colour = node_type, size = ifelse(node_type == "state", 4, 0.8)),
                  alpha = 0.8) +
  # State labels below
  geom_node_text(
    aes(label = ifelse(node_type == "state", label, NA)),
    size = 2, angle = 45, hjust = 1, vjust = 1,
    nudge_y = -0.2, na.rm = TRUE
  ) +
  # Y-axis date labels on the left
  annotate("text", x = 0.2, y = tick_positions, label = tick_labels,
           size = 2.5, hjust = 1, colour = "grey40") +
  # Dummy layer for conflict linkage legend
  geom_curve(
    data = conf_edges[1, , drop = FALSE],
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
        linetype = "Conflict linkage"),
    colour = NA, curvature = -0.5
  ) +
  scale_linetype_manual(
    values = c("Conflict linkage" = "solid"),
    name   = NULL,
    guide  = guide_legend(
      override.aes = list(colour = "#009E73", linewidth = 0.8, alpha = 0.5)
    )
  ) +
  scale_colour_manual(
    values = c(event = "#E69F00" , state = "#0072B2"),
    name   = "Node type"
  ) +
  scale_size_identity() +
  theme_graph() +
  theme(plot.margin = margin(10, 10, 10, 10)) +
  coord_cartesian(clip = "off", ylim = c(-6, max(layout_manual[, 2], na.rm = TRUE) + 0.5)) +
  labs(
    title    = "Temporal Layered Bipartite: Events by month",
    subtitle = paste0("GED v25 | ", as.character(date_origin), " to ",
                      as.character(date_max),
                      " | green arcs = shared cross-border conflicts")
  )

plot_out <- file.path(results_dir, "event_state_temporal_layers.png")
ggsave(plot_out, p,
       width = 20, height = 12, dpi = 300)
cat("\nPlot saved to ", plot_out, "\n", sep = "")
