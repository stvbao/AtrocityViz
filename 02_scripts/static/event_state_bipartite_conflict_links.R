# event_state_bipartite_conflict_links.R
# Augmented bipartite: Events — States, with state–state edges from shared conflicts
# Two edge types: event_state (event belongs to a country)
#                 conflict_link (two states share a cross-border conflict)

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
  unset = file.path(project_root, "01_data", "GEDevent_v26_0_2.xlsx")
)

results_dir <- file.path(project_root, "03_results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# --- 1. Load data -------------------------------------------------------
raw <- read_excel(data_file)

# --- 2. Build node lists ------------------------------------------------

events <- raw |>
  distinct(id, .keep_all = TRUE) |>
  transmute(
    name          = paste0("evt_", id),
    node_type     = "event",
    label         = paste0("E", id),
    best_est      = best,
    deaths_civ    = deaths_civilians,
    violence_type = type_of_violence
  )

all_states <- sort(unique(raw$country))
states <- tibble(
  name          = paste0("state_", all_states),
  node_type     = "state",
  label         = all_states,
  best_est      = NA_real_,
  deaths_civ    = NA_real_,
  violence_type = NA_real_
)

nodes <- bind_rows(events, states)

# --- 3. Build edge lists ------------------------------------------------

# Event → State
edges_evt_state <- raw |>
  distinct(id, country, .keep_all = TRUE) |>
  transmute(
    from      = paste0("evt_", id),
    to        = paste0("state_", country),
    edge_type = "event_state",
    weight    = best
  )

# State ↔ State (via shared conflicts)
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

# --- 4. Create igraph object -------------------------------------------

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

cat("=== Augmented Bipartite (Event–State + Conflict Links) ===\n")
cat("Nodes:", vcount(g), "\n")
cat("  Events:", sum(V(g)$node_type == "event"), "\n")
cat("  States:", sum(V(g)$node_type == "state"), "\n")
cat("Edges:", ecount(g), "\n")
cat("  Event-State:",   sum(E(g)$edge_type == "event_state"),   "\n")
cat("  Conflict-Link:", sum(E(g)$edge_type == "conflict_link"), "\n")
cat("Components:", components(g)$no, "\n")
cat("Density:", round(edge_density(g), 6), "\n")

# --- 5. Visualize -------------------------------------------------------

tg <- as_tbl_graph(g) |>
  mutate(deg = centrality_degree())

# Drop weight for FR layout (zero weights cause errors)
tg_plot <- tg |> activate(edges) |> select(-weight)

p <- ggraph(tg_plot, layout = "fr") +
  geom_edge_link(aes(colour = edge_type, width = edge_type, alpha = edge_type)) +
  geom_node_point(aes(colour = node_type, size = deg), alpha = 0.7) +
  geom_node_text(
    aes(label = ifelse(node_type == "state", label, NA)),
    size = 2.5, repel = TRUE, na.rm = TRUE
  ) +
  scale_colour_manual(
    values = c(event = "#E69F00", state = "#D55E00"),
    name   = "Node type"
  ) +
  scale_edge_colour_manual(
    values = c(event_state = "grey70", conflict_link = "#009E73"),
    name   = "Edge type"
  ) +
  scale_edge_width_manual(
    values = c(event_state = 0.3, conflict_link = 1.5),
    guide  = "none"
  ) +
  scale_edge_alpha_manual(
    values = c(event_state = 0.1, conflict_link = 0.7),
    guide  = "none"
  ) +
  scale_size_continuous(range = c(0.5, 10), name = "Degree") +
  theme_graph() +
  labs(
    title    = "Augmented Bipartite: Events — States + Conflict Links",
    subtitle = paste0("GED v26.0.2 | ", vcount(g), " nodes, ", ecount(g), " edges | ",
                      "green edges = states sharing a cross-border conflict")
  )

plot_out <- file.path(results_dir, "event_state_bipartite_conflict_links.png")
ggsave(plot_out, p,
       width = 14, height = 10, dpi = 300)
cat("Plot saved to ", plot_out, "\n", sep = "")

# --- 6. Stats ------------------------------------------------------------

cat("\n=== Degree stats by node type ===\n")
deg_df <- tibble(
  name      = V(g)$name,
  node_type = V(g)$node_type,
  degree    = degree(g)
)

deg_df |>
  group_by(node_type) |>
  summarise(
    n          = n(),
    mean_deg   = round(mean(degree), 2),
    median_deg = median(degree),
    max_deg    = max(degree),
    .groups    = "drop"
  ) |>
  print()

cat("\nTop 10 states by degree:\n")
deg_df |>
  filter(node_type == "state") |>
  arrange(desc(degree)) |>
  head(10) |>
  mutate(label = V(g)$label[match(name, V(g)$name)]) |>
  select(label, degree) |>
  print(n = 10)

cat("\nConflict links between states:\n")
edges_state_state |>
  mutate(
    state_a = str_remove(from, "^state_"),
    state_b = str_remove(to, "^state_")
  ) |>
  arrange(desc(weight)) |>
  select(state_a, state_b, weight) |>
  print(n = 30)
