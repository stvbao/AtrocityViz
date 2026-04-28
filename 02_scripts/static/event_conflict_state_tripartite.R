# event_conflict_state_tripartite.R
# Tripartite network: Events — Conflicts — States
# Edges: Event--Conflict (each event belongs to a conflict)
#        Conflict--State (each conflict occurs in one or more countries)

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

# Event nodes
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

# Conflict nodes
conflicts <- raw |>
  distinct(conflict_new_id, .keep_all = TRUE) |>
  transmute(
    name          = paste0("conf_", conflict_new_id),
    node_type     = "conflict",
    label         = conflict_name,
    best_est      = NA_real_,
    deaths_civ    = NA_real_,
    violence_type = NA_real_
  )

# State nodes
states <- raw |>
  distinct(country) |>
  transmute(
    name          = paste0("state_", country),
    node_type     = "state",
    label         = country,
    best_est      = NA_real_,
    deaths_civ    = NA_real_,
    violence_type = NA_real_
  )

nodes <- bind_rows(events, conflicts, states)

# --- 3. Build edge lists ------------------------------------------------

# Event → Conflict
edges_evt_conf <- raw |>
  distinct(id, conflict_new_id, .keep_all = TRUE) |>
  transmute(
    from      = paste0("evt_", id),
    to        = paste0("conf_", conflict_new_id),
    edge_type = "event_conflict",
    weight    = best
  )

# Conflict → State
edges_conf_state <- raw |>
  distinct(conflict_new_id, country) |>
  transmute(
    from      = paste0("conf_", conflict_new_id),
    to        = paste0("state_", country),
    edge_type = "conflict_state",
    weight    = 1
  )

edges <- bind_rows(edges_evt_conf, edges_conf_state)

# --- 4. Create igraph object -------------------------------------------

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

cat("=== Triple Network (Conflict) Summary ===\n")
cat("Nodes:", vcount(g), "\n")
cat("  Events:",    sum(V(g)$node_type == "event"),    "\n")
cat("  Conflicts:", sum(V(g)$node_type == "conflict"), "\n")
cat("  States:",    sum(V(g)$node_type == "state"),    "\n")
cat("Edges:", ecount(g), "\n")
cat("  Event-Conflict:",    sum(E(g)$edge_type == "event_conflict"),  "\n")
cat("  Conflict-State:",    sum(E(g)$edge_type == "conflict_state"),  "\n")
cat("Components:", components(g)$no, "\n")
cat("Density:", round(edge_density(g), 6), "\n")

# --- 5. Visualize -------------------------------------------------------

tg <- as_tbl_graph(g) |>
  mutate(deg = centrality_degree())

# Drop weight attribute so FR layout works (some weights are 0)
tg_plot <- tg |> activate(edges) |> select(-weight)

p <- ggraph(tg_plot, layout = "fr") +
  geom_edge_link(aes(colour = edge_type), alpha = 0.15, width = 0.3) +
  geom_node_point(aes(colour = node_type, size = deg), alpha = 0.7) +
  scale_colour_manual(
    values = c(event = "#E69F00", conflict = "#56B4E9", state = "#D55E00"),
    name   = "Node type"
  ) +
  scale_edge_colour_manual(
    values = c(event_conflict = "grey60", conflict_state = "#009E73"),
    name   = "Edge type"
  ) +
  scale_size_continuous(range = c(0.5, 8), name = "Degree") +
  theme_graph() +
  labs(title = "Tripartite Network: Events — Conflicts — States",
       subtitle = paste0("GED v26.0.2 | ", vcount(g), " nodes, ", ecount(g), " edges"))

plot_out <- file.path(results_dir, "event_conflict_state_tripartite.png")
ggsave(plot_out, p, width = 14, height = 10, dpi = 300)
cat("Plot saved to ", plot_out, "\n", sep = "")

# --- 6. Basic network stats per layer ----------------------------------

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

cat("\nTop 10 conflicts by degree:\n")
deg_df |>
  filter(node_type == "conflict") |>
  arrange(desc(degree)) |>
  head(10) |>
  mutate(label = V(g)$label[match(name, V(g)$name)]) |>
  select(label, degree) |>
  print(n = 10)
