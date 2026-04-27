# state_projection.R
# One-mode state projection from the conflict tripartite network
# Two states share an edge if they share at least one conflict
# Edge weight = number of shared conflicts

library(tidyverse)
library(readxl)
library(igraph)
library(tidygraph)
library(ggraph)

if (!exists("viz_data_file", mode = "function")) {
  for (candidate in c("AtrocityViz/paths.R", "paths.R", "../paths.R")) {
    if (file.exists(candidate)) {
      source(candidate)
      break
    }
  }
}

# --- 1. Load data -------------------------------------------------------
raw <- read_excel(viz_data_file("GEDevent_v26_0_2.xlsx"))

# --- 2. Build conflict–state incidence ----------------------------------

conf_state <- raw |>
  distinct(conflict_new_id, conflict_name, country)

cat("Conflict–state pairs:", nrow(conf_state), "\n")
cat("Conflicts spanning >1 country:\n")
conf_state |>
  group_by(conflict_new_id, conflict_name) |>
  summarise(n_countries = n_distinct(country), .groups = "drop") |>
  filter(n_countries > 1) |>
  arrange(desc(n_countries)) |>
  print(n = 20)

# --- 3. Project to state–state edges -----------------------------------
# Self-join: for each conflict, pair all countries involved

state_pairs <- conf_state |>
  inner_join(conf_state, by = "conflict_new_id", suffix = c("_a", "_b")) |>
  filter(country_a < country_b) |>  # undirected, no self-loops
  group_by(country_a, country_b) |>
  summarise(
    weight          = n(),
    shared_conflicts = paste(unique(conflict_name_a), collapse = "; "),
    .groups         = "drop"
  )

cat("\nState–state edges:", nrow(state_pairs), "\n")

# --- 4. Build igraph ----------------------------------------------------

# Include ALL states as vertices (even isolates with no cross-border conflicts)
all_states <- sort(unique(raw$country))
g <- graph_from_data_frame(state_pairs, directed = FALSE, vertices = data.frame(name = all_states))

cat("\n=== State Projection Summary ===\n")
cat("Nodes:", vcount(g), "\n")
cat("Edges:", ecount(g), "\n")
cat("Components:", components(g)$no, "\n")
cat("Density:", round(edge_density(g), 4), "\n")

# --- 5. Save -------------------------------------------------------------

graph_out <- viz_tidy_file("state_projection.rds")
saveRDS(g, graph_out)
cat("\nGraph saved to ", graph_out, "\n", sep = "")

# --- 6. Visualize --------------------------------------------------------

tg <- as_tbl_graph(g) |>
  mutate(
    deg      = centrality_degree(),
    strength = centrality_degree(weights = weight),
    btw      = centrality_betweenness()
  )

p <- ggraph(tg, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight), colour = "#009E73") +
  geom_node_point(aes(size = strength), colour = "#D55E00", alpha = 0.8) +
  geom_node_text(aes(label = name), size = 2.5, repel = TRUE) +
  scale_edge_width_continuous(range = c(0.3, 3), name = "Shared conflicts") +
  scale_edge_alpha_continuous(range = c(0.2, 0.8), guide = "none") +
  scale_size_continuous(range = c(2, 12), name = "Strength") +
  theme_graph() +
  labs(title = "State-to-State Projection (via shared conflicts)",
       subtitle = paste0("GED v26.0.2 | ", vcount(g), " states, ", ecount(g), " edges"))

plot_out <- viz_results_file("state_projection.png")
ggsave(plot_out, p, width = 14, height = 10, dpi = 300)
cat("Plot saved to ", plot_out, "\n", sep = "")

# --- 7. Stats ------------------------------------------------------------

cat("\n=== Top 10 states by strength (weighted degree) ===\n")
tibble(
  state    = V(g)$name,
  degree   = degree(g),
  strength = strength(g)
) |>
  arrange(desc(strength)) |>
  head(10) |>
  print(n = 10)

cat("\n=== Top 10 state pairs by shared conflicts ===\n")
state_pairs |>
  arrange(desc(weight)) |>
  head(10) |>
  select(country_a, country_b, weight, shared_conflicts) |>
  print(n = 10, width = 120)
