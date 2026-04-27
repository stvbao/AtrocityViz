# AtrocityViz

Standalone visualization scripts and Shiny apps extracted from the main AtrocityNet project.

Structure
- `scripts/`: static network construction and plotting scripts
- `shiny_apps/`: interactive Shiny apps
- `paths.R`: shared helper for data and output paths

Current data expectation
- By default, scripts look for input files under `01_raw/` in the project root.
- You can override the input file path by setting `ATROCITYVIZ_DATA` in your R session before sourcing a script.

Current output locations
- Graph objects: `02_tidy/AtrocityViz/`
- Figures: `04_results/AtrocityViz/`

Posit / Shiny note
- These Shiny apps are plain local Shiny apps.
- They are not linked to Positron Cloud, `rsconnect`, or `shinyapps.io`.
- To publish them later, you would need to add deployment configuration separately.
