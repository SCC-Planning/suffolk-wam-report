# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c(
    "readr",
    "readxl",
    "readODS",
    "here",
    "dplyr",
    "tidyr",
    "kableExtra",
    "flextable",
    "ggplot2",
    "ggalt",
    "ggtext",
    "grid",
    "gridExtra",
    "sysfonts",
    "showtext",
    "sccthemes"    
    ), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), tar_source)
# tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    folder, here::here("Data")
  ),
  tar_target(
    wdi_data, read_wdi(folder)
  ),
  tar_target(
    lacw_data, read_lacw(folder)
  ),
  tar_target(
    rlc_data, read_rcl(folder)
  ),
  tar_target(
    wam_report, render_wam_report_site(wdi_data, lacw_data, rlc_data)
  )
)
