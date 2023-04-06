library(bookdown)
library(ggplot2)
lapply(paste0(here::here("R"), "/", list.files(here::here("R"))), source)

folder <- paste0(here::here("Data"), "/")
wdi_data <- read_wdi(folder)
lacw_data <- read_lacw(folder)
rlc_data <- read_rcl(folder)

render_wam_report_site(wdi_data, lacw_data, rlc_data)
