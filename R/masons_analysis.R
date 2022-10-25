library(readr)
library(readxl)
library(readODS)
library(here)
library(dplyr)
library(tidyr)
library(kableExtra)
library(flextable)
library(ggplot2)
library(ggalt)
library(ggtext)
library(grid)
library(gridExtra)
library(sysfonts)
library(showtext)
source(here("R", "scc_style.R"))
source(here("R", "scc_barchart.R"))

# All waste managed
years_of_interest <- c("2019", "2020", "2021")

wdi_waste_cat <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    select(`Basic Waste Cat`, `Tonnes Received`) |> 
    group_by(`Basic Waste Cat`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_waste_cat <- bind_rows(wdi_waste_cat)

# Treated Waste
years_of_interest <- c("2019", "2020", "2021")

wdi_treatment <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    filter(`Site Category` == "Treatment") |> 
    group_by(`EWC Chapter`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_treatment <- bind_rows(wdi_treatment)

wdi_breakdown <- wdi_treatment |> 
  select()

# Landfill

wdi_landfill <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    filter(`Site Category` == "Treatment") |> 
    group_by(`EWC Chapter`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_treatment <- bind_rows(wdi_treatment)