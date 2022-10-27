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

options(scipen=999)

# What are the categories of waste managed in Suffolk? (hazardous, inert etc.)----
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

scc_barchart_grouped(wdi_waste_cat, wdi_waste_cat$`Basic Waste Cat`, wdi_waste_cat$`tonnes`, wdi_waste_cat$year , "Waste Managed by Basic Waste Category in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# Of Waste Managed, which types of sites is it sent to?----
wdi_site_cat <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    group_by(`Site Category`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_site_cat <- bind_rows(wdi_site_cat)
wdi_site_cat <- wdi_site_cat |> 
  filter(!`Site Category` %in% c("Mobile Plant", "Use of Waste"))

scc_barchart_grouped(wdi_site_cat, wdi_site_cat$`Site Category`, wdi_site_cat$`tonnes`, wdi_site_cat$year , "Waste Managed by Site Category in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# Of the waste treated, which types of facilities treat waste?----
wdi_treatment <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    filter(`Site Category` == "Treatment") |> 
    group_by(`Facility Type`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_treatment <- bind_rows(wdi_treatment)

wdi_treatment$`Facility Type` <- gsub("digestion", "Digestion", wdi_treatment$`Facility Type`)
wdi_treatment$`Facility Type` <- gsub("Non Haz", "Non-Haz", wdi_treatment$`Facility Type`)

scc_barchart_grouped(wdi_treatment, wdi_treatment$`Facility Type`, wdi_treatment$`tonnes`, wdi_treatment$year , "Waste Treated by Facility Type in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# For our landfill, which sites receive the landfill waste?----
wdi_totals <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  totals  <- wdi %>%
    select(`Site Category`, `Site Name`, `Tonnes Received`) %>%
    group_by(`Site Category`, `Site Name`) %>%
    summarise(tonnes = sum(`Tonnes Received`), .groups = "drop")
  
  totals$year <- year
  
  return(totals)
})

landfill_totals <- bind_rows(wdi_totals)
landfill_totals$`Site Name` <- gsub("EPR", "", landfill_totals$`Site Name`)
landfill_totals$`Site Name` <- gsub("BV4517IM", "", landfill_totals$`Site Name`)
landfill_totals$`Site Name` <- gsub("\\/", "", landfill_totals$`Site Name`)
landfill_totals$`Site Name` <- gsub(" - ", "", landfill_totals$`Site Name`)

landfill_totals <- landfill_totals %>%
  filter(`Site Category` == "Landfill")

landfill_totals <- landfill_totals[order(-landfill_totals$tonnes),]

scc_barchart_grouped(landfill_totals, landfill_totals$`Site Name`, landfill_totals$`tonnes`, landfill_totals$year , "Waste Received to Landfill in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
