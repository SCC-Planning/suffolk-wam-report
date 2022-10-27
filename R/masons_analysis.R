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

# Waste movements----
# Who do we import our waste from?----
wdi_origin <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  totals  <- wdi %>%
    select(`Tonnes Received`, `Origin WPA`) %>%
    group_by(`Origin WPA`) %>%
    summarise(tonnes = sum(`Tonnes Received`))
  
  totals$year <- year
  
  return(totals)
})

relevant_counties <- c(
  "Suffolk",
  "Norfolk",
  "Essex",
  "Cambridgeshire",
  "Peterborough"
)

wdi_origin <- bind_rows(wdi_origin)

counties <- wdi_origin %>%
  filter(`Origin WPA` %in% relevant_counties)

other <- wdi_origin %>%
  filter(!`Origin WPA` %in% relevant_counties) %>%
  select(tonnes, year) %>%
  group_by(year) %>%
  summarise(tonnes = sum(tonnes))

other$`Origin WPA` <- "Other"

wdi_origin_table <- bind_rows(counties, other)

wdi_origin_plot <- wdi_origin_table |> 
  filter(`Origin WPA` != "Suffolk")

wdi_origin_plot <- wdi_origin_plot[(order(-wdi_origin_plot$year)),]

scc_barchart_grouped(wdi_origin_plot, wdi_origin_plot$`Origin WPA`, wdi_origin_plot$`tonnes`, wdi_origin_plot$year , "Waste Imported to Suffolk by Origin (tonnes)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

wdi_origin_table <- wdi_origin_table %>%
  pivot_wider(names_from = `year`, values_from = `tonnes`)

wdi_origin_table <- wdi_origin_table[(order(-wdi_origin_table$`2021`)),]

suffolk_total <- wdi_origin_table %>%
  filter(`Origin WPA` == "Suffolk")

from_others <- data.frame(
  `Origin` = c("Total waste managed origin outwith Suffolk"),
  `2019` = sum(wdi_origin_table$`2019`) - suffolk_total$`2019`,
  `2020` = sum(wdi_origin_table$`2020`) - suffolk_total$`2020`,
  `2021` = sum(wdi_origin_table$`2021`) - suffolk_total$`2021`
)

managed_totals <- data.frame(
  `Origin` = c("Total waste managed in Suffolk"),
  `2019` = sum(wdi_origin_table$`2019`),
  `2020` = sum(wdi_origin_table$`2020`),
  `2021` = sum(wdi_origin_table$`2021`)
)
colnames(from_others) <- colnames(wdi_origin_table)
colnames(managed_totals) <- colnames(wdi_origin_table)

wdi_origin_table <- bind_rows(wdi_origin_table, from_others)
wdi_origin_table <- bind_rows(wdi_origin_table, managed_totals)

wdi_origin_table$`2019` <- totals_formatting(wdi_origin_table$`2019`)
wdi_origin_table$`2020` <- totals_formatting(wdi_origin_table$`2020`)
wdi_origin_table$`2021` <- totals_formatting(wdi_origin_table$`2021`)

wdi_origin_table %>%
  flextable() %>%
  set_table_properties(layout = "autofit") %>%
  add_header_row(
    colwidths = c(4),
    values = c("All waste managed in Suffolk (tonnes)")
  ) %>%
  bold(i = 7:8, j = 1:4, part = "body") %>%
  theme_vanilla()

# Who do we export our waste to?----
