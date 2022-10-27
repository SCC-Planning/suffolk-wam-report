library(readr)
library(readxl)
library(readODS)
library(here)
library(dplyr)
library(tidyr)
library(kableExtra)
library(flextable)
library(ggplot2)
library(ggrepel)
library(ggalt)
library(ggtext)
library(grid)
library(ggpubr)
library(gridExtra)
library(sysfonts)
library(showtext)
library(forcats)
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

p1_barchart <- scc_barchart_grouped(wdi_waste_cat, wdi_waste_cat$`Basic Waste Cat`, wdi_waste_cat$`tonnes`, wdi_waste_cat$year , "Waste Managed by Basic Waste Category in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p1_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Managed_by_Cat.png"),
  width_pixels = 640,
  height_pixels = 450
)

# pie chart for 2020 only
wdi_waste_cat_pie <- wdi_waste_cat |> 
  filter(year == 2020)

p1_piechart <- scc_piechart(
  wdi_waste_cat_pie,
  wdi_waste_cat_pie$tonnes,
  wdi_waste_cat_pie$`Basic Waste Cat`,
  title = "Waste Managed by Basic Waste Category in Suffolk",
  subtitle = paste("Total of", totals_formatting(sum(wdi_waste_cat_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p1_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Managed_by_Cat.png"),
  width_pixels = 640,
  height_pixels = 450
)

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
  filter(!`Site Category` %in% c("Mobile Plant", "Use of Waste", "Storage"))

p2_barchart <- scc_barchart_grouped(wdi_site_cat, wdi_site_cat$`Site Category`, wdi_site_cat$`tonnes`, wdi_site_cat$year , "Waste Managed by Site Category in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p2_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Managed_by_Site_Cat.png"),
  width_pixels = 640,
  height_pixels = 450
)

# pie chart for 2020 only
wdi_site_cat_pie <- wdi_site_cat |> 
  filter(year == 2020)

p2_piechart <- scc_piechart(
  wdi_site_cat_pie,
  wdi_site_cat_pie$tonnes,
  wdi_site_cat_pie$`Site Category`,
  title = "Waste Managed by Site Category in Suffolk",
  subtitle = paste("Total of", totals_formatting(sum(wdi_site_cat_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p2_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Managed_by_Site_Cat.png"),
  width_pixels = 640,
  height_pixels = 450
)

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

wdi_treatment <- wdi_treatment |> 
  filter(!`Facility Type` %in% c("Recovery of Waste", "WEEE treatment facility", "Chemical Treatment"))

wdi_treatment$`Facility Type` <- gsub("Transfer / ", "", wdi_treatment$`Facility Type`)


p3_barchart <- scc_barchart_grouped(wdi_treatment, wdi_treatment$`Facility Type`, wdi_treatment$`tonnes`, wdi_treatment$year , "Waste Treated by Facility Type in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p3_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Managed_by_Facility_Type.png"),
  width_pixels = 640,
  height_pixels = 450
)

# pie chart for 2020 only
wdi_treatment_pie <- wdi_treatment |> 
  filter(year == 2020)

p3_piechart <- scc_piechart(
  wdi_treatment_pie,
  wdi_treatment_pie$tonnes,
  wdi_treatment_pie$`Facility Type`,
  title = "Waste Treated by Facility Type in Suffolk",
  subtitle = paste("Total of", totals_formatting(sum(wdi_treatment_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p3_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Managed_by_Facility_Type.png"),
  width_pixels = 950,
  height_pixels = 500
)

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

landfill_totals$`Site Name` <- gsub("Folly Farm Waste Management Facility", "Folly Farm", landfill_totals$`Site Name`)
landfill_totals$`Site Name` <- gsub("Shrublands Quarry Landfill", "Shrublands Quarry", landfill_totals$`Site Name`)

p4_barchart <- scc_barchart_grouped(landfill_totals, landfill_totals$`Site Name`, landfill_totals$`tonnes`, landfill_totals$year , "Waste Received to Landfill in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p4_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Received_to_Landfill.png"),
  width_pixels = 800,
  height_pixels = 600
)

# pie chart for 2020 only
landfill_totals_pie <- landfill_totals |> 
  filter(year == 2020)

p4_piechart <- scc_piechart(
  landfill_totals_pie,
  landfill_totals_pie$tonnes,
  landfill_totals_pie$`Site Name`,
  title = "Waste Received to Landfill by Site in Suffolk",
  subtitle = paste("Total of", totals_formatting(sum(landfill_totals_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p4_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Received_to_Landfill.png"),
  width_pixels = 950,
  height_pixels = 500
)

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

p5_barchart <- scc_barchart_grouped(wdi_origin_plot, wdi_origin_plot$`Origin WPA`, wdi_origin_plot$`tonnes`, wdi_origin_plot$year , "Waste Imported to Suffolk by Origin (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p5_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Imported_to_Suffolk.png"),
  width_pixels = 640,
  height_pixels = 450
)


wdi_origin_pie <- wdi_origin_plot |> 
  filter(year == 2020)

p5_piechart <- scc_piechart(
  wdi_origin_pie,
  wdi_origin_pie$tonnes,
  wdi_origin_pie$`Origin WPA`,
  title = "Waste Imported to Suffolk by Origin",
  subtitle = paste("Total of", totals_formatting(sum(wdi_origin_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p5_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Imported_to_Suffolk.png"),
  width_pixels = 640,
  height_pixels = 450
)

# Who do we export our waste to?----
wdi_export <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Origin WPA` == "Suffolk" & `Facility WPA` != "Suffolk")
  
  totals  <- wdi %>%
    select(`Tonnes Received`, `Facility WPA`) %>%
    group_by(`Facility WPA`) %>%
    summarise(tonnes = sum(`Tonnes Received`))
  
  totals$year <- year
  
  return(totals)
})

wdi_export <- bind_rows(wdi_export)

wdi_export$`Facility WPA`[is.na(wdi_export$`Facility WPA`)] <- "Other"

counties_export <- wdi_export %>%
  filter(`Facility WPA` %in% relevant_counties)

other_export <- wdi_export %>%
  filter(!`Facility WPA` %in% relevant_counties) %>%
  select(tonnes, year) %>%
  group_by(year) %>%
  summarise(tonnes = sum(tonnes))

other_export$`Facility WPA` <- "Other"

wdi_export_table <- bind_rows(counties_export, other_export)

p6_barchart <- scc_barchart_grouped(wdi_export_table, wdi_export_table$`Facility WPA`, wdi_export_table$`tonnes`, wdi_export_table$year , "Waste Exported from Suffolk by Destination (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p6_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Exported_from_Suffolk.png"),
  width_pixels = 640,
  height_pixels = 450
)

wdi_export_pie <- wdi_export_table |> 
  filter(year == 2020)

p6_piechart <- scc_piechart(
  wdi_export_pie,
  wdi_export_pie$tonnes,
  wdi_export_pie$`Facility WPA`,
  title = "Waste Exported from Suffolk by Destination",
  subtitle = paste("Total of", totals_formatting(sum(wdi_export_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p6_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Exported_from_Suffolk.png"),
  width_pixels = 640,
  height_pixels = 450
)


# On/In land
# Of the waste treated, which types of facilities treat waste?----
wdi_onland <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    filter(`Site Category` == "On/In Land") |> 
    group_by(`Site Name`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_onland <- bind_rows(wdi_onland)

wdi_onland <- wdi_onland |> 
  filter(!`Site Name` %in% c("Lion Barn Phase 2", "Stratton Hall Logoons"))

p7_barchart <- scc_barchart_grouped(wdi_onland, wdi_onland$`Site Name`, wdi_onland$`tonnes`, wdi_onland$year , "Waste Received On/In Land in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p7_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Waste_Received_to_On_In_Land.png"),
  width_pixels = 640,
  height_pixels = 450
)

# pie chart for 2020 only
wdi_onland_pie <- wdi_onland |> 
  filter(year == 2020)

p7_piechart <- scc_piechart(
  wdi_onland_pie,
  wdi_onland_pie$tonnes,
  wdi_onland_pie$`Site Name`,
  title = "Waste Received On/In Land by Site in Suffolk",
  subtitle = paste("Total of", totals_formatting(sum(wdi_onland_pie$tonnes)), "tonnes in 2020")
)

finalise_plot(
  p7_piechart,
  source = "Environment Agency Waste Data Interrogator 2020",
  save_filepath = here("plots", "2020_piechart_Waste_Received_to_On_In_Land.png"),
  width_pixels = 840,
  height_pixels = 450
)

# Remaining Landfill Capacity
capacity <- read_excel(here("Data", "2021_Remaining_Landfill_Capacity.xlsx"), skip = 7)

capacity <- capacity %>%
  filter(`Local Authority` == "Suffolk") %>%
  select(`Operator Name`, `Facility Name`, `Site Type`, `Remaining Capacity end 2021 (cubic metres)`)

capacity_2020 <- read_excel(here("Data", "2020_Remaining_Landfill_Capacity_v2.xlsx"), skip = 7)

capacity_2020 <- capacity_2020 %>%
  filter(`Former Planning Sub Region` == "Suffolk") %>%
  select(`Operator Name`, `Facility Name`, `Site Type`, `Remaining Capacity end 2020 (cubic metres)`)

capacity_2020 <- capacity_2020[order(-capacity_2020$`Remaining Capacity end 2020 (cubic metres)`),]

capacity_2019 <- read_excel(here("Data", "2019 Remaining Landfill Capacity v2.xlsx"), skip = 7)

capacity_2019 <- capacity_2019 %>%
  filter(`Former Planning Sub Region` == "Suffolk") %>%
  select(`Operator Name`, `Facility Name`, `Site Type`, `Remaining Capacity end 2019 (cubic metres)`)

capacity <- inner_join(capacity, capacity_2019)
capacity <- inner_join(capacity, capacity_2020)

capacity <- capacity %>%
  select(
    Operator = `Operator Name`, Facility = `Facility Name`, `Site Type`,
    `2019` = `Remaining Capacity end 2019 (cubic metres)`,
    `2020` = `Remaining Capacity end 2020 (cubic metres)`,
    `2021` = `Remaining Capacity end 2021 (cubic metres)`
  )

capacity_plot <- pivot_longer(
  capacity, 
  cols = c(`2019`, `2020`, `2021`), 
  names_to = "year",
  values_to = "cubic_metres"
)

capacity_plot <- capacity_plot %>%
  filter(cubic_metres > 0)

p8_barchart <- scc_barchart_grouped(capacity_plot, x = capacity_plot$Facility, y = capacity_plot$cubic_metres, group = capacity_plot$year, "Remaining Landfill capacity by Facility in Suffolk (cubic metres)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p8_barchart,
  source = "Environment Agency Remaining Landfill Capacity 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Remaining_Landfill_Capacity.png"),
  width_pixels = 640,
  height_pixels = 450
)

# Types of waste received by Masons Landfill
wdi_masons <- lapply(years_of_interest, function(year) {
  wdi <- read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
    filter(`Facility WPA` == "Suffolk")
  
  wdi  <- wdi %>%
    filter(`Site Category` == "Landfill") |> 
    select(`Site Name`, `Basic Waste Cat`, `Tonnes Received`) |> 
    group_by(`Basic Waste Cat`,`Site Name`) |> 
    summarise(tonnes = sum(`Tonnes Received`))
  
  wdi$year <- year
  
  return(wdi)
})

wdi_masons <- bind_rows(wdi_masons)

wdi_masons <- wdi_masons |> 
  filter(year == 2020)

wdi_masons$`Site Name` <- gsub("EPR", "", wdi_masons$`Site Name`)
wdi_masons$`Site Name` <- gsub("BV4517IM", "", wdi_masons$`Site Name`)
wdi_masons$`Site Name` <- gsub("\\/", "", wdi_masons$`Site Name`)
wdi_masons$`Site Name` <- gsub(" - ", "", wdi_masons$`Site Name`)

wdi_masons <- wdi_masons[order(-wdi_masons$tonnes),]

wdi_masons$`Site Name` <- gsub("Folly Farm Waste Management Facility", "Folly Farm", wdi_masons$`Site Name`)
wdi_masons$`Site Name` <- gsub("Shrublands Quarry Landfill", "Shrublands Quarry", wdi_masons$`Site Name`)

p9_barchart <- ggplot(data = wdi_masons, 
                      aes(x = `Site Name`,
                          y = tonnes,
                          fill = `Basic Waste Cat`)) +
  geom_bar(stat = "identity", 
           position = "fill") +
  scc_style() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(title = "Type of Waste Category Managed by each Landfill Site in Suffolk",
       subtitle = paste("Total of", totals_formatting(sum(wdi_masons$tonnes)), "tonnes in 2020")) +
  theme(legend.position = "top", 
        legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

finalise_plot(
  p9_barchart,
  source = "Environment Agency Waste Data Interrogator 2019, 2020 and 2021",
  save_filepath = here("plots", "barchart_Type_of_Waste_Managed_Landfill_Site.png"),
  width_pixels = 800,
  height_pixels = 500
)

# Types of waste received by Masons Landfill
wdi <- read_excel(here("Data", paste0(2020," Waste Received.xlsx"))) %>%
  filter(`Facility WPA` == "Suffolk")

wdi_hshld_site  <- wdi %>%
  filter(`Basic Waste Cat` == "Hhold/Ind/Com") |> 
  select(`Site Name`, `Tonnes Received`) |> 
  group_by(`Site Name`) |> 
  summarise(tonnes = sum(`Tonnes Received`)) |> 
  top_n(5) |> 
  arrange(desc(tonnes))

scc_barchart(wdi_hshld, x = wdi_hshld$`Site Name`, y = wdi_hshld$tonnes, "Hhold/Ind/Com Waste Destination in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

wdi_hshld_cat <- wdi |> 
  filter(`Basic Waste Cat` == "Hhold/Ind/Com") |> 
  select(`Site Category`, `Tonnes Received`) |>
  group_by(`Site Category`) |> 
  summarise(tonnes = sum(`Tonnes Received`))
  
scc_barchart(wdi_hshld_cat, x = wdi_hshld_cat$`Site Category`, y = wdi_hshld_cat$tonnes, "Hhold/Ind/Com Waste Managed Category in Suffolk (tonnes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
scc_piechart(
  wdi_hshld_cat,
  wdi_hshld_cat$tonnes,
  wdi_hshld_cat$`Site Category`,
  title = "Hhold/Ind/Com Waste Managed Category in Suffolk",
  subtitle = paste("Total of", totals_formatting(sum(wdi_hshld_cat$tonnes)), "tonnes in 2020")
)
