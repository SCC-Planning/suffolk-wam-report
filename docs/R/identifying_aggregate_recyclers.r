# Waste Data Interrogator
# Identifying active aggregate recycling sites
library(readxl)
library(dplyr)
library(stringr)

df_wdi_all <- read_excel("~\\Waste Data\\Data\\Waste Received.xlsx") %>%
  filter(`Facility WPA` == "Suffolk")

barton <- df_wdi_all %>%
  filter(grepl("Barton Mills", `Site Name`))

unique(barton$`Site Category`)

sheet_list <- c(
  # Found
  "Bolton Brothers",
  # Found
  "Spall",
  # Not found
  "Sheepdrift",
  # Not found
  "Sinks Pit",
  # Not found
  "Broomfield Pit",
  # Not found
  "Gazeley",
  # Not Found
  "Marston",
  # Found
  "Chicory",
  # Found,
  "sole bay",
  # Not found
  "Newmarket",
  # Lakenheath recycling facility, but not causeway pit
  "Causeway Pit",
  # Not found - not sure seems to exist on satellite imagery but can't find an operator name
  "Somersham Road",
  # Not found
  "Harpers Hill",
  # Not found
  "Beccles",
  # Not found - we do have chilton airfield
  "Chilton Grove",
  # This is Masons landfill
  "Claydon Skips",
  # Not found
  "Ellough",
  # Found
  "Barton Mills",
  # Found
  "Lawn Farm",
  "Wetherden",
  # This is Bentwaters Park
  "Control tower"
)

check_name <- df_wdi_all %>%
  filter(grepl(paste0(sheet_list, collapse = "|"), `Site Name`, ignore.case = TRUE)) %>%
  select(`Site Name`, `Operator`, `SOC Sub Category`) %>%
  unique()

check_operator <- df_wdi_all %>%
  filter(grepl(paste0(sheet_list, collapse = "|"), `Operator`, ignore.case = TRUE)) %>%
  select(`Site Name`, `Operator`, `SOC Sub Category`) %>%
  unique()

check <- bind_rows(check_operator, check_name) %>%
  unique()

non_mineral <- check %>%
  group_by(`Site Name`, `Operator`) %>%
  mutate(`Mineral Categories` = paste(`SOC Sub Category`, collapse = ", ")) %>%
  select(-`SOC Sub Category`) %>%
  unique()

write.csv(non_mineral, "~\\Waste Data\\Output\\Aggregate Recyclers non mineral.csv", row.names = FALSE)


df_wdi_received <- read_excel("~\\Waste Data\\Data\\Waste Received.xlsx") %>%
  filter(`Facility WPA` == "Suffolk") %>%
  filter(`SOC Category` == "12 - Mineral wastes") %>%
  filter(`Site Category` %in% c("MRS", "Treatment", "Transfer", "On/In Land")) %>%
  select(
    `Site Name`,
    `Operator`,
    # `EWC Waste Desc`,
    # `Permit`,
    `SOC Sub Category`
  ) %>%
  unique()

df_wdi_removed <- read_excel("~\\Waste Data\\Data\\Waste Removed.xlsx") %>%
  filter(`Facility WPA` == "Suffolk") %>%
  filter(`SOC Category` == "12 - Mineral wastes") %>%
  filter(`Site Category` %in% c("MRS", "Treatment", "Transfer", "On/In Land")) %>%
  select(
    `Site Name`,
    `Operator`,
    # `EWC Waste Desc`,
    # `Permit`,
    `SOC Sub Category`
  ) %>%
  unique()

df_wdi_recycling_aggregates <- bind_rows(df_wdi_received, df_wdi_removed) %>%
  unique()

# Collapse all the SOC sub categories into a single row of text for each site/operator
df_wdi_recycling_aggregates <- df_wdi_recycling_aggregates %>%
  group_by(`Site Name`, `Operator`) %>%
  mutate(`Mineral Categories` = paste(`SOC Sub Category`, collapse = ", ")) %>%
  select(-`SOC Sub Category`) %>%
  unique()

df_wdi_recycling_aggregates <- df_wdi_recycling_aggregates[order(df_wdi_recycling_aggregates$`Site Name`), ]

write.csv(df_wdi_recycling_aggregates, "~\\Waste Data\\Output\\Aggregate Recyclers2.csv", row.names = FALSE)




# Obsolete code below?
df_wdi_removed <- read_excel("~\\Waste Data\\Data\\Waste Removed.xlsx") %>%
  filter(`Facility WPA` == "Suffolk") %>%
  filter(`SOC Category` == "12 - Mineral Wastes")
filter(`Site Category` %in% c("MRS", "Treatment", "Transfer"))

write.csv(df_wdi_removed, "~\\Waste Data\\Output\\Waste Removed.csv", row.names = FALSE)

df_wdi_received <- read_excel("~\\Waste Data\\Data\\Waste Received.xlsx") %>%
  filter(`Facility WPA` == "Suffolk") %>%
  filter(`Site Category` %in% c("MRS", "Treatment", "Transfer"))

write.csv(df_wdi_received, "~\\Waste Data\\Output\\Waste Received.csv", row.names = FALSE)

# Collapsing these lists
df_wdi_collapsed <- df_wdi_removed %>%
  select(`Site Name`, `Operator`, `SOC Category`)

unique(df_wdi_collapsed)
