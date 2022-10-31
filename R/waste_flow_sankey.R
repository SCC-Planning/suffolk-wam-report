library(networkD3)
wdi <- read_excel(here("Data", paste0(2020," Waste Received.xlsx"))) %>%
  filter(`Facility WPA` == "Suffolk")

wdi_export <- read_excel(here("Data", paste0(2020," Waste Received.xlsx")))

wdi$`Site Name` <- gsub("Masons Landfill - EPR/BV4517IM", "Masons Landfill", wdi$`Site Name`)
wdi$`Site Name` <- gsub("Eye Power Station EPR/BP3635LA", "Eye Power Station", wdi$`Site Name`)
wdi$`Site Name` <- gsub("Folly Farm Waste Management Facility", "Folly Farm", wdi$`Site Name`)
wdi$`Site Name` <- gsub("SUEZ Suffolk Energy from Waste Facility  EPR/WP3438HZ", "SUEZ Suffolk Energy from Waste Facility", wdi$`Site Name`)

links <- wdi |> 
  select(
    source = `Site Category`,
    target = `Site Name`,
    value = `Tonnes Received`
  ) |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

all_waste <- wdi |> 
  select(
    source = `Origin WPA`,
    target = `Basic Waste Cat`,
    value = `Tonnes Received`
  ) |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

site_cat <- wdi |> 
  select(
    source = `Basic Waste Cat`,
    target = `Site Category`,
    value = `Tonnes Received`
  ) |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

export <- wdi_export |> 
  filter(`Origin WPA` == "Suffolk" & `Facility WPA` != "Suffolk") |> 
  select(
    source = `Origin WPA`,
    target = `Facility WPA`,
    value = `Tonnes Received`
  ) |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

export$target[export$value < 100000] <- "Other Export"

export <- export |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

export$target <- gsub("Norfolk", "Norfolk Export", export$target)
export$target <- gsub("Essex", "Essex Export", export$target)

all_waste$source[all_waste$value < 100000] <- "Other Regions"

links$target[links$value < 100000] <- "Other Sites"

links$target[links$source == "Treatment"] <- "Other Sites"

all_waste <- all_waste |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

links <- links |> 
  group_by(source, target) |> 
  summarise(value = sum(value))

links <- bind_rows(links, all_waste)
links <- bind_rows(links, site_cat)
links <- bind_rows(links, export)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Add a 'group' column to each connection:
links$group <- "type_a"
links$group[links$target == "Masons Landfill"] <- "type_b"
links$group[links$target == "Landfill"] <- "type_b"
links$group[links$target == "Hhold/Ind/Com" & links$source != "Hazardous"] <- "type_b"
links$group[links$target == "Inert/C+D"] <- "type_b"
links$group[links$target == "Hazardous"] <- "type_b"

links$group <- as.factor(links$group)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("my_unique_group"))

"#e2eefa", "#2d6ca2", "#e8850c"
# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#e2eefa", "#e8850c", "grey"])'

font <- "Source Sans Pro"
get_scc_font()

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", fontSize = 36, fontFamily = font,
                   colourScale=my_color, LinkGroup="group", NodeGroup="group")
p
saveNetwork(p, here("plots", "Waste_Flow_to_Masons.html"))
