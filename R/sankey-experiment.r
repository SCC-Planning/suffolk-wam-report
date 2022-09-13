wdi_import <- read_excel(here("Data", "2020 Waste Received.xlsx")) %>%
    filter(`Facility WPA` == "Suffolk")

wdi_import  <- wdi_import %>%
    select(`Tonnes Received`, `Origin WPA`) %>%
    group_by(`Origin WPA`) %>%
    summarise(tonnes = sum(`Tonnes Received`))

 wdi_import <- wdi_import %>%
    filter(`Origin WPA` != "Suffolk")

wdi_import$target <- "Suffolk"

wdi_export <- read_excel(here("Data", "2020 Waste Received.xlsx")) %>%
    filter(`Origin WPA` == "Suffolk")

wdi_export  <- wdi_export %>%
    select(`Tonnes Received`, `Facility WPA`) %>%
    group_by(`Facility WPA`) %>%
    summarise(tonnes = sum(`Tonnes Received`))

wdi_export <- wdi_export %>%
    filter(`Facility WPA` != "Suffolk")   

wdi_export$source <- "Suffolk"

colnames(wdi_import) <- c("source", "value", "target")
colnames(wdi_export) <- c("target", "value", "source")

wdi <- bind_rows(wdi_import, wdi_export)

relevant_counties <- c(
    "Norfolk",
    "Essex",
    "Cambridgeshire",
    "Peterborough"
)
wdi <- wdi[complete.cases(wdi),]
wdi <- wdi %>%
    filter(value > 100000)

nodes <- data.frame(
    name=c(as.character(wdi$source), 
    as.character(wdi$target)) %>%
     unique()
)

wdi$IDsource <- match(wdi$source, nodes$name)-1 
wdi$IDtarget <- match(wdi$target, nodes$name)-1

p <- sankeyNetwork(Links = wdi, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE)
p
