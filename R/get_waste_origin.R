#' Get waste origin
#' 
#' @param years_of_interest the years WDI totals are required for, make sure
#' you have the relevant files in the Data folder
get_waste_origin <- function(years_of_interest) {
  wdi_origin <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here::here("Data", paste0(year," Waste Received.xlsx"))) %>%
      dplyr::filter(`Facility WPA` == "Suffolk")
    
    totals  <- wdi %>%
      dplyr::select(`Tonnes Received`, `Origin WPA`) %>%
      dplyr::group_by(`Origin WPA`) %>%
      dplyr::summarise(tonnes = sum(`Tonnes Received`))
    
    totals$year <- year
    
    return(totals)
  })
  
  wdi_origin <- bind_rows(wdi_origin)
  return(wdi_origin)
}