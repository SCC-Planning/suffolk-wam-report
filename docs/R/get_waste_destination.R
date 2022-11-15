#' Get waste destination
#' 
#' @param years_of_interest the years WDI totals are required for, make sure
#' you have the relevant files in the Data folder
get_waste_destination <- function(years_of_interest) {
  wdi_export <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here::here("Data", paste0(year," Waste Received.xlsx"))) |>
      dplyr::filter(`Origin WPA` == "Suffolk" & `Facility WPA` != "Suffolk")
    
    totals  <- wdi |>
      dplyr::select(`Tonnes Received`, `Facility WPA`) |>
      dplyr::group_by(`Facility WPA`) |>
      dplyr::summarise(tonnes = sum(`Tonnes Received`))
    
    totals$year <- year
    
    return(totals)
  })
  
  wdi_export <- dplyr::bind_rows(wdi_export)
  return(wdi_export)
}