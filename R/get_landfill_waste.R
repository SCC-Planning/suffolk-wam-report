#' Get landfill waste
#' 
#' @param years_of_interest the years WDI totals are required for, make sure
#' you have the relevant files in the Data folder
get_landfill_waste <- function(years_of_interest) {
  wdi_landfill <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here::here("Data", paste0(year," Waste Received.xlsx"))) |>
      dplyr::filter(`Facility WPA` == "Suffolk" & `Site Category` == "Landfill")
    
    totals  <- wdi |>
      dplyr::select(`Tonnes Received`, `Origin WPA`) |>
      dplyr::group_by(`Origin WPA`) |>
      dplyr::summarise(tonnes = sum(`Tonnes Received`))
    
    totals$year <- year
    
    return(totals)
  })
  
  wdi_landfill <- bind_rows(wdi_landfill)
}