#' Obtain the wdi totals for relevant years
#' 
#' @param years_of_interest the years WDI totals are required for, make sure
#' you have the relevant files in the Data folder
get_wdi_totals <- function(years_of_interest) {
  wdi_totals <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here::here("Data", paste0(year," Waste Received.xlsx"))) |>
      dplyr::filter(`Facility WPA` == "Suffolk")
    
    totals  <- wdi |>
      dplyr::select(`Site Category`, `Tonnes Received`) |>
      dplyr::group_by(`Site Category`) |>
      dplyr::summarise(tonnes = sum(`Tonnes Received`))
    
    totals$year <- year
    
    return(totals)
  })
  
  wdi_totals <- dplyr::bind_rows(wdi_totals)
  return(wdi_totals)
}

#' Obtain the wdi totals for relevant years with sitename
#' 
#' @param years_of_interest the years WDI totals are required for, make sure
#' you have the relevant files in the Data folder
get_wdi_totals_by_site <- function(years_of_interest) {
  wdi_totals <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here::here("Data", paste0(year," Waste Received.xlsx"))) |>
      dplyr::filter(`Facility WPA` == "Suffolk")
    
    totals  <- wdi |>
      dplyr::select(`Site Category`, `Site Name`, `Tonnes Received`) |> 
      dplyr::group_by(`Site Category`, `Site Name`) |> 
      dplyr::summarise(tonnes = sum(`Tonnes Received`), .groups = "drop")
    
    totals$year <- year
    
    return(totals)
  })
  
  wdi_totals <- dplyr::bind_rows(wdi_totals)
  return(wdi_totals)
}