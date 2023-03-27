#' Obtain the wdi totals for relevant years
#' 
#' @param wdi_data read in WDI data 
#' @param county county of interest
get_wdi_totals <- function(wdi_data, county) {
  wdi <- wdi_data|>
    dplyr::filter(`Facility WPA` == county)
  
  totals  <- wdi |>
    dplyr::select(`Site Category`, `Tonnes Received`, `year`) |>
    dplyr::group_by(`Site Category`, `year`) |>
    dplyr::summarise(tonnes = sum(`Tonnes Received`))
  
  return(totals)
}

#' Obtain the wdi totals for relevant years with sitename
#' 
#' @param wdi_data read in WDI data 
#' @param county county of interest
get_wdi_totals_by_site <- function(wdi_data, county) {
  wdi <- wdi_data |>
    dplyr::filter(`Facility WPA` == county)
  
  totals_by_site  <- wdi |>
    dplyr::select(`Site Category`, `Site Name`, `Tonnes Received`, `year`) |> 
    dplyr::group_by(`Site Category`, `Site Name`, `year`) |> 
    dplyr::summarise(tonnes = sum(`Tonnes Received`), .groups = "drop")
  
  
  return(totals_by_site)
}
