#' Get landfill waste
#' 
#' @param wdi_data read in WDI data 
#' @param county county of interest
get_landfill_waste <- function(wdi_data, county) {
  wdi <- wdi_data |> 
    dplyr::filter(`Facility WPA` == county & `Site Category` == "Landfill")
  
  landfill_totals  <- wdi |>
    dplyr::select(`Tonnes Received`, `Origin WPA`, `year`) |>
    dplyr::group_by(`Origin WPA`, `year`) |>
    dplyr::summarise(tonnes = sum(`Tonnes Received`))
  
  return(landfill_totals)
}