#' Get waste between wpas
#' 
#' @param wdi_data read in WDI data 
#' @param origin the origin county/wpa of the waste
#' @param destination the destination county/wpa of the waste
get_waste_between_wpas <- function(wdi_data, origin, destination) {
  wdi <- wdi_data |> 
    dplyr::filter(`Origin WPA` == origin & `Facility WPA` == destination)
  
  totals_between_wpas  <- wdi |> 
    dplyr::group_by(`Site Name`, `Operator`, `year`) |> 
    dplyr::summarise(tonnes = sum(`Tonnes Received`))
  
  totals_between_wpas <- totals_between_wpas[order(-totals_between_wpas$tonnes),]
  return(totals_between_wpas)
}

#' Get waste origin
#' 
#' @param wdi_data read in WDI data 
#' @param county county of interest
get_waste_origin <- function(wdi_data, county) {
  wdi_origin <- wdi_data |> 
    dplyr::filter(`Facility WPA` == county)
  
  wdi_origin  <- wdi_origin |> 
    dplyr::select(`Tonnes Received`, `Origin WPA`, `year`) |> 
    dplyr::group_by(`Origin WPA`, `year`) |> 
    dplyr::summarise(tonnes = sum(`Tonnes Received`))
  
  return(wdi_origin)
}

#' Get waste destination
#' 
#' @param wdi_data read in WDI data 
#' @param county county of interest
get_waste_destination <- function(wdi_data, county) {
  wdi_export <- wdi_data |> 
    dplyr::filter(`Origin WPA` == county & `Facility WPA` != county)
  
  wdi_export  <- wdi_export |>
    dplyr::select(`Tonnes Received`, `Facility WPA`, `year`) |>
    dplyr::group_by(`Facility WPA`, `year`) |>
    dplyr::summarise(tonnes = sum(`Tonnes Received`))
  
  return(wdi_export)
}
