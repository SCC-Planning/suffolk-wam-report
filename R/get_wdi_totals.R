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
  
  return(wdi_totals)
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

#' Obtain the wdi totals for all operators
#' 
#' @param wdi_data read in WDI data 
#' @param county county of interest
get_operators_ranked <- function(wdi_data, county) {
  wdi <- wdi_data |>
    dplyr::filter(`Facility WPA` == county)
  
  operator  <- wdi |>
    dplyr::select(`Site Category`, `Tonnes Received`, `Site Name`, `year`) |>
    dplyr::filter(!`Site Category` %in% c("Storage", "Mobile Plant")) |>
    dplyr::group_by(`Site Category`, `Site Name`, `year`) |>
    dplyr::summarise(tonnes = sum(`Tonnes Received`))
  
  operators_ranked <- lapply(unique(operator$`Site Category`), function(category) {
    df <- operator |>
      dplyr::filter(`Site Category` == category)
    
    df <- df[order(-df$tonnes),]
    return(df)
  })
  
  operators_ranked <- dplyr::bind_rows(operators_ranked)
  
  operators_ranked <- tidyr::pivot_wider(
    operators_ranked,
    names_from = year,
    values_from = tonnes,
    values_fill = 0
  )
  
  return(operators_ranked)
}

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

#' Get waste between wpas
#' 
#' @param wdi_data read in WDI data 
#' @param origin the origin county/wpa of the waste
#' @param destination the destination county/wpa of the waste
get_waste_between_wpas <- function(wdi_data, origin, destination) {
  wdi <- wdi_data |> 
    dplyr::filter(`Origin WPA` == origin & `Facility WPA` == destination)
  
  totals_between_wpas  <- wdi %>%
    dplyr::group_by(`Site Name`, `Operator`, `year`) %>%
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
