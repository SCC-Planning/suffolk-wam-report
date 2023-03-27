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
