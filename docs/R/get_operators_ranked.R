#' Obtain the wdi totals for relevant years
#' 
#' @param years_of_interest the years WDI totals are required for, make sure
#' you have the relevant files in the Data folder
get_operators_ranked <- function(years_of_interest) {
  operators_ranked <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here::here(
      "Data", paste0(year," Waste Received.xlsx")
    )) |>
      dplyr::filter(`Facility WPA` == "Suffolk")
    
    operator  <- wdi |>
      dplyr::select(`Site Category`, `Tonnes Received`, `Site Name`) |>
      dplyr::filter(!`Site Category` %in% c("Storage", "Mobile Plant")) |>
      dplyr::group_by(`Site Category`, `Site Name`) |>
      dplyr::summarise(tonnes = sum(`Tonnes Received`))
    
    operators_ranked <- lapply(unique(operator$`Site Category`), function(category) {
      df <- operator |>
        dplyr::filter(`Site Category` == category)
      
      df <- df[order(-df$tonnes),]
      return(df)
    })
    
    operators_ranked <- dplyr::bind_rows(operators_ranked)
    operators_ranked$year <- year
    return(operators_ranked)
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