#' Get waste between wpas
#' 
#' @param years_of_interest
#' @param origin the origin county/wpa of the waste
#' @param destination the destination county/wpa of the waste
get_waste_between_wpas <- function(
    years_of_interest,
    origin,
    destination
) {
  wdi <- lapply(years_of_interest, function(year) {
    wdi <- readxl::read_excel(here("Data", paste0(year," Waste Received.xlsx"))) %>%
      dplyr::filter(`Origin WPA` == origin & `Facility WPA` == destination)
    
    totals  <- wdi %>%
      dplyr::group_by(`Site Name`, `Operator`) %>%
      dplyr::summarise(tonnes = sum(`Tonnes Received`))
    
    totals$year <- year
    
    return(totals)
  })
  
  wdi <- dplyr::bind_rows(wdi)
  wdi <- wdi[order(-wdi$tonnes),]
}