#'Get Remaining Landfill Capacity
#'
#' @param rlc_data read in RLC data
#' @param county county of interest
get_rlc_capacity <- function(rlc_data, county) {
  rlc_data$Operator <- gsub("Limited", "Ltd", rlc_data$Operator)
  rlc_data$Operator <- gsub("Aggmax Transport Ltd", "Aggmax Ltd", rlc_data$Operator)
  rlc_data$Facility <- gsub("Shrublands Quarry Landfill", "Shrublands Quarry", rlc_data$Facility)
  rlc_data$`Site Type` <- gsub("L05: Inert Landfill", "L05 - Inert Landfill", rlc_data$`Site Type`)
  
  capacity <- rlc_data |> 
    dplyr::filter(`Local Authority` == "Suffolk" & rlc_data$`Remaining Capacity (cubic metres)` != 0) |> 
    tidyr::pivot_wider(
      names_from = year,
      values_from = `Remaining Capacity (cubic metres)`
    ) |> 
    dplyr::select(-`Local Authority`)
  
  return(capacity)
}