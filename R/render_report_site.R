#' Render the wamreport as a website
#' 
#' @param wdi_data read in WDI data 
#' @param lacw_data read in the LACW data
#' @param rlc_data read in the RLC data
#' 
render_wam_report_site <- function(wdi_data, lacw_data, rlc_data) {
  # Report parameters
  # County of interest
  county <- "Suffolk"
  count_la <- "Suffolk County Council"
  origin <- county
  
  # Destination county for breakdown of report
  destination <- "Norfolk"
  
  # Creating a list of data outputs
  wamreport <- list(
    rlc = rlc_data,
    lacw = lacw_data,
    wdi_all = wdi_data,
    wdi_totals = get_wdi_totals(wdi_data, county),
    totals_by_site = get_wdi_totals_by_site(wdi_data, county),
    operators_ranked = get_operators_ranked(wdi_data, county),
    landfill_waste = get_landfill_waste(wdi_data, county),
    od_waste = get_waste_between_wpas(wdi_data, origin, destination),
    waste_origin = get_waste_origin(wdi_data, county),
    waste_destination = get_waste_destination(wdi_data, county),
    rlc_capacity = get_rlc_capacity(rlc_data, county)
  )
  
  rmarkdown::render_site()
}
