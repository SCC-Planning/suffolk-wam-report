#' Read WDI
#'   
#' Read in all WDI files
#' 
#' @param folder data folder with files.
read_wdi <- function(folder) {
  wdi_files <- list.files(folder)
  
  wdi_files <- str_subset(wdi_files, "Waste Received")
  wdi_files <- paste0(folder, wdi_files)
  
  wdi <- lapply(wdi_files, function(file_to_read) {
    wdi <- readxl::read_excel(file_to_read)
    
    # Remove file names to extract year
    file_to_read <- gsub(folder, "", file_to_read)
    file_to_read <- gsub(" Waste Received.xlsx", "", file_to_read)
    
    wdi$year <- file_to_read
    
    return(wdi)
  })
  
  wdi <- dplyr::bind_rows(wdi)
  return(wdi)
}
