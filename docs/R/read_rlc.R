#' Read RCL
#'   
#' Read in all Remaining Landfill Capacity files
#' 
#' @param folder data folder with files.
read_rcl <- function(folder) {
  rlc_files <- list.files(folder)
  
  rlc_files <- str_subset(rlc_files, "Landfill")
  rlc_files <- paste0(folder, rlc_files)
  
  rlc <- lapply(rlc_files, function(file_to_read) {
    capacity <- readxl::read_excel(file_to_read, skip = 7)
    # Select column by number because names vary by year
    capacity <- capacity[, c(2, 3, 9, 10)]
    
    colnames(capacity) <- c(
      "Operator",
      "Facility",
      "Site Type",
      "Remaining Capacity (cubic metres)"
    )
    
    # Remove file names to extract year
    file_to_read <- gsub(folder, "", file_to_read)
    file_to_read <- gsub("_Remaining_Landfill_Capacity_v2.xlsx", "", file_to_read)
    file_to_read <- gsub("_Remaining_Landfill_Capacity.xlsx", "", file_to_read)
    
    capacity$year <- file_to_read
    
    return(capacity)
  })
  
  rlc <- bind_rows(rlc)
}

