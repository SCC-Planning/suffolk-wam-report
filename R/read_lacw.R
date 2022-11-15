#' Read lacw
#'   
#' Read in all lacw files
#' 
#' @param folder data folder with files.
read_lacw <- function(folder) {
  lacw <- readODS::read_ods(
    paste0(folder,"LA_and_Regional_Spreadsheet_2021_rev.ods"),
    sheet = "Table_1",
    skip = 3
  )
  return(lacw)
}
