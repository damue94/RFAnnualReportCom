#' Download PDF from Annualreport.com
#'
#' @param .tab A Dataframe created by get_links_map()
#' @param .dir The main directory for results
#' @param .wait_time Mean wait time between downloads (follows normal distribution with mean = .wait_time and sd = .2 * .wait_time)
#' @param .mode Download mode, default is "wb", see download.file() for more information
#'
#' @return 
#' - PDFs will be downloaded to ".dir/pdf"\cr
#' - Datframe saved as .xlsx that indicates success (".dir/results_download.xlsx)
#' @export
download_pdf <- function(.tab, .dir, .wait_time = 5, .mode = "wb") {
  
  for (i in 1:nrow(.tab)) {
    .path_pdf <- dpath(.dir, "pdf", paste0(.tab[["name_save"]][i], ".pdf"))
    if (file.exists(.path_pdf)) next
    try(download.file(
      url      = .tab[["link"]][i], 
      destfile = .path_pdf,
      mode     = .mode
    ), silent = TRUE)
    
    Sys.sleep(rnorm(1, .wait_time, 0.2 * .wait_time))
  }
  
  prc <- gsub(".pdf", "", list.files(file.path(.dir, "pdf"), "pdf$"))
  tab <- dplyr::mutate(.tab, success = as.integer(name_save %in% prc))
  
  openxlsx::write.xlsx(tab, file.path(.dir, "results_download.xlsx"))
  
}