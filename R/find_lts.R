find_lts_page <- function(.fst, .phrases) {
  .phrases <- standardize_phrase(.phrases)
  
  tab <- fst::read_fst(.fst) %>%
    dplyr::mutate(text = standardize_phrase(text)) %>%
    dplyr::filter(text %in% .phrases) %>%
    dplyr::distinct(page)
  
  if (nrow(tab) != 1) {
    tab <- tibble::tibble(page = NA_integer_)
  } else {
    tab <- select(tab, page)
  }
  
  return(tab)
  
  
}

#' Find start page for Letter to Shareholders
#'
#' @param .dir The main directory for results
#' @param .phrases A character vector with phrases
#'
#' @return A datframe with the page number of the start page of the Letter to shareholder section, saved to ".dir/letter_to_shareholder_pages.xlsx"
#' @export
find_lts_page_map <- function(.dir, .phrases) {
  
  fil_fst <- lft(file.path(.dir, "fst"))
  
  .path_store <- dpath(.dir, "letter_to_shareholder_pages.xlsx")
  
  tab <- map_dfr(
    .x = set_names(fil_fst$path, fil_fst$doc_id), 
    .f = ~ find_lts_page(.x, .phrases), 
    .id = "doc_id"
  )
  
  openxlsx::write.xlsx(tab, .path_store)
}

#' Extract Letter to shareholder
#'
#' @param .tab A Dataframe with three columns: \cr
#' - doc_id: Filename of the PDF\cr
#' - start: The start page of the Letter section\cr
#' - stop: The end page of the Letter section
#' @param .dir The main directory for results
#' @param .default_length If stop is empty, the number of pages to be extracted counting from start
#'
#' @return
#' PDFs and text files saved to ".dir/lts"
#' @export
extract_lts <- function(.tab, .dir, .default_length = 3) {
  if (any(is.na(.tab[["start"]]))) stop("Start Column MUST NOT be NA", call. = FALSE)
  
  .fil_pdf <- lft(file.path(.dir, "pdf")) %>%
    dplyr::select(doc_id, path_in_pdf = path) %>%
    dplyr::mutate(path_out_pdf = dpath(.dir, "lts", "pdf", basename(path_in_pdf)))
  
  .fil_fst <- lft(file.path(.dir, "fst")) %>%
    dplyr::select(doc_id, path_in_fst = path) %>%
    dplyr::mutate(path_out_fst = dpath(.dir, "lts", "txt", basename(path_in_fst)))
  
  tab <- .tab %>%
    dplyr::left_join(.fil_pdf, by = "doc_id") %>%
    dplyr::left_join(.fil_fst, by = "doc_id") 
  
  
  for (i in 1:nrow(tab)) {
    start <- tab[["start"]][i]
    stop  <- tab[["stop"]][i]
    if (is.na(stop)) {
      stop <- start + .default_length
    }
    
    if (!file.exists(tab[["path_out_pdf"]][i])) {
      pdftools::pdf_subset(
        input = tab[["path_in_pdf"]][i],
        pages = start:stop,
        output = tab[["path_out_pdf"]][i]
      )
    }
    
    if (!file.exists(tab[["path_out_fst"]][i])) {
      txt <- fst::read_fst(tab[["path_in_fst"]][i]) %>%
        dplyr::filter(page %in% start:stop)
      
      openxlsx::write.xlsx(txt, gsub(".fst$", ".xlsx", tab[["path_out_fst"]][i]))
      write(paste(txt$text, collapse = "\r\n"), gsub(".fst$", ".txt", tab[["path_out_fst"]][i]))
    }
    
  }
}
