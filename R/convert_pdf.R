pdf_to_txt_line <- function(.dir, .pdf) {
  tab <- tibble::tibble(text = pdftools::pdf_text(.pdf)) %>%
    dplyr::mutate(page = dplyr::row_number()) %>%
    tidytext::unnest_tokens(
      output   = text,
      input    = text,
      token    = stringi::stri_split_regex,
      pattern  = "\r|\n",
      to_lower = FALSE
    ) %>%
    dplyr::filter(!text == "") %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::select(page, row, text)
  
  fst::write_fst(tab, dpath(.dir, "fst", gsub(".pdf", ".fst", basename(.pdf))))
  return(NULL)
}


#' Convert PDF to FST (line based)
#'
#' @param .dir The main directory for results
#'
#' @return FST files saved to to ".dir/fst"
#' @export
convert_pdf <- function(.dir) {
  fil_pdf <- lft(file.path(.dir, "pdf")) %>%
    dplyr::rename(path_pdf = path) %>%
    dplyr::mutate(path_fst = dpath(.dir, "fst", paste0(doc_id, ".fst")))
  
  prc <- lft(file.path(.dir, "fst"))
  
  fil_pdf <- dplyr::filter(fil_pdf, !doc_id %in% prc[["doc_id"]])
  
  spdf_to_txt_line <- purrr::safely(pdf_to_txt_line)
  
  lst <- purrr::map(
    .x = set_names(fil_pdf$path_pdf, fil_pdf$doc_id), 
    .f = ~ spdf_to_txt_line(.dir, .x)
  ) %>% purrr::transpose()
  
  err <- purrr::compact(lst$error) %>%
    tibble::enframe() %>%
    dplyr::mutate(
      message = map_chr(value, ~.x[[1]]),
      query_time = Sys.time()
    ) %>%
    dplyr::select(doc_id = name, message, query_time)
  
  if (nrow(err) > 0) {
    openxlsx::write.xlsx(err, dpath(.dir, "errors_text_concersion.xlsx"))
  }
  
}