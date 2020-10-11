get_links <- function(.tab_row, .col_url, .col_id, .col_ticker) {
  tab_links <- xml2::read_html(dplyr::pull(.tab_row, {{.col_url}})) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    tibble::enframe()
  
  expand_grid(.tab_row, tab_links) %>%
    dplyr::filter(endsWith(value, "pdf")) %>%
    dplyr::mutate(
      year = as.integer(stringi::stri_extract_last_regex(value, "\\d{4}")),
      link = xml2::url_absolute(value, "https://www.annualreports.com"),
      name_orig = gsub(".pdf", "", basename(link)),
      name_save = paste({{.col_id}}, {{.col_ticker}}, year, sep = "_"),
      query_time = Sys.time()
    ) %>% dplyr::select(-value, -name)
}

#' Get Links Report Links from AnnualReport.com
#'
#' @param .tab Dataframe with at least three columns \cr
#' - URL column: Column with main URLs \cr
#' - ID Column: Column with Unique Firm IDs \cr
#' - Ricker Column: Column with unique Firm Ticker \cr
#' @param .col_url The URL column
#' @param .col_id The ID column
#' @param .col_ticker The Ticker column
#' @param .dir The main directory for results
#'
#' @return 
#' - A dataframe with links to global environment and to ".dir/results_links.xlsx"\cr
#' - A dataframe with potentiall errors to ".dir/errors_links.xlsx"
#' @export
get_links_map <- function(.tab, .col_url, .col_id, .col_ticker, .dir) {
  
  # Create Directories
  .dir_store <- dpath(.dir, "store")
  
  # Read Processed Files
  .path_links_res <- file.path(.dir_store, "res.rds")
  if (file.exists(file.path(.path_links_res))) {
    tab_prc_res <- read_rds(.path_links_res)
  } else {
    tab_prc_res <- tibble::tibble({{.col_id}} := NA_character_, .rows = 0)
  }
  
  .path_links_err <- file.path(.dir_store, "err.rds")
  if (file.exists(file.path(.path_links_err))) {
    tab_prc_err <- read_rds(.path_links_err)
  } else {
    tab_prc_err <- tibble::tibble({{.col_id}} := NA_character_, .rows = 0)
  }
  
  # Safe Function
  sget_link <- purrr::safely(get_links)
  
  # Prepare List
  lst_tab <- split(.tab, dplyr::pull(.tab, {{.col_id}}))
  lst_tab <- lst_tab[!names(lst_tab) %in% dplyr::pull(tab_prc_res, {{.col_id}})]
  
  # Map
  lst_links <- purrr::map(
    .x = lst_tab,
    .f = ~ sget_link(.x, {{.col_url}}, {{.col_id}}, {{.col_ticker}})
  ) %>% purrr::transpose()
  
  # Separate Errors
  tab_err <- purrr::compact(lst_links$error) %>%
    tibble::enframe() %>%
    dplyr::mutate(
      message = purrr::map_chr(value, ~.x[[1]]),
      query_time = Sys.time()
    ) %>%
    dplyr::select({{.col_id}} := name, message, query_time) %>%
    dplyr::bind_rows(tab_prc_err)
  
  # Combine Results
  tab_res <- dplyr::bind_rows(tab_prc_res, dplyr::bind_rows(lst_links$result))
  
  # Write Files
  readr::write_rds(tab_res, .path_links_res)
  readr::write_rds(tab_err, .path_links_err)
  
  openxlsx::write.xlsx(tab_res, dpath(.dir, "results_links.xlsx"))
  openxlsx::write.xlsx(tab_err, dpath(.dir, "errors_links.xlsx"))
  
  return(tab_res)
}