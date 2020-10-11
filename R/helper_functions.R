lft <- function(dirs, reg = "*", id = "doc_id", rec = FALSE, info = FALSE) {
  tab_fil <- purrr::map_dfr(
    .x = dirs,
    .f = ~tibble::tibble(path = list.files(.x, reg, F, T, rec))
  ) %>% dplyr::mutate(
    file_ext = paste0(".", tools::file_ext(path)),
    !!id    := stringi::stri_replace_last_fixed(basename(path), file_ext, "")
  ) %>% dplyr::select(!!id, file_ext, path)
  
  if (info) {
    tab_fil <- dplyr::bind_cols(tab_fil, tibble::as.tibble(file.info(tab_fil$path)))
  }
  
  return(tab_fil)
}
dpath <- function(..., type = c("f", "d")) {
  type <- match.arg(type)
  path <- file.path(...)
  if (type == "f") dir = dirname(path)
  if (type == "d") dir = path
  for (i in unique(dir)) if (!dir.exists(i)) dir.create(i, FALSE, TRUE)
  return(path)
}
standardize_phrase <- function(.phrases) {
  .phrases <- stringi::stri_replace_all_regex(.phrases, "[[:punct:]]", " ")
  .phrases <- gsub("\\s+", " ", .phrases)
  tolower(trimws(.phrases))
}