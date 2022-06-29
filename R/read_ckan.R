#' @title Search CKAN Data Repository
#'
#' @param search_term character words for search, use multiple word to improve results
#' @param file_type character c("all", "msoffice"), use to restrict files shown to msoffice compatible files
#' @param ckan_url character url for ckan api
#' @param rows integer number of rows/results to return from api
#' @param detailed logical full ckanr result or simple output
#' @param ... variables passed to package_search
#'
#' @import ckanr
#' @importFrom glue glue
#' @importFrom dplyr select mutate left_join bind_cols
#' @importFrom rlang .data .env
#'
#' @return
#' @export
#'
#' @examples
search_ckan <- function(search_term, ckan_url, rows = 10, file_type, detailed = FALSE, ...){

  #ckanr::ckanr_setup(url = ckan_url)

  results <- ckanr::package_search(search_term,
                                   rows = rows,
                                   as = 'table',
                                   url = ckan_url,
                                   ...)


  if (results$count == 0) {
    warning('no results found')
    return(NULL)
  } else if (results$count > nrow(results$results)) {
    warning(glue::glue('{results$count} records found but only {rows} returned'))
    message("refine your search term or increase number of rows returned")
  }

  if (detailed) {
    return(results)
  } else {

    org <- results$results$organization |>
      dplyr::select(org_id = .data$id, .data$title, .data$type)

    metadata <- results$results |>
      dplyr::select(.data$id, .data$name, .data$notes) |>
      dplyr::mutate(package_name = .data$name) |>
      dplyr::select(-c(.data$name))

    resources <- dplyr::bind_rows(results$results$resources) |>
      dplyr::select(id = .data$package_id, .data$created, .data$url, .data$name)|>
      dplyr::mutate(file_name = .data$name) |>
      dplyr::select(-c(.data$name))

    out <- dplyr::bind_cols(metadata, org) |>
      dplyr::left_join(resources, by = c("id")) |>
      dplyr::mutate(ext = tolower(tools::file_ext(.data$url)),
                    src = .env$ckan_url) |>
      dplyr::select(.data$title, .data$package_name, .data$file_name, .data$url, everything(), -c(.data$id, .data$org_id, .data$created, .data$type)) |>
      dplyr::arrange(.data$package_name, .data$file_name)

    if(file_type == "msoffice"){
      out <- out |>
        dplyr::filter(ext=="docx"|ext=="xlsx"|ext=="xls"|ext=="csv"|ext=="pptx")
      warning("returned files filtered to only msoffice files")
      }

    rcount<-nrow(out)

    return(out)

  }



}



#' @rdname search_ckan
search_ckan_vic <- function(search_term, file_type = "all",...){
  ckan <- "https://discover.data.vic.gov.au/"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_nsw <- function(search_term, file_type = "all", ...){
  ckan <- "https://data.nsw.gov.au/data/"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_aus <- function(search_term, file_type = "all", ...){
  ckan <- "https://data.gov.au/data/"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_qld <- function(search_term, file_type = "all", ...){
  ckan <- "https://data.qld.gov.au/"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_sa <- function(search_term, file_type = "all", ...){
  ckan <- "https://data.sa.gov.au/data"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_wa <- function(search_term, file_type = "all", ...){
  ckan <- "https://catalogue.data.wa.gov.au/"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_nt <- function(search_term, file_type = "all", ...){
  ckan <- "https://data.nt.gov.au/"
  search_ckan(search_term, file_type, ckan_url = ckan, ...)
}







