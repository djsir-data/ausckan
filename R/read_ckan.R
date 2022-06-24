
#' @title Search CKAN Data Repository
#'
#' @param search_term
#' @param ckan_url
#' @param rows
#' @param detailed
#'
#' @return
#' @export
#'
#' @examples
search_ckan <- function(search_term, ckan_url, rows = 10, detailed = FALSE){

  #ckanr::ckanr_setup(url = ckan_url)

  results <- ckanr::package_search(search_term,
                                   rows = rows,
                                   as = 'table',
                                   url = ckan_url)

  if (detailed) {
    return(results)
  } else {

    org <- results$results$organization |>
      dplyr::select(org_id = id, title, type)

    metadata <- results$results |>
      dplyr::select(id, name, notes, metadata_created)

    resources <- dplyr::bind_rows(results$results$resources) |>
      dplyr::select(id = package_id, created, url)

    out <- dplyr::bind_cols(metadata, org) |>
      dplyr::left_join(resources) |>
      dplyr::mutate(ext = tools::file_ext(url),
                    src = ckan_url)

    return(out)

  }

}





#' @rdname search_ckan
search_ckan_vic <- function(search_term, ...){
  ckan <- "https://discover.data.vic.gov.au/"
  search_ckan(search_term, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_nsw <- function(search_term, ...){
  ckan <- "https://data.nsw.gov.au/data/"
  search_ckan(search_term, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_aus <- function(search_term, ...){
  ckan <- "https://data.gov.au/data/"
  search_ckan(search_term, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_qld <- function(search_term, ...){
  ckan <- "https://data.qld.gov.au/"
  search_ckan(search_term, ckan_url = ckan, ...)
}

#' @rdname search_ckan
search_ckan_sa <- function(search_term, ...){
  ckan <- "https://data.sa.gov.au/data"
  search_ckan(search_term, ckan_url = ckan, ...)
}

