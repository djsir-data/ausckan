
#' @title Search CKAN Data Repository
#'
#' @param search_term character words for search, use multiple word to improve results
#' @param ckan_url character url for ckan api
#' @param rows integer number of rows/results to return from api
#' @param detailed logical full ckanr result or simple output
#' @param ... variables passed to package_search
#'
#' @import ckanr
#' @importFrom glue glue
#' @importFrom dplyr select mutate left_join bind_cols
#' @importFrom rlang .data .env
#' @importFrom purrr map_dfr
#'
#' @return
#' @export
#'
#' @examples
search_ckan <- function(search_term, ckan_url, rows = 10, detailed = FALSE, ...){

  suppressWarnings({
    results <- ckanr::package_search(search_term,
                                     rows = rows,
                                     as = 'table',
                                     url = ckan_url,
                                     ...)
  })

  if (results$count == 0) {
    warning('no results found')
    return(data.frame())
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
      dplyr::select(.data$id, .data$name, .data$notes, .data$metadata_created)

    resources <- results$results$resources |>
      map_dfr(~ dplyr::select(.x, id = .data$package_id, .data$created, .data$url))



    suppressWarnings({
      out <- dplyr::bind_cols(metadata, org) |>
        dplyr::left_join(resources) |>
        dplyr::mutate(ext = tools::file_ext(.data$url),
                      src = .env$ckan_url)
    })

    return(out)

  }

}





#' @rdname search_ckan
#' @export
search_ckan_vic <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$vic, ...)
}

#' @rdname search_ckan
#' @export
search_ckan_nsw <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$nsw, ...)
}

#' @rdname search_ckan
#' @export
search_ckan_aus <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$aus, ...)
}

#' @rdname search_ckan
#' @export
search_ckan_qld <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$qld, ...)
}

#' @rdname search_ckan
#' @export
search_ckan_sa <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$sa, ...)
}

#' @rdname search_ckan
#' @export
search_ckan_wa <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$wa, ...)
}

#' @rdname search_ckan
#' @export
search_ckan_nt <- function(search_term, ...){
  search_ckan(search_term, ckan_url = urls$nt, ...)
}







