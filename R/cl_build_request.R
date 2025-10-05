

#' Create an API Request for the UCDP Database
#'
#' Constructs an API request to the Uppsala Conflict Data Program (UCDP) API using specified parameters,
#' including the resource, API version, page size, and page number(s).
#' This function allows for both single and multiple page requests.
#'
#' @param resource A character string specifying the resource path for the API (e.g., "conflict", "actor").
#' @param version A character string indicating the version of the API to use (e.g., "v1", "v2").
#' @param pagesize An integer specifying the number of records per page. Must be a positive number.
#' @param page An integer or vector of integers indicating the page number(s) to request.
#' If a single number is provided, the function returns a single request; if a vector of numbers is provided,
#' a list of requests is returned.
#' @param params A list of validated params returned by `cl_check_params`.
#'
#' @return If a single page number is provided, the function returns a `httr2_request` object.
#' If multiple page numbers are provided, the function returns a list of `httr2_request` objects.
cl_build_request <- function(resource, version, pagesize, page, params) {

  ## make base request


  if(length(page)==1){
    req <- base_request_factory(resource, version, pagesize, page, params)
    return(req)
  } else {
    reqs <- purrr::map(page,
      ~base_request_factory(resource, version, pagesize, page = .x, params)
    )
    return(reqs)
  }

  return(request)

}

base_request_factory <- function(resource, version, pagesize, page, params){
  req <- httr2::request("https://ucdpapi.pcr.uu.se/api/") |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_path_append(version)|>
    httr2::req_url_query(!!!params,.multi = "comma") |>
    httr2::req_url_query(pagesize = pagesize, page = page)

  return(req)
}
