#' Retrieve UCDP Dataset from API
#'
#' This function retrieves a dataset from the Uppsala Conflict Data Program (UCDP) API.
#' It takes the resource type and version as input parameters, constructs the API request,
#' and returns the combined results of all pages. The function also validates and filters
#' the input parameters based on the specified resource to ensure compatibility with the API.
#'
#' @param resource A character string specifying the type of resource to request.
#' Must be one of:
#'   - `"ucdpprioconflict"`: State-based conflicts data.
#'   - `"dyadic"`: Dyadic-level conflict data.
#'   - `"battledeaths"`: Data on battle-related deaths.
#'   - `"nonstate"`: Non-state conflicts data.
#'   - `"onesided"`: Data on one-sided violence.
#'   - `"gedevents"`: Event-based conflict data from the UCDP GED.
#'
#' @param version A character string specifying the version of the resource to request.
#'
#' @param country A vector of numeric country codes (Gleditsch and Ward codes).
#'   - Valid for all resources.
#'   - Must be a numeric vector, where each code corresponds to a country involved in the conflict.
#'
#' @param conflict A vector of numeric conflict IDs.
#'   - Valid for `"ucdpprioconflict"`, `"dyadic"`, `"battledeaths"`, and `"nonstate"` resources.
#'   - Not valid for the `"onesided"` resource.
#'   - Must be a numeric vector representing conflict IDs.
#'
#' @param year A vector of years in YYYY format.
#'   - Valid for all resources.
#'   - Must be a numeric vector or character vector in the format `"YYYY"`.
#'
#' @param conflict_incompatibility A vector of numeric conflict incompatibility codes.
#'   - Valid for `"ucdpprioconflict"`, `"dyadic"`, and `"battledeaths"` resources.
#'   - Not valid for `"nonstate"` and `"onesided"` resources.
#'   - Must be a numeric vector representing the type of incompatibility.
#'
#' @param dyad A vector of numeric dyad IDs.
#'   - Valid for `"gedevents"`, `"dyadic"`, `"battledeaths"`, and `"onesided"` resources.
#'   - Not valid for `"ucdpprioconflict"` and `"nonstate"` resources.
#'   - Must be a numeric vector representing dyad IDs.
#'
#' @param org A vector of numeric organization IDs.
#'   - Valid only for the `"nonstate"` resource.
#'   - Not valid for `"ucdpprioconflict"`, `"dyadic"`, `"battledeaths"`, `"onesided"`, and `"gedevents"` resources.
#'   - Must be a numeric vector representing organization IDs.
#'
#' @param conflict_type A vector of numeric conflict type codes.
#'   - Valid for `"ucdpprioconflict"`, `"dyadic"`, and `"battledeaths"` resources.
#'   - Not valid for `"nonstate"`, `"onesided"`, and `"gedevents"` resources.
#'   - Must be a numeric vector representing conflict type codes.
#'
#' @param geography A string specifying the geographic bounding box in the format "y0 x0,y1 x1".
#'   - Valid only for the `"gedevents"` resource.
#'   - Format: Two coordinates describing the bounding box (latitude and longitude).
#'   - Example: `"40 -75,41 -73"` for New York City.
#'
#' @param start_date A string specifying the start date in "YYYY-MM-DD" format.
#'   - Valid only for the `"gedevents"` resource.
#'   - Must specify the date from which events have definitely happened.
#'
#' @param end_date A string specifying the end date in "YYYY-MM-DD" format.
#'   - Valid only for the `"gedevents"` resource.
#'   - Must specify the date up to which events have definitely happened.
#'
#' @param type_of_violence A vector of numeric violence type codes.
#'   - Valid only for the `"gedevents"` resource.
#'   - Must be a numeric vector representing types of violence.
#'
#' @param actor A vector of numeric actor IDs.
#'   - Valid only for the `"gedevents"` resource.
#'   - Must be a numeric vector representing actor IDs.
#'
#' @seealso \url{https://ucdp.uu.se/apidocs/}
#' @return A data frame containing the requested data.
#' @export
cl_get_data <- function(resource,
                        version,
                        country = NULL,
                        conflict = NULL,
                        year = NULL,
                        conflict_incompatibility = NULL,
                        dyad = NULL,
                        org = NULL,
                        conflict_type = NULL,
                        geography = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        type_of_violence = NULL,
                        actor = NULL) {

  # Validate 'resource' using rlang::arg_match
  resource <- rlang::arg_match(resource, values = c(
    "ucdpprioconflict", "dyadic", "battledeaths", "nonstate", "onesided", "gedevents"
  ))

  # Validate and prepare parameters
  validated_params <- list(
    Country = check_country(country, resource),
    Conflict = check_conflict(conflict, resource),
    Year = check_year(year, resource),
    ConflictIncompatibility = check_conflict_incompatibility(conflict_incompatibility, resource),
    ConflictType = check_conflict_type(conflict_type, resource),
    Dyad = check_dyad(dyad, resource),
    Org = check_org(org, resource),
    Geography = check_geography(geography, resource),
    StartDate = check_start_date(start_date, resource),
    EndDate = check_end_date(end_date, resource),
    TypeOfViolence = check_type_of_violence(type_of_violence, resource),
    Actor = check_actor(actor, resource)
  )


  ## make base request
  max_pages_body <- cl_build_request(resource, version, pagesize = 1, page = 1,
                                     params = validated_params) |>
    cl_perform_request()

  last_page <- ceiling(max_pages_body$TotalCount / cl_env$max_page_size)

  reqs <- cl_build_request(resource, version,
                          pagesize =  cl_env$max_page_size,
                          page = 0:last_page,
                          params = validated_params)

  resps <- reqs |>
    cl_perform_request()

  result <- resps |>
    cl_process_response()

  if (max_pages_body$TotalCount != nrow(result)) {
    cli::cli_abort(message =
                     c("x"= "Total collected data ({nrow(resps)}) does not match number listed in TotalCount ({max_pages_body$TotalCount})"))
  }

  return(result)
}
