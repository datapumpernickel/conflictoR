
cl_perform_request <- function(reqs) {

  ## check if this is a single instance of a httr2_request
  if(isa(reqs,"httr2_request")){
    resp <- reqs |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)

    return(resp)
  } else {
    progress_bar <- list(
      name = "API Requests",
      type = "tasks",
      format = "{cli::pb_name} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent} | Call: {cli::pb_current}/{cli::pb_total}",
      clear = FALSE
    )

    # Perform requests sequentially with a progress bar
    resps <- httr2::req_perform_parallel(
      reqs,
      progress = progress_bar
    ) |>
      purrr::map(~ httr2::resp_body_json(.x, simplifyVector = TRUE))

    return(resps)
  }

  return(resps)
}
