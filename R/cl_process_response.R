cl_process_response <- function(resps){
  result <- purrr::map_dfr(resps, ~purrr::pluck(.x,"Result"))
  return(result)
}
