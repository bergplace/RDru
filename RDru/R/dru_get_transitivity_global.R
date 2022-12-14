#' Return the global transitivity value.
#'
#' The function sends a request to a Dru instance
#' and returns the global value of transitivity in the created graph.
#'
#' Returns the clustering coefficient of the graph
#' created from the blocks in the range: start - end. \cr
#' This value is global for the graph.
#' For node-level clustering coefficient, use dru_get_transitivity function.
#'
#' @param connection A string that contains an address of the Dru instance.
#' @param start A number which is the start height of a block
#' that is included in the search range.
#' @param end A number which is the end height of a block
#' that is included in the search range.
#'
#' @author Mateusz Gabrys
#'
#' @import httr
#' @import jsonlite
#' @import devtools
#'
#' @return This function returns a number that is the global transitivity value,
#' for a given graph
#' @export
#'
#' @examples
#' dru_get_transitivity_global("https://dru.bergplace.org/api", 0, 1000)

dru_get_transitivity_global <- function(connection, start, end) {

addr <- sprintf("%s/get_transitivity_global/%s/%s", connection, start, end) # nolint
resp <- GET(addr)
if (status_code(resp) != 200) {
 stop("Invalid HTTP status code!")
}
json <- fromJSON(addr)
result_url <- substring(json, 1)
resp <- GET(addr)
if (status_code(resp) != 200) {
 stop("Invalid HTTP status code!")
}
check_status <- fromJSON(result_url)
first <- TRUE
while (check_status$ready != TRUE) {
 if (first) {
  print("Waiting for results ...")
  Sys.sleep(3)
  first <- FALSE
 }
 Sys.sleep(10)
 check_status <- fromJSON(result_url)
}
if (check_status$ready == TRUE) {
 result <- check_status$data
}
return(result)
}
