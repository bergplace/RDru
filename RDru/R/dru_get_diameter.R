#' Return the diameter of a graph.
#'
#' The function sends a request to a Dru instance
#' and returns the diameter of the created graph.
#'
#' Returns the diameter of the graph created from the blocks
#' in the range: start - end. \cr
#' The graph can be considered as directed or undirected.
#'
#' @param connection A string that contains an address of the Dru instance.
#' @param start A number which is the start height of a block
#' that is included in the search range.
#' @param end A number which is the end height of a block
#' that is included in the search range.
#' @param directed A boolean that specifies if the graph should be considered
#' directed or undirected.
#'
#' @author Mateusz Gabrys
#'
#' @import httr
#' @import jsonlite
#' @import devtools
#'
#' @return This function returns a number that is the diameter of the graph.
#' @export
#'
#' @examples
#' dru_get_diameter("https://dru.bergplace.org/api", 0, 1000, "true")


dru_get_diameter <-
function(connection, start, end, directed) {

addr <- sprintf("%s/get_diameter/%s/%s/%s", connection, start, end, directed) # nolint
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
 diameter <- check_status$data
}
return(diameter)
}
