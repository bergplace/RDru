#' Return the density of a graph.
#'
#' The function sends a request to a Dru instance
#' and returns the density of the created graph.
#'
#' Returns the density of the graph created from the blocks
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
#' @param loops A boolean that specifies if loops should be in the build graph.
#'
#' @author Mateusz Gabrys
#'
#' @import httr
#' @import jsonlite
#' @import devtools
#'
#' @return This function returns a number that is the density of the graph.
#' @export
#'
#' @examples
#' dru_get_density("https://dru.bergplace.org/api", 0, 1000, "true", "true")



dru_get_density <-
function(connection, start, end, directed, loops) {

addr <- sprintf("%s/get_density/%s/%s/%s/%s", connection, start, end, directed, loops) # nolint
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
 density <- check_status$data
}
return(density)
}
