#' Return betweenness value for addresses.
#'
#' The function sends a request to a Dru instance
#' and returns the value of betweenness for addresses in a created graph.
#'
#' Returns the address and the value of betweenness in the graph created. \cr
#' The graph is created from the from the blocks in the range start - end. \cr
#' The graph can be built either as directed or undirected. \cr
#' All shortest paths have to be computed, this operation is time-consuming.
#' Use with care.
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
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item address
#'  \item betweenness
#' }
#' @export
#'
#' @examples
#' dru_get_betweenness_max("https://dru.bergplace.org/api", 0, 1000, "true")

dru_get_betweenness_max <-
function(connection, start, end, directed) {

addr <- sprintf("%s/get_betweenness_max/%s/%s/%s", connection, start, end, directed) # nolint
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
 data <- check_status$data
 results <- data.frame(matrix(ncol = 2, nrow = 1))
 colnames(results) <- c("address", "betweenness")
 results[, 1:2] <- c(names(data), data[1])
}
return(results)
}
