#' Return closeness value for a collection of addresses.
#'
#' The function sends a request to a Dru instance
#' and returns the value of closeness for addresses in a created graph.
#'
#' Returns the list of addresses and the value of closeness
#' corresponding to them. \cr The graph is created from the from the blocks
#' in the range: start - end.
#' The graph can be built either as directed or undirected.
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
#'  \item closeness
#' }
#' @export
#'
#' @examples
#' dru_get_closeness("https://dru.bergplace.org/api", 0, 1000, "true")


dru_get_closeness <- function(connection, start, end, directed) {

addr <- sprintf("%s/get_closeness/%s/%s/%s", connection, start, end, directed)
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
 data <- data.frame(check_status$data)
 data <- as.data.frame(t(data))
 rows <- nrow(data)
 results <- data.frame(matrix(ncol = 2, nrow = rows))
 colnames(results) <- c("address", "closeness")
 for (x in 1:rows){
  results[x, 1] <- rownames(data)[x]
  results[x, 2] <- data[x, ]
 }
}
return(results)
}
