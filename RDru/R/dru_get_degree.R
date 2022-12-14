#' Return the value of the degree
#'
#' The function sends a request to a Dru instance and returns
#' the list of addresses and degree values corresponding to them.
#'
#' Returns the list of addresses and the value of degree corresponding to them.
#' \cr
#' The graph is created from the blocks in the range: start - end. \cr
#' The graph will be built as directed.
#' Different measure variants can be computed: all, in, out.
#'
#' @param connection A string that contains an address of the Dru instance.
#' @param start A number which is the start height of a block
#' that is included in the search range.
#' @param end A number which is the end height of a block
#' that is included in the search range.
#' @param mode A string that specifies the method to calculate the degree.
#' Three modes are available : \cr
#' all - total degree of the node,
#' in - in degree of the node, out - out degree of the node.
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
#'  \item degree
#' }
#' @export
#'
#' @examples
#' dru_get_degree("https://dru.bergplace.org/api", 0, 10, "all")

dru_get_degree <-
function(connection, start, end, mode) {

addr <- sprintf("%s/get_degree/%s/%s/%s", connection, start, end, mode)
resp <- GET(addr)
if (status_code(resp) != 200) {
 stop("Invalid HTTP status code!")
}
json <- fromJSON(addr)
result_url <- substring(json, 1)
resp <- GET(result_url)
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
 colnames(results) <- c("address", "degree")
 for (x in 1:rows){
  results[x, 1] <- rownames(data)[x]
  results[x, 2] <- data[x, ]
 }
}
return(results)
}
