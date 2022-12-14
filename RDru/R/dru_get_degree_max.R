#' Return the value of the degree
#'
#' The function sends a request to a Dru instance
#' and returns addresses with the highest degree value.
#'
#' Returns an address and the value of the degree corresponding to it. \cr
#' The graph is created from the blocks in the range: start - end. \cr
#' The graph will be built as directed,
#' but using the mode,
#' all variants of the measure can be computed: all, in, out.
#'
#' @param connection A string that contains an address of the Dru instance.
#' @param start A number which is the start height of a block
#' that is included in the search range.
#' @param end A number which is the end height of a block
#' that is included in the search range.
#' @param mode A string that specifies the method to calculate the degree.
#' 3 modes are available : \cr all - total degree of the node.
#' in - in degree of the node, out - out degree of the node.
#'
#' @author Mateusz Gabrys
#'
#' @import httr
#' @import jsonlite
#' @import devtools
#'
#' @return This function returns a list including an address, mode
#' as well as all of the degrees with their heights.
#'
#' @export
#'
#' @examples
#' dru_get_degree_max("https://dru.bergplace.org/api", 0, 1000, "all")


dru_get_degree_max <-
function(connection, start, end, mode) {

addr <- sprintf("%s/get_degree_max/%s/%s/%s", connection, start, end, mode) # nolint
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
 data <- check_status$data
 results <- data.frame(matrix(ncol = 2, nrow = 1))
 colnames(results) <- c("address", "degree")
 results[, 1:2] <- c(names(data), data[1])
}
return(results)
}
