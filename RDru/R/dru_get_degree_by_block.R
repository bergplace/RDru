#' Return the value of the degree
#'
#' The function sends a request to a Dru instance
#' and returns the list of addresses degree values,
#' corresponding to them. \cr The degree is computed in each block separately.
#'
#' Returns the list of addresses and the value of the degree
#' corresponding to them. \cr
#' The graph is created from the blocks in the range: start - end. \cr
#' This variant computes the degree in each block separately. \cr
#' The graph will be built as directed,
#' but using the mode,
#' all variants of the measure can be computed: all, in, out.
#'
#' @param connection A string that contains an address of the Dru instance.
#' @param start A number which is the start height of a block
#' that is included in the search range.
#' @param end A number which is the end height of a block
#' that is included in the search range.
#' @param address A string that specifies an address, which values of degree
#' are shown in the results.
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
#' @return This function returns a list including an address, mode
#' as well as all of the degrees with their heights.
#'
#' @export
#'
#' @examples
#' dru_get_degree_by_block("https://dru.bergplace.org/api",
#' 0, 10, "t1KstPVzcNEK4ZeauQ6cogoqxQBMDSiRnGr", "all")

dru_get_degree_by_block <-
function(connection, start, end, address, mode) {

addr <- sprintf("%s/get_degree_by_block/%s/%s/%s/%s", connection, start, end, address, mode) # nolint
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
 results <- check_status$data
}
return(results)
}
