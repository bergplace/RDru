#' Check if the two addresses are connected.
#'
#' The function sends a request to a Dru instance
#' and checks if two given addresses are connected.
#'
#' Returns true/false information whether two addresses
#' are connected within a given range of blocks. \cr
#' If any of these addresses does not exist in the graph, None will be returned.
#'
#' @param connection A string that contains an address of the Dru instance.
#' @param start A number which is the start height of a block
#' that is included in the search range.
#' @param end A number which is the end height of a block
#' that is included in the search range.
#' @param address1 A string that contains the first address to be checked.
#' @param address2 A string that contains the second address to be checked.
#' @param directed A boolean that specifies if the graph should be considered
#' directed or undirected.
#'
#' @author Mateusz Gabrys
#'
#' @import httr
#' @import jsonlite
#' @import devtools
#'
#' @return A data frame, containing all parameters
#' as well as the boolean are_connected
#' @export
#'
#' @examples
#' dru_are_connected("https://dru.bergplace.org/api", 0, 100,
#' "t1StbPM4X3j4FGM57HpGnb9BMbS7C1nFW1r", "t1KstPVzcNEK4ZeauQ6cogoqxQBMDSiRnGr", "true")

dru_are_connected <-
function(connection, start, end, address1, address2, directed) {

addr <- sprintf("%s/are_connected/%s/%s/%s/%s/%s",
connection, start, end, address1, address2, directed)
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
 results <- check_status$data
}
return(results)
}
