#' Return the list of blocks
#'
#' The function sends a request to a Dru instance
#' and returns the list of blocks for a given range of block's height.
#'
#' Returns the list of blocks for a given range of block's heights. \cr
#' The returned JSON can be used for further processing
#' if none of the available functions is suitable
#' for performing the requested analysis. \cr
#' This function returns whole blocks. No attributes are stripped.
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
#' @return This function returns a JSON containing detailed information
#' about blocks in a given range.
#' @export
#'
#' @examples
#' dru_get_blocks("https://dru.bergplace.org/api", 0, 10)

dru_get_blocks <- function(connection, start, end) {

addr <- sprintf("%s/get_blocks/%s/%s", connection, start, end)
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