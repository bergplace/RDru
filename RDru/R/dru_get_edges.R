#' Return the list of edges
#'
#' The function sends a request to a Dru instance
#' and returns the list of edges.
#'
#' Returns the list of edges blocks for a given range of blocks's height.
#' These edges can be easily imported into graph-processing libraries.
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
#' @import lubridate
#' @import devtools
#'
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item source
#'  \item target
#'  \item value
#'  \item block_height
#'  \item block_time
#'  \item block_datetime
#' }
#' @export
#'
#' @examples
#' dru_get_edges("https://dru.bergplace.org/api", 0, 10)


dru_get_edges <- function(connection, start, end) {

addr <- sprintf("%s/get_edges/%s/%s", connection, start, end)
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
 disable_warning <- getOption("warn")
 options(warn = -1)
 results["block_datetime"] <- lapply(results$block_time, as_datetime)
 options(warn = disable_warning)
}
return(results)
}
