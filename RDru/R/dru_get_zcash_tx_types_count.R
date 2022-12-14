#' Return transaction types in a given graph.
#'
#' The function sends a request to a Dru instance
#' and returns block's height and the types of transactions.
#'
#' The returned data frame contains the blocks heights accompanied
#' by the following: \cr block time and number of shielded-shielded, shielded-transparent,
#' transparent-shielded and transparent-transparent transactions. \cr
#' For details on transactions types, see: https://z.cash/technology/
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
#'  \item block_height
#'  \item s.s (shielded-shielded transactions)
#'  \item s.t (shielded-transparent transactions)
#'  \item t.s (transparent-shielded transactions)
#'  \item t.t (transparent-transparent transactions)
#'  \item time
#'  \item date_time
#' }
#'
#' @export
#'
#' @examples
#' dru_get_zcash_tx_types_count("https://dru.bergplace.org/api", 0, 100)


dru_get_zcash_tx_types_count <-
function(connection, start, end) {

addr <- sprintf("%s/get_zcash_tx_types_count/%s/%s", connection, start, end)
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
 count <- end - start + 1
 results <- data.frame(matrix(ncol = 6, nrow = count))
 colnames(results) <- c("block_height", "time", "s.s", "s.t", "t.s", "t.t")
 for (i in 1:count) {
   results[i, 1] <- start + i - 1
  for (j in 2:6){
   results[i, j] <- data[(i - 1) * 5 + j - 1, ]
  }
 }
 disable_warning <- getOption("warn")
 options(warn = -1)
 results["date_time"] <- lapply(results$time, as_datetime)
 options(warn = disable_warning)
 results <- results[, c(1, 3, 4, 5, 6, 2, 7)]
}
return(results)
}
