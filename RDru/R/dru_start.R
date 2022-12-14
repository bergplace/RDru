#' Initialize Dru connection.
#'
#' Function connects to a Dru instance, and checks if the connections work.
#'
#' This function has been designed to make working with the RDru package easier.
#' It sets up the connection between the user and the Dru instance.
#' To use the function we have to provide the address of the instance as well as
#' specify, whether we want to use https or not.
#' It is recommended to run the dru_start function
#' and assign it's result to a variable,
#' before running other functions included in the RDru package.
#'
#' @param address A string that contains an address of the Dru instance
#' @param https A boolean that specifies, if we want to use https connection
#'
#' @author Mateusz Gabrys
#'
#' @import httr
#' @import devtools
#' @import devtools
#'
#' @return String with either http or https address of the instance
#' @export
#'
#' @examples
#' dru_start("dru.bergplace.org/api", TRUE)

dru_start <- function(address, https = TRUE) {
  if (https) {
   addr <- sprintf("https://%s", address)
   test <- sprintf("%s/get_blocks/0/0", addr)
   resp <- GET(test)
   if (status_code(resp) != 200) {
    stop("Invalid HTTP status code!")
   } else if (http_type(resp) != "application/json") {
    stop("Wrong result format")
   }
  } else {
   addr <- sprintf("http://%s", address)
   test <- sprintf("%s/get_blocks/0/0", addr)
   resp <- GET(test)
   if (status_code(resp) != 200) {
    stop("Invalid HTTP status code!")
   } else if (http_type(resp) != "application/json") {
    stop("Wrong result format")
   }
  }
  return(addr)
}
