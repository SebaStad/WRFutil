#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
f_melt <- function(.data){
  library(magritt)
  expect_that(length(dim(.data)), equals(2))
  as.data.frame.table(.data, responseName = "value") %>%
    mutate_if(is.factor, as.integer)
}
