#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
f_melt <- function(.data){
  stopifnot(length(dim(.data))==2)
  as.data.frame.table(.data, responseName = "value") %>%
    mutate_if(is.factor, as.integer)
}
