#' Title
#'
#' @param theta
#' @param p_data
#' @param pb_data
#' @param psurf
#'
#' @return
#' @export
#'
#' @examples
calc_temp <- function(theta, p_data, pb_data, psurf = NULL) {
  ptot <- p_data + pb_data

  if (!is.null(psurf)) {
    ptot <- c(psurf, ptot)
  }

  temp <- theta * (ptot / 1000)^(2 / 7)

  return(temp)
}
