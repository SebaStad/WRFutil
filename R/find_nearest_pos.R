#' Title
#'
#' @param lat_arr 2D Array of latitute in WRF
#' @param lon_arr 2D array of longitude in WRF
#' @param lat Latitude of position in question
#' @param lon Longitute of position in qestion
#' @param prec_lat Precision for Latitute
#' @param prec_lon
#'
#' @return
#' @export
#'
#' @examples
find_nearest_pos <- function(lat_arr, lon_arr, lat, lon, prec_lat = 0.005, prec_lon = 0.005) {
  find_pos <- array(0, dim(lat_arr))
  find_lat <- array(0, dim(lat_arr))
  find_lon <- array(0, dim(lat_arr))


  find_lat[which(lat_arr < lat + prec_lat & lat_arr > lat - prec_lat)] <- 1

  find_lon[which(lon_arr < lon + prec_lon & lon_arr > lon - prec_lon)] <- 1

  find_pos[which(find_lon > 0 & find_lat > 0)] <- 1

  out_vec <- which(find_pos > 0, arr.ind = T)

  if (nrow(out_vec) > 1) {
    print("More than one point detected. Change precision!")
    print("Only first point selected")

    out_vec <- out_vec[1, ]
  }

  return(data.frame(
    "x" = out_vec[1],
    "y" = out_vec[2]
  ))
}
