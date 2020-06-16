#' Title
#'
#' @param ncfile
#' @param tstep
#' @param zlayer
#'
#' @return
#' @export
#'
#' @examples
get_pot_temp_2D <- function(ncfile, tstep, zlayer) {
  nc_file <- ncdf4::nc_open(ncfile)

  Pa_to_hPA <- 100

  start_time <- tstep[1]
  duration <- length(tstep)

  start_layer <- zlayer
  heights <- length(zlayer)

  x_len <- nc_file$dim$west_east$len
  y_len <- nc_file$dim$south_north$len

  t_data <- ncdf4::ncvar_get(nc_file, "T",
    start = c(1, 1, start_layer, start_time),
    count = c(x_len, y_len, heights, duration)
  )

  p_data <- ncdf4::ncvar_get(nc_file, "P",
    start = c(1, 1, start_layer, start_time),
    count = c(x_len, y_len, heights, duration)
  ) / Pa_to_hPA

  pp_data <- ncdf4::ncvar_get(nc_file, "PB",
    start = c(1, 1, start_layer, start_time),
    count = c(x_len, y_len, heights, duration)
  ) / Pa_to_hPA

  t_d300 <- t_data + 300
  ptot <- p_data + pp_data

  temp <- t_d300 * (ptot / 1000)^(2 / 7)

  nc_close(nc_file)

  return(temp)
}
