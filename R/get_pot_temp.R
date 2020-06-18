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
get_pot_temp_2D <- function(ncfile, x_start = 1, y_start = 1, z_start = 1, t_start = 1,
                            x_len, y_len, z_len = 1, t_len = 1) {
  nc <- ncdf4::nc_open(ncfile)

  sdf <- data.frame(
    x = x_start,
    y = y_start,
    z = z_start,
    t = t_start,
    stringsAsFactors = F
  )

  ldf <- data.frame(
    x = x_len,
    y = y_len,
    z = z_len,
    t = t_len,
    stringsAsFactors = F
  )

  Pa_to_hPA <- 100

  t_data <- ncvar_subs("T", start_df = sdf, len_df = ldf, nc = nc)
  p_data <- ncvar_subs("P", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA
  pp_data <- ncvar_subs("PB", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA

  t_d300 <- t_data + 300
  ptot <- p_data + pp_data

  temp <- t_d300 * (ptot / 1000)^(2 / 7)

  nc_close(nc_file)

  return(temp)
}
