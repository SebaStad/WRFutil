#' Title
#'
#' @param file
#' @param xpos
#' @param ypos
#' @param tstep
#'
#' @return
#' @export
#'
#' @examples
get_wind <- function(ncfile, x_start = 1, y_start = 1, z_start = 1, t_start = 1,
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

  u_data <- ncvar_subs("U", start_df = sdf, len_df = ldf, nc = nc)
  v_data <- ncvar_subs("V", start_df = sdf, len_df = ldf, nc = nc)

  cosalpha <- ncvar_subs("COSALPHA", start_df = sdf, len_df = ldf, nc = nc)
  sinalpha <- ncvar_subs("SINALPHA", start_df = sdf, len_df = ldf, nc = nc)

  u10_data <- ncvar_subs("U10", start_df = sdf, len_df = ldf, nc = nc)
  v10_data <- ncvar_subs("V10", start_df = sdf, len_df = ldf, nc = nc)

  lon <- ncvar_subs("XLONG", start_df = sdf, len_df = ldf, nc = nc)
  lat <- ncvar_subs("XLAT", start_df = sdf, len_df = ldf, nc = nc)

  h_lev <- ncvar_subs("PH", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA
  hb_lev <- ncvar_subs("PHB", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA

  u_unrot <- c(u10_data, u_data) * c(cosalpha) - c(u10_data, u_data) * c(sinalpha)
  v_unrot <- c(v10_data, v_data) * c(cosalpha) + c(v10_data, v_data) * c(sinalpha)

  heights <- c(10, 100 * (h_lev + hb_lev) / 9.81)

  uv_dat <- sqrt(u_unrot * u_unrot + v_unrot * v_unrot)
  nc_close(nc)

  return(data.frame(
    "U" = u_unrot,
    "V" = v_unrot,
    "UV2" = uv_dat,
    "HGHT" = heights
  ))
}
