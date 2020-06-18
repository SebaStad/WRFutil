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

  u_arr   <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  u_unrot <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  u_arr[,,1,] <- u10_data
  u_arr[,,2:(ldf$z+1),] <- u_data

  v_arr   <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  v_unrot <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  v_arr[,,1,] <- v10_data
  v_arr[,,2:(ldf$z+1),] <- v_data


  h_temp <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  h_temp[,,2:(ldf$z+1),] <- (h_lev+ hb_lev)
  heights <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  heights[,,1,] <- 10


  for(i in seq(ldf$z+1)){
    for(j in seq(ldf$t)){
      u_unrot[,,i,j] <- u_arr[,,i,j] * cosalpha -  u_arr[,,i,j] * sinalpha
      v_unrot[,,i,j] <- v_arr[,,i,j] * cosalpha + v_arr[,,i,j] * sinalpha

      if(i>1){
        heights[,,i,j] <- 100 *( h_temp[,,i,j] / 9.81)
      }

    }
  }

  uv_dat <- sqrt(u_unrot * u_unrot + v_unrot * v_unrot)
  nc_close(nc)

  return(list(
    "U" = u_unrot,
    "V" = v_unrot,
    "UV2" = uv_dat,
    "HGHT" = heights
  ))
}
