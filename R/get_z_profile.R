#' Title
#'
#' @param file
#' @param xpos
#' @param ypos
#' @param tstep
#' @param moisture
#'
#' @return
#' @export
#'
#' @examples
get_z_profile <- function(ncfile, x_start = 1, y_start = 1, z_start = 1, t_start = 1,
                          x_len, y_len, z_len = 1, t_len = 1, moisture = TRUE) {
  nc <- ncdf4::nc_open(ncfile)
  sdf <- data.frame(
    x = x_start,
    y = y_start,
    z = 1,
    t = t_start,
    stringsAsFactors = F
  )

  ldf <- data.frame(
    x = x_len,
    y = y_len,
    z = nc$dim$bottom_top$len,
    t = t_len,
    stringsAsFactors = F
  )
  Pa_to_hPA <- 100

  t_data  <- ncvar_subs("T", start_df = sdf, len_df = ldf, nc = nc)
  t2_data <- ncvar_subs("T2", start_df = sdf, len_df = ldf, nc = nc)
  ps_data <- ncvar_subs("PSFC", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA
  p_data  <- ncvar_subs("P", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA
  pb_data <- ncvar_subs("PB", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA


  h_lev   <- ncvar_subs("PH", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA
  hb_lev  <- ncvar_subs("PHB", start_df = sdf, len_df = ldf, nc = nc) / Pa_to_hPA


  theta   <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  tempe   <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  t_arr <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  t_arr[,,1,] <- t2_data
  t_arr[,,2:(ldf$z+1),] <- t_data


  h_temp <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  h_temp[,,2:(ldf$z+1),] <- (h_lev+ hb_lev)
  heights <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  heights[,,1,] <- 2

  p_temp <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
  p_temp[,,2:(ldf$z+1),] <- (p_data + pb_data)
  p_temp[,,1,] <- ps_data


  for(i in seq(ldf$z+1)){
    for(j in seq(ldf$t)){
#      u_unrot[,,i,j] <- u_arr[,,i,j] * cosalpha -  u_arr[,,i,j] * sinalpha
#      v_unrot[,,i,j] <- v_arr[,,i,j] * cosalpha + v_arr[,,i,j] * sinalpha

      if(i>1){
        heights[,,i,j] <- 100 *( h_temp[,,i,j] / 9.81)
        theta[,,i,j] <- calc_theta(t_arr[,,i,j])
        tempe[,,i,j] <- calc_temp(theta[,,i,j], p_temp[,,i,j], 0)
      }
      if(i==1){
        theta[,,i,j] <- t_arr[,,i,j]
        tempe[,,i,j] <- calc_temp(theta[,,i,j], p_temp[,,i,j], 0)
      }

    }
  }



  # theta <- c(t2_data, calc_theta(t_data))
  # tempe <- calc_temp(theta, p_data, pb_data, psurf = ps_data)

  data <- list(
    "THTA" = theta,
    "TEMP" = tempe - 273.15,
    "HGHT" = heights
  )
  if (moisture == F) {
    nc_close(nc)
    return(data)
  } else {
    q_data <- ncvar_subs("QVAPOR", start_df = sdf, len_df = ldf, nc = nc)
    q2_data <- ncvar_subs("Q2", start_df = sdf, len_df = ldf, nc = nc)


    q_arr <- array(0,c(ldf$x,ldf$y, ldf$z+1, ldf$t))
    q_arr[,,1,] <- q2_data
    q_arr[,,2:(ldf$z+1),] <- q_data

    # q_data <- c(q2_data, q_data)
    w_data <- q_arr / (1 - q_arr)
    # https://www.e-education.psu.edu/meteo300/node/519
    ws_av <- (6.11 * 10^((7.5 * (tempe - 273.15)) / (237.7 + (tempe - 273.15)))) / 1000
    rel_hum <- w_data / ws_av

    data[["RELF"]] <- rel_hum * 100
    data[["Q"]] <- q_arr
    nc_close(nc)
    return(data)
  }
}
