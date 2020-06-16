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
get_z_profile_one_point <- function(file, xpos, ypos, tstep, moisture = TRUE){

  ncfile <- nc_open(file)
  Pa_to_hPA <- 100

  t_data <- ncvar_get(ncfile, "T", start = c(xpos,ypos,1,tstep),
                      count = c(1,1,ncfile$dim$bottom_top$len,1))

  t2_data <- ncvar_get(ncfile, "T2", start = c(xpos,ypos,tstep),
                       count = c(1,1,1))

  ps_data <- ncvar_get(ncfile, "PSFC", start = c(xpos,ypos,tstep),
                       count = c(1,1,1))/Pa_to_hPA


  p_data <- ncvar_get(ncfile, "P", start = c(xpos,ypos,1,tstep),
                      count = c(1,1,ncfile$dim$bottom_top$len,1))/Pa_to_hPA

  pb_data <- ncvar_get(ncfile, "PB", start = c(xpos,ypos,1,tstep),
                       count = c(1,1,ncfile$dim$bottom_top$len,1))/Pa_to_hPA

  theta <- c(t2_data,f_calc_theta(t_data))
  tempe <- f_calc_temp(theta, p_data, pb_data, psurf = ps_data)


  h_lev <- ncvar_get(ncfile, "PH", start = c(xpos,ypos,1,tstep),
                     count = c(1,1,ncfile$dim$bottom_top$len,1))/Pa_to_hPA

  hb_lev <- ncvar_get(ncfile, "PHB", start = c(xpos,ypos,1,tstep),
                      count = c(1,1,ncfile$dim$bottom_top$len,1))/Pa_to_hPA

  heights <- 100*(h_lev + hb_lev)/9.81

  data <- data.frame("THTA" = theta,
                     "TEMP" = tempe-273.15,
                     "HGHT" = c(2,heights))
  if(moisture == F){
    return(data)
  } else {
    q_data <- ncvar_get(ncfile, "QVAPOR", start = c(xpos,ypos,1,tstep),
                        count = c(1,1,ncfile$dim$bottom_top$len,1))

    q2_data <- ncvar_get(ncfile, "Q2", start = c(xpos,ypos,tstep),
                         count = c(1,1,1))

    q_data <- c(q2_data, q_data)
    w_data <- q_data/(1-q_data)
    # https://www.e-education.psu.edu/meteo300/node/519

    ws_av <- (6.11 * 10^((7.5*(tempe-273.15))/(237.7+(tempe-273.15))))/1000

    rel_hum <- w_data/ws_av

    data$RELH <- rel_hum*100
    return(data)

  }

}
