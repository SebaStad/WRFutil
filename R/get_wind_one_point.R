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
get_wind_one_point <- function(file, xpos, ypos, tstep){

  ncfile <- ncdf4::nc_open(file)
  Pa_to_hPA <- 100

  u_data <- ncdf4::ncvar_get(ncfile, "U", start = c(xpos,ypos,1,tstep),
                      count = c(1,1,ncfile$dim$bottom_top$len,1))

  v_data <- ncdf4::ncvar_get(ncfile, "V", start = c(xpos,ypos,1,tstep),
                      count = c(1,1,ncfile$dim$bottom_top$len,1))

  cosalpha <- ncdf4::ncvar_get(ncfile, "COSALPHA",start = c(xpos,ypos,tstep),
                        count = c(1,1,1))
  sinalpha <- ncdf4::ncvar_get(ncfile, "SINALPHA",start = c(xpos,ypos,tstep),
                        count = c(1,1,1))

  u10_data <- ncdf4::ncvar_get(ncfile, "U10", start = c(xpos,ypos,tstep),
                        count = c(1,1,1))

  v10_data <- ncdf4::ncvar_get(ncfile, "V10", start = c(xpos,ypos,tstep),
                        count = c(1,1,1))



  u_unrot <- c(u10_data,u_data)*c(cosalpha) -c(u10_data,u_data)*c(sinalpha)
  v_unrot <- c(v10_data,v_data)*c(cosalpha) + c(v10_data,v_data)*c(sinalpha)



  lon <- ncdf4::ncvar_get(ncfile, "XLONG", start = c(xpos,ypos,tstep),
                   count = c(1,1,1)); print(lon)

  lat <- ncdf4::ncvar_get(ncfile, "XLAT", start = c(xpos,ypos,tstep),
                   count = c(1,1,1)); print(lat)



  h_lev <- ncdf4::ncvar_get(ncfile, "PH", start = c(xpos,ypos,1,tstep),
                     count = c(1,1,ncfile$dim$bottom_top$len,1))/Pa_to_hPA

  hb_lev <- ncdf4::ncvar_get(ncfile, "PHB", start = c(xpos,ypos,1,tstep),
                      count = c(1,1,ncfile$dim$bottom_top$len,1))/Pa_to_hPA

  heights <- c(10,100*(h_lev + hb_lev)/9.81)

  uv_dat <- sqrt(u_unrot*u_unrot + v_unrot*v_unrot)

  return(data.frame("U" = u_unrot,
                    "V" = v_unrot,
                    "UV2"= uv_dat,
                    "HGHT" = heights))



}
