#' Title
#'
#' @param varname
#' @param start_df
#' @param len_df
#' @param nc
#'
#' @return
#' @export
#'
#' @examples
ncvar_subs <- function(varname, start_df = sdf, len_df = ldf, nc = nc){
  stopifnot(is.data.frame(start_df))
  stopifnot(is.data.frame(len_df))

  nc_var_dimids <- nc$var[[varname]]$dimids

  dimnames <- names(nc$dim)[nc_var_dimids+1]

  if(none(grepl("west_east", dimnames))){
    start_df$x <- NULL
    len_df$x  <- NULL
  }

  if(none(grepl("south_north", dimnames))){
    start_df$y <- NULL
    len_df$y  <- NULL
  }
  if(none(grepl("bottom_top", dimnames))){
    start_df$z <- NULL
    len_df$z  <- NULL
  }
  if(none(grepl("Time", dimnames))){
    start_df$t <- NULL
    len_df$t  <- NULL
  }


  dat <- ncdf4::ncvar_get(nc = nc, varid = varname,
                          start = unlist(start_df),
                          count = unlist(len_df)
                          )

  return(dat)

}
