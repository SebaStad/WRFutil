#' Title
#'
#' @param ncfile
#' @param all
#'
#' @return
#' @export
#'
#' @examples
WRF_dump <- function(ncfile, all = FALSE){
  nc <- ncdf4::nc_open(ncfile)

  xdim <- nc$dim$west_east$len
  ydim <- nc$dim$south_north$len
  zdim <- nc$dim$bottom_top$len

  tdim <- nc$dim$Time$len
  ncdf4::nc_close(nc)

  cat(glue::glue("Dimensions of this WRF file:\nx-Direction:\t{xdim}\ny-Direction:\t{ydim}\nz-Direction:\t{zdim}\nt-Direction:\t{tdim}\n"))
}
