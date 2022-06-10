NULL
#' Finds the real name of a variable in a netCDF file.
#'
#' @param nc file (or connection to)  provided by CORDEX / EURO-CORDEX projections
#' @param ncvar_name possible names for the netCDF variables
#' @param ... further arguments (not used)
#' @export
#'
#' @examples
#'
#' nc1 <- "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//pr_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_ICTP-RegCM4-6_v1_mon_198101-199012.nc"
#' var <- which_ncvar_name(nc1,ncvar_name=c("X","x","rlon","auto"))
#'
#'

which_ncvar_name <- function (nc,ncvar_name,...) {

  if (class(nc)=="ncdf4") {
    nc_close_at_end <- FALSE
    ncc <- nc
  } else {
    ncc <- nc_open(filename=nc)
    nc_close_at_end <- FALSE
  }

  out <- as.character(NA)
  for (it in ncvar_name) {
    temp <- try(ncvar_get(ncc,varid=it),silent=TRUE)
    if (class(temp)!="try-error") out <- it
  }
  ###
  if (nc_close_at_end) nc_close(ncc)
  ###
  return(out)

}
