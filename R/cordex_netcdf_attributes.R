NULL
#' Extraction of CORDEX netcdf attributes
#'
#' @param nc file (or connection to)  provided by CORDEX / EURO-CORDEX projections
#' @param variable variable, e.g. c("pr","tas","tasmin","tasmax") or \code{NULL}. In case of \code{NULL} (default) , it is retrived of \code{nc} filename. See \code{varid} on \code{\link{ncvar_get}}
#' @param ncatts_in_df_names names
#' @param ncatts_in_df logical value. If it is \code{TRUE} netCDF attributes will be added to the returened data frame.
#' @param ... further arguments.
#'
#' @importFrom ncdf4 ncatt_get
#' @importFrom stringr str_split str_trim
#' @importFrom lubridate seconds

#' @export
#'
#' @seealso \code{\link{cordex_netcdf_attributes_multi_sources}},\code{\link{ncvar_get}}
#'
#' @details CORDEX data are available on \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cordex-domains-single-levels?tab=overview}
#'
#'
#'
#'
#'
#' @examples
#'
#' nc <- system.file('netcdfs/pr_EUR-11_CCCma-CanESM2_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_mon_208101-209012.nc',package="cordexr")
#' out <- cordex_netcdf_attributes(nc)
#' out <- cordex_netcdf_attributes(nc,ncatts_in_df=TRUE)
#'
#'


cordex_netcdf_attributes <- function(nc,variable=NULL,ncatts_in_df=FALSE,
                                     ncatts_in_df_names=c("driving_model_id","model_id","institution","experiment_id","variable","frequency","source","driving_model_ensemble_member"),...){




  if (class(nc)=="ncdf4") {
    nc_close_at_end <- FALSE
    ncc <- nc
  } else {
    if (is.null(variable)) variable <- NA
    if (is.na(variable)) variable <- str_split(basename(nc),"_")[[1]][1]


    ncc <- nc_open(nc)
    nc_close_at_end <- FALSE
  }
  variable <- variable[1]

  out <- list()
  global_attributes   <- ncatt_get(ncc,varid=0)
  variable_attributes <- ncatt_get(ncc,varid=variable)
  ##out <- list()


  if (nc_close_at_end) nc_close(ncc)

  if (ncatts_in_df) {
    out <- data.frame()
    for (it in names(variable_attributes)) {
      out[1,paste("variable",it,sep="_")] <- variable_attributes[[it]][1]
    }
    for (it in names(global_attributes)) {
      out[1,it] <- global_attributes[[it]][1]
    }
    if (is.null(ncatts_in_df_names)) ncatts_in_df_names <- NA
    if (!is.na(ncatts_in_df_names[1])) {
      ncatts_in_df_names <- ncatts_in_df_names[which(ncatts_in_df_names %in% names(out))]
      out <- out[,ncatts_in_df_names]
    }


  } else {
    out$global_attributes <- global_attributes
    out$variable_attributes <- variable_attributes
  }
  out$variable <- variable
  out$file <- nc
  return(out)
}


