NULL
#' Extraction of CORDEX grid  values
#'
#' @param nc file (or connection to)  provided by CORDEX / EURO-CORDEX projections
#' @param rlon,x rotated longitude values or x axis values
#' @param rlat,y rotated latitude values or y axis values
#' @param time time coordinate  values
#' @param dim_x possible names for x/longitude dimension
#' @param dim_y possible names for y/latitude dimension
#' @param variable variable, e.g. c("pr","tas","tasmin","tasmax") or \code{NULL}. In case of \code{NULL} (default) , it is retrived of \code{nc} filename. See \code{varid} on \code{\link{ncvar_get}}
#' @param tz time zone used for time unit. Not specified in the CORDEX netcdf file. Default is \code{GMT}
#' @param returns_time_posixct logical value. If it is \code{TRUE} (default) , time coordinate will be transformed and returned as POSIxct class (\code{\link{as.POSIXct}}).
#' @param add_suffix suffix that can be  added to the the name variable. Default is \code{NA} (no suffix added).
#' @param latlon_tolerance,lonlat_tolerance,lon_tolerance,lat_tolerance,time_tolerance tolarances
#' @param ncatts_in_df logical value. If it is \code{TRUE} netCDF attributes will be added to the returened data frame.
#' @param ... further arguments.
#'
#' @importFrom ncdf4 ncatt_get
#' @importFrom stringr str_split str_trim
#' @importFrom lubridate seconds

#' @export
#'
#' @seealso \code{\link{cordex_grid_vertices}},\code{\link{ncvar_get}}
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
#'
#' library(RMAWGEN)
#' data(trentino)
#' stations <- cbind(as.data.frame(STATION_LATLON),STATION_NAMES) %>% st_as_sf(coords=c(1,2),crs=4326)
#' rlatlon <- cordex_grid_vertices(nc,crop_sf=stations)
#'
#' out <- cordex_grid_cell_values(nc,rlon=rlatlon$rlon,rlat=rlatlon$rlat)
#' out2 <- cordex_grid_cell_values(nc,rlon=rlatlon$rlon,rlat=rlatlon$rlat,returns_time_posixct=FALSE)
#'
#'
#'

cordex_grid_cell_values <- function(nc,rlon,rlat,x=rlon,y=rlat,time=NULL,variable=NULL,tz="GMT",

                                    dim_x=c("rlon","x","auto"),dim_y=c("rlat","x","auto"),
                                    returns_time_posixct=TRUE,add_suffix=as.character(NA),ncatts_in_df=FALSE,
                                    latlon_tolerance=0.01,lonlat_tolerance=latlon_tolerance,lon_tolerance=lonlat_tolerance,
                                    lat_tolerance=lonlat_tolerance,time_tolerance=10/(24*3600),...) {



  if (class(nc)=="ncdf4") {
    nc_close_at_end <- FALSE
    ncc <- nc
  } else {
    if (is.null(variable)) variable <- NA
    if (is.na(variable)) variable <- str_split(basename(nc),"_")[[1]][1]

    print(nc)
    ncc <- nc_open(nc)
    nc_close_at_end <- FALSE
  }
  variable <- variable[1]
  #####
  dim_x <- which_ncvar_name(ncc,dim_x)
  dim_y <- which_ncvar_name(ncc,dim_y)
  #####



  ####
  ####
  time_unit <- ncatt_get(ncc,varid="time",attname = "units")$value
  time_unit2 <-  str_split(time_unit,pattern="since")[[1]] %>% str_trim()
  if (time_unit2[1]!="days") stop("Time units are not days!")
  start_date <- time_unit2[2] %>% as.POSIXct(tz=tz)

  ####
  #rlon <- range(rlon,na.rm=TRUE)
  #rlat <- range(rlat,na.rm=TRUE)
  ncrlon <- ncvar_get(ncc,varid=dim_x) ## dim_x
  ncrlat <- ncvar_get(ncc,varid=dim_y) ## dim_y
  condlon <- (ncrlon>=(min(x,na.rm=TRUE)-lon_tolerance) & ncrlon<=(max(x,na.rm=TRUE)+lon_tolerance))
  condlat <- (ncrlat>=(min(y,na.rm=TRUE)-lat_tolerance) & ncrlat<=(max(y,na.rm=TRUE)+lat_tolerance))



  print(dim_x)
  print(dim_y)
  print(range(x))
  print(range(y))
  ####
  ilon <- which(condlon) %>% range() ## DA CORREGGERE QUI!!!
  ilat <- which(condlat) %>% range()
  print(ilon)
  print(ilat)
  if (is.null(time)) time <- NA
  if (is.na(time))  time <- ncvar_get(ncc,varid="time")
  if (class(time)=="Date") time <- as.POSIXct(time,tz=tz)
  if (any(class(time)=="POSIXt")) {

    time <- as.numeric(time-start_date,unit="days")

  }
 ### condlat <- (ncrlon>=(min(rlat,na.rm=TRUE)-time_tolerance) & ncrlon<=(max(rlat,na.rm=TRUE)+time_tolerance))


  itime <- which(ncvar_get(ncc,varid="time") %in% time) %>% range()
  out <- list()



  ## -5.275 -5.165 -5.055 -4.945 -4.835 -4.725 -4.615 -4.505 -4.395 -4.285 -4.175
  ## -5.275, -5.165, -5.055, -4.945, -4.835, -4.725, -4.615, -4.505, -4.395, -4.285, -4.175,
  out_rlon <- ncvar_get(ncc,varid=dim_x,start=ilon[1],count=ilon[2]-ilon[1]+1) ### ncvar_get(nc, varid=NA, start=NA, count=NA, verbose=FALSE,
  out_rlat <- ncvar_get(ncc,varid=dim_y,start=ilat[1],count=ilat[2]-ilat[1]+1)
  out_time <- ncvar_get(ncc,varid="time",start=itime[1],count=itime[2]-itime[1]+1)
  out <- ncvar_get(ncc,varid=variable,start=c(ilon[1],ilat[1],itime[1]),count=c(ilon[2],ilat[2],itime[2])-c(ilon[1],ilat[1],itime[1])+1)
  ###out <- NULL


  out <- reshape2::melt(out)
  names(out)[1] <- dim_x
  names(out)[2] <- dim_y
  names(out)[3] <- "time"

  if (is.na(add_suffix)) add_suffix <- NULL


  if (!ncatts_in_df) {
    names(out)[names(out)=="value"] <- paste0(variable,add_suffix)
  } else {

    out$variable <- variable
  }
  out[,dim_x] <- out_rlon[out[,dim_x]]
  out[,dim_y] <- out_rlat[out[,dim_y]]
  out$time <- out_time[out$time]

  returns_time_posixct <- as.logical(returns_time_posixct)
  if (returns_time_posixct==TRUE) {


     out$time   <- start_date+lubridate::seconds(round(out$time*24*3600)) ## precision of second!


  }
  ##names(out)
  global_attributes   <- ncatt_get(ncc,varid=0)
  variable_attributes <- ncatt_get(ncc,varid=variable)

  if (nc_close_at_end) nc_close(ncc)

  if (ncatts_in_df) {

    for (it in names(variable_attributes)) {
      out[,paste("variable",it,sep="_")] <- variable_attributes[[it]]
    }
    for (it in names(global_attributes)) {
      out[,it] <- global_attributes[[it]]
    }
  } else {
    attr(out,"global_attributes") <- global_attributes
    attr(out,"variable_attributes") <- variable_attributes
  }

  return(out)
}


