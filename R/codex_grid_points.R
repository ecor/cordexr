NULL
#' Extraction of CORDEX grid centers of cells (lat/lon coordinates)
#'
#' @param nc file (or connection to)  provided by CORDEX / EURO-CORDEX projections
#' @param crs CRS for output. Used in case \code{return_sf==TRUE} or \code{crop_sf} is not null.
#' @param dim_x possible names for x/longitude dimension
#' @param dim_y possible names for y/latitude dimension
#' @param return_sf logical variable. If it is \code{TRUE} , it return \code{\link{sf}} object.
#' @param crop_sf spatial feature or an extent on which the netcdf data has been cropped.
#' @param ... further arguments
#'
#'
#' @importFrom dplyr full_join summarize ungroup group_by
#' @importFrom sf st_as_sf st_crop st_polygon st_coordinates
#'
#' @seealso \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cordex-domains-single-levels?tab=overview})
#' @export
#' @examples
#'
#' nc <- system.file('netcdfs/pr_EUR-11_CCCma-CanESM2_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_mon_208101-209012.nc',package="cordexr")
#' out <- cordex_grid_points(nc)
#'
#' library(RMAWGEN)
#' data(trentino)
#' stations <- cbind(as.data.frame(STATION_LATLON),STATION_NAMES) %>% st_as_sf(coords=c(1,2),crs=4326)
#' out <- cordex_grid_points(nc,crop_sf=stations)
#'
#'
#'




cordex_grid_points <- function(nc,...,dim_x=c("rlon","x","auto"),dim_y=c("rlat","y","auto"),crs=4326,return_sf=TRUE,crop_sf=NULL) {


  if (class(nc)=="ncdf4") {
    nc_close_at_end <- FALSE
    ncc <- nc
  } else {
    ncc <- nc_open(filename=nc)
    nc_close_at_end <- FALSE
  }
  #####
  dim_x <- which_ncvar_name(ncc,dim_x)
  dim_y <- which_ncvar_name(ncc,dim_y)
  #####

  out <- list()
  out$lon <- ncvar_get(ncc,varid="lon") %>% melt()
  out$lat <- ncvar_get(ncc,varid="lat") %>% melt()
  rlon <- ncvar_get(ncc,varid=dim_x)
  rlat <- ncvar_get(ncc,varid=dim_y)
 ## out$nvertex <- ncvar_get(ncc,varid="vertices")

  names(out$lon) <- c(dim_x,dim_y,"lon")
  names(out$lat) <- c(dim_x,dim_y,"lat")
  out <- dplyr::full_join(out$lon,out$lat)
  out[,dim_y] <- rlat[out[,dim_y]]
  out[,dim_x]  <- rlon[out[,dim_x]]

  if (return_sf) {

    out <- out %>% st_as_sf(coords=c("lon","lat"),crs=crs)
    if (!is.null(crop_sf)) out <- st_crop(x=out,y=crop_sf)
  }
  attr(out,"global_attributes") <- ncatt_get(ncc,varid=0)
  attr(out,sprintf("%s_attributes",dim_x)) <- ncatt_get(ncc,varid=dim_x)
  attr(out,sprintf("%s_attributes",dim_x)) <- ncatt_get(ncc,varid=dim_y)
  if (nc_close_at_end) nc_close(ncc)


  return(out)


}
