NULL
#' Extraction of CORDEX grid  cells (lat/lon coordinates of the polygons)
#
#' @param nc file (or connection to)  provided by CORDEX / EURO-CORDEX projections
#' @param nvertex number of grid vertices. Default is 4.
#' @param dim_x possible names for x/longitude dimension
#' @param dim_y possible names for y/latitude dimension
#' @param lon_vertices,lat_vertices possible names for possible names of grid vertex longitude and latitude.
#' @param crs CRS for output. Used in case \code{return_sf==TRUE} or \code{crop_sf} is not null.
#' @param return_sf logical variable. If it is \code{TRUE} , it return \code{\link{sf}} object.
#' @param crop_sf spatial feature or an extent on which the netcdf data has been cropped.
#' @param buffer buffer in coordinate unit (e.g. lat/lon degrees) in order to increase the extent or bounding box of \code{crop_sf}.
#' @param ... further arguments
#'
#' @importFrom dplyr full_join
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom sf st_crop st_as_sf st_bbox
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#'
#' @export
#'
#'
#' @examples
#'
#' nc <- system.file('netcdfs/pr_EUR-11_CCCma-CanESM2_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_mon_208101-209012.nc',package="cordexr")
#' out <- cordex_grid_vertices(nc)
#'
#'
#' \dontrun{
#' ## mapview::mapview(out)
#' }
#'
#' library(RMAWGEN)
#' data(trentino)
#' stations <- cbind(as.data.frame(STATION_LATLON),STATION_NAMES) %>% st_as_sf(coords=c(1,2),crs=4326)
#' out <- cordex_grid_vertices(nc,crop_sf=stations)
#'
#' plot(st_geometry(out))
#' plot(stations,add=TRUE)
#' ## LOOK AT ST_WITHIN
#' station_cell <- st_within(stations,out,sparse=TRUE) %>% sapply(function(x){x[1]}) ## see buffer
#' stations$nccell <- station_cell
#'
#' out2 <- as.data.frame(out[station_cell,]) %>% cbind(stations)
#'
#' station_cell <- st_within(stations,out) %>% unlist()## see buffer
#' \dontrun{
#' ncs <- '/home/ecor/local/data/climate/cordex/europe/monthly_unzipped/' %>% list.files(full.name=TRUE,pattern=".nc")
#' ##ncs[-c(22,24,25,37,40,47,49,52,56,60,62,65,83,86,89,92,104,107,114,116,119,123,127,129,132,154,156,157,169,172,179,181,184,188,192,194,197,219,222,234,237,244,246,249,253,257,259,262)
#'
#' }
cordex_grid_vertices <- function(nc,...,dim_x=c("rlon","x","auto"),dim_y=c("rlat","y","auto"),lon_vertices=c("lon_vertices","bounds_lat","auto"),lat_vertices=c("lat_vertices","bounds_lat","auto"),nvertex=4,crs=4326,return_sf=TRUE,crop_sf=NULL,buffer=0.1) {





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
  lon_vertices <- which_ncvar_name(ncc,lon_vertices)
  lat_vertices <- which_ncvar_name(ncc,lat_vertices)

  #####
  out <- list()
  out$lon_vertices <- ncvar_get(ncc,varid=lon_vertices) %>% melt()

  out$lat_vertices <- ncvar_get(ncc,varid=lat_vertices) %>% melt()

  rlon <- ncvar_get(ncc,varid=dim_x)
  rlat <- ncvar_get(ncc,varid=dim_y)
 ## out$nvertex <- ncvar_get(ncc,varid="vertices")

  names(out$lon_vertices) <- c("vertices",dim_x,dim_y,"lon")
  names(out$lat_vertices) <- c("vertices",dim_x,dim_y,"lat")
  out <- dplyr::full_join(out$lon_vertices,out$lat_vertices)
  out[,dim_y] <- rlat[out[,dim_y]]
  out[,dim_x] <- rlon[out[,dim_x]]
  if (return_sf) {

    ####out <- out %>% st_as_sf(coords=c("lon","lat"),crs=crs) %>% dplyr::group_by(rlon,rlat) %>% dplyr::mutate(geometry2=st_combine(geometry))
   # out$coordx <- 0
    #out$coordy <- 0
   ##### out <- out   %>% st_as_sf(crs=crs,ccords="coordx","coordy") %>% group_by(rlon,rlat) %>% summarize(geometry=st_polygon(list(cbind(lon[c(1:4,1)],lat[c(1:4,1)]))))  %>% ungroup()  ###st_as_sf(coords=c("lon","lat"),crs=crs)
  ##  out <- out %>% group_by(rlon,rlat) %>% summarize(geometry=st_as_sfc(st_polygon(list(cbind(lon[c(1:4,1)],lat[c(1:4,1)])))))  %>% ungroup()  ###st_as_sf(coords=c("lon","lat"),crs=crs)
   ## out <- out %>% group_by(rlon,rlat) %>% summarize(geometry=((list(cbind(lon[c(1:4,1)],lat[c(1:4,1)])))))  %>% ungroup() ##%>%
    names(out)[names(out)==dim_x] <- "xr"
    names(out)[names(out)==dim_y] <- "yr"
    out <- out %>% group_by(.data$xr,.data$yr) %>% summarize(geometry=((list(cbind(.data$lon[c(1:nvertex,1)],.data$lat[c(1:nvertex,1)])))))  %>% ungroup()
    out$geometry  <- map(out$geometry,list) %>% map(st_polygon)
    out <- out %>% st_as_sf(crs=crs)
    names(out)[names(out)=="xr"] <- dim_x
    names(out)[names(out)=="yr"] <- dim_y

    if (!is.null(crop_sf)) {
      out <- st_crop(x=out,y=st_bbox(crop_sf)+c(-1,-1,1,1)*buffer)
    }
  }

  attr(out,"global_attributes") <- ncatt_get(ncc,varid=0)
  attr(out,sprintf("%s_attributes",dim_x)) <- ncatt_get(ncc,varid=dim_x)
  attr(out,sprintf("%s_attributes",dim_y)) <- ncatt_get(ncc,varid=dim_y)
  if (nc_close_at_end) nc_close(ncc)

  return(out)


}
