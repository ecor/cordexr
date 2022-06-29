NULL
#' Extraction of CORDEX grid  values from points
#'
#' @param nc file (or connection to)  provided by CORDEX / EURO-CORDEX projections
#' @param crop_sf spatial feature containing the points and on which the netcdf data has been cropped
#' @param point_id_name point id names in \code{crop_sf}.
#' @param names_xy  names for x and y columns for the function value. Default is \code{c("x","y")}.
#' @param type option. Default is \code{c("grid","point")[1]} (only first element, \code{"grid"} is considered). See function internal code for details.
#' @param ... further arguments to be implemented


#' @seealso \code{\link{cordex_grid_cell_values}},\code{\link{cordex_grid_vertices}}
#'
#'
#' @export
#'
#' @details CORDEX data are available on \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cordex-domains-single-levels?tab=overview}
#'
#' @importFrom dplyr filter
#' @importFrom sf st_within
#' @importFrom dplyr select
#'
#'
#' @examples
#'
#' nc <- system.file('netcdfs/pr_EUR-11_CCCma-CanESM2_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_mon_208101-209012.nc',package="cordexr")
#'
#' library(RMAWGEN)
#' data(trentino)
#' stations <- cbind(as.data.frame(STATION_LATLON),STATION_NAMES) %>% st_as_sf(coords=c(1,2),crs=4326)
#'
#' cc <- system.time(out <- cordex_grid_cell_value_from_points(nc,crop_sf=stations))
#'
#' ccp <- system.time(outp <- cordex_grid_cell_value_from_points(nc,crop_sf=stations,type="point"))
#'
#'
#'
#'
#'
cordex_grid_cell_value_from_points <- function(nc,crop_sf,point_id_name=c("STATION_NAMES"),names_xy=c("x","y"),type=c("grid","point"),...) {


  type <- type[1]
  if (type=="grid") {
    rlatlon0 <- cordex_grid_vertices(nc,crop_sf=crop_sf,...)
    rlatlon00 <<- rlatlon0
    station_cell0 <- st_within(crop_sf,rlatlon0,sparse=TRUE) %>% sapply(function(x){x[1]}) ## see buffer
    crop_sf$nccell <- station_cell0
    out20 <- as.data.frame(rlatlon0[station_cell0,]) %>% dplyr::select(-.data$geometry) %>% cbind(crop_sf) %>% st_as_sf()
  } else if (type=="point") {
    rlatlon0 <- cordex_grid_points(nc,return_sf=FALSE)
    crop_sf0 <- st_coordinates(crop_sf)
    station_cell0 <- list()
    for (i in 1:nrow(crop_sf0)) {

      station_cell0[[i]] <- which.min(((rlatlon0$lon-crop_sf0[i,1])^2+(rlatlon0$lat-crop_sf0[i,2])^2)^0.5)[1]
    }
    station_cell0 <- unlist(station_cell0)
    crop_sf$ncell <- station_cell0
    ## METTERE A POSTO QUI !!!

    out20 <<- as.data.frame(rlatlon0[station_cell0,]) %>% cbind(crop_sf) ##%>% st_as_sf()

  }
  out30 <- cordex_grid_cell_values(nc,rlon=rlatlon0[[1]],rlat=rlatlon0[[2]])
  ## unit <- attr(out3,"variable_attributes")   #units
  nn20 <- names(out20)[1:2]
  names(out20)[1:2] <- c("x","y")
  names(out30)[1:2] <- c("x","y")
  out <- out30 %>% filter(.data$x %in% out20$x,.data$y %in% out20$y) %>% full_join(out20)
  ####
  ipn <- which(point_id_name %in% names(out))
  point_id_name <- point_id_name[ipn]

  ####
  iout <- which(!is.na(out[,point_id_name]))
  out <- out[iout,]
  names(out)[1:2] <- nn20
  if (length(names_xy)>=2)  names(out)[1:2] <- names_xy
  ### UNIT
  return(out)
}
# sf_use_s2(FALSE)
#
# variable0 <- "pr"
# experiment_id0 <- "historical"
#
# out40 <- list()
# variables <- c("pr","tasmax","tasmin")
# for (variable0 in variables) {
#   out40[[variable0]] <- list()
#   sims2 <- sims %>% filter(experiment_id==experiment_id0,variable==variable0)
#   if (variable0=="pr") sims2 <- sims2[-c(3,4,5,6,11,12,13,15,16,17,18,21,22,23,24,25,26,28,29),]
#   if (variable0=="tasmax") sims2 <- sims2[-c(3,4,5,6,10,11,12,14,15,16,17,20,21,22,23,24,25,27,28),]
#   if (variable0=="tasmin") sims2 <- sims2[-c(3,4,5,6,10,11,12,14,15,16,17,20,21,22,23,24,25,27,28),]
#   ##for (i in (1:nrow(sims2))[-c(3,4,5,6,11,12,13,15,16,17,18,21,22,23,24,25,26,28,29)]) {
#   ##for (i in (1:nrow(sims2))[-c(3,4,5,6,10,11,12,14,15,16,17,20,21,22,23,24,25,27,28)]) {
#   ##for (i in (1:nrow(sims2))[-c(3,4,5,6,10,11,12,14,15,16,17,20,21,22,23,24,25,27,28)]) {
#   for (i in (1:nrow(sims2))) {
#     print(i)
#     nc <- sims2$file[i]
#     it <- paste(sims2$driving_model_id[i],sims$model_id[i],sep="_")
#     stations0 <- stations
#     rlatlon0 <- cordex_grid_vertices(nc,crop_sf=stations0)
#     station_cell0 <- st_within(stations0,rlatlon0,sparse=TRUE) %>% sapply(function(x){x[1]}) ## see buffer
#     stations0$nccell <- station_cell
#     out20 <- as.data.frame(rlatlon0[station_cell0,]) %>% select(-geometry) %>% cbind(stations0) %>% st_as_sf()
#     out30 <- cordex_grid_cell_values(nc,rlon=rlatlon0[[1]],rlat=rlatlon0[[2]])
#     unit <- attr(out3,"variable_attributes")#units
#     names(out20)[1:2] <- c("x","y")
#     names(out30)[1:2] <- c("x","y")
#     out40[[variable0]][[it]] <- out30 %>% filter(x %in% out20$x,y %in% out20$y) %>% full_join(out20) %>% filter(!is.na(STATION_NAMES))
#     ### UNIT
#     if (variable0=="pr") {
#
#       out40[[variable0]][[it]]$pr[out40[[variable0]][[it]]$pr<0] <- NA
#       ## units from kg m-2 s-1 to mm/hr
#
#       ##nnday <- out50$time0+months(1)-out50$time0
#       out40[[variable0]][[it]]$pr <-   out40[[variable0]][[it]]$pr/1000*1000*3600*24 ##*as.numeric(nnday)
#       out40[[variable0]][[it]]$pr[out40[[variable0]][[it]]$pr>=1000/30] <- NA
#     } else if (variable0 %in% c("tasmin","tasmax","tas")) {
#
#       out40[[variable0]][[it]][,variable0] <-   out40[[variable0]][[it]][,variable0]-273.15
#     }
#
#     ####
#     out40[[variable0]][[it]]$model <- it
#     out40[[variable0]][[it]]$variable <- variable0
#     names(out40[[variable0]][[it]])[names(out40[[variable0]][[it]])==variable0] <- "value"
#
#     if (i==1) {
#
#       nn <- names(out40[[variable0]][[it]])
#     } else {
#
#       out40[[variable0]][[it]]  <- out40[[variable0]][[it]][,nn]
#     }
#     ##
#     ## DATA CLEANING
#
#
#
#     ##
#
#   }
#   out40[[variable0]] <- out40[[variable0]] %>% do.call(what="rbind")
#
# }
#
# out40 <- out40 %>% do.call(what="rbind") %>% as_tibble()
