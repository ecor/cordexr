

rm(list=ls())

library(cordexr)
sf_use_s2(FALSE)
ncs <- '/home/ecor/local/data/climate/cordex/europe/monthly_unzipped/' %>% list.files(full.name=TRUE,pattern=".nc")


library(RMAWGEN)
data(trentino)
stations <- cbind(as.data.frame(STATION_LATLON),STATION_NAMES) %>% st_as_sf(coords=c(1,2),crs=4326)
rlatlon <- cordex_grid_vertices(ncs[1],crop_sf=stations)

out <- cordex_grid_cell_values_multi_sources(ncs[1:3],rlon=rlatlon$rlon,rlat=rlatlon$rlat)
##cc <- system.time(out <- cordex_grid_cell_values_multi_sources(ncs[1:3],rlon=rlatlon$rlon,rlat=rlatlon$rlat))
cc <- system.time(out <- cordex_grid_cell_values_multi_sources(ncs[-c(22,24,25,37,40,47,49,52,56,60,62,65,83,86,89,92,104,107,114,116,119,123,127,129,132,154,156,157,169,172,179,181,184,188,192,194,197,219,222,234,237,244,246,249,253,257,259,262)],rlon=rlatlon$rlon,rlat=rlatlon$rlat))
dr_exp <- unique(out$driving_experiment)[1]
outdd <- out[which(out$driving_experiment==dr_exp),]
str(outdd$driving_experiment)
str(outdd$driving_model_id)
unique(outdd$rcm_version_id)
meta_out <- out %>% group_by(driving_model_id,model_id,experiment_id,variable) %>% summarize() %>% ungroup()

#########
#ncs <- '/home/ecor/local/data/climate/cordex/europe/monthly_unzipped/' %>% list.files(full.name=TRUE,pattern=".nc")
#' ##ncs[-c(22,24,25,37,40,47,49,52,56,60,62,65,83,86,89,92,104,107,114,116,119,123,127,129,132,154,156,157,169,172,179,181,184,188,192,194,197,219,222,234,237,244,246,249,253,257,259,262)
source("~/local/rpackages/rendena100/cordexr/R/codex_grid_vertices.R")
source("~/local/rpackages/rendena100/cordexr/R/codex_grid_points.R")

####

####

rlatlon_vert <- cordex_grid_vertices(ncs[1],crop_sf=stations)
rlatlon_pnt <- cordex_grid_points(ncs[1],crop_sf=stations)


library(terra)
uu <- rast(ncs[22])
xy_vert <- cordex_grid_vertices(ncs[22],dim_x="x",dim_y="y",lon_vertices="bounds_lon",lat_vertices="bounds_lat",crop_sf=stations)
xy_vert <- cordex_grid_vertices(ncs[22],dim_x="x",dim_y="y",lon_vertices="bounds_lon",lat_vertices="bounds_lat",crop_sf=stations)
xy_vert1 <- cordex_grid_vertices(ncs[24],dim_x="rlon",dim_y="rlat",lon_vertices="lon_vertices",lat_vertices="lat_vertices",crop_sf=stations)


