NULL
#' Extraction of CORDEX grid  values from several Netcdf file sources from CORDEX
#'
#' @param nc file template name  provided by CORDEX / EURO-CORDEX projections. See examples
#' @param ... further arguments for \code{\link{cordex_grid_cell_values}}
#'
#
#'
#' @export
#'
#' @examples
#'
#' ncs <- '/home/ecor/local/data/climate/cordex/europe/monthly_unzipped/' %>% list.files(full.name=TRUE,pattern=".nc")
# ncs <- ncs[-c(2,4,7)]
#'
#' library(RMAWGEN)
#' data(trentino)
#' stations <- cbind(as.data.frame(STATION_LATLON),STATION_NAMES) %>% st_as_sf(coords=c(1,2),crs=4326)
#' rlatlon <- cordex_grid_vertices(ncs[1],crop_sf=stations)
#' \dontrun{
#' out <- cordex_grid_cell_values_multi_sources(ncs[1:3],rlon=rlatlon$rlon,rlat=rlatlon$rlat)
#' cc <- system.time(out <- cordex_grid_cell_values_multi_sources(ncs[1:3],rlon=rlatlon$rlon,rlat=rlatlon$rlat))
#' cc <- system.time(out <- cordex_grid_cell_values_multi_sources(ncs[-c(22,24,25,37,40,47,49,52,56,60,62,65,83,86,89,92,104,107,114,116,119,123,127,129,132,154,156,157,169,172,179,181,184,188,192,194,197,219,222,234,237,244,246,249,253,257,259,262)],rlon=rlatlon$rlon,rlat=rlatlon$rlat))
#' dr_exp <- unique(out$driving_experiment)[1]
#' outdd <- out[which(out$driving_experiment==dr_exp),]
#' str(outdd$driving_experiment)
#'  ##chr [1:116160] "CCCma-CanESM2, historical, r1i1p1" "CCCma-CanESM2, historical, r1i1p1" "CCCma-CanESM2, historical, r1i1p1" ...
#' str(outdd$driving_model_id)
#  ###$ chr [1:116160] "CCCma-CanESM2" "CCCma-CanESM2" "CCCma-CanESM2" "CCCma-CanESM2" "CCCma-CanESM2" "CCCma-CanESM2" "CCCma-CanESM2" "CCCma-CanESM2" ...
#' ### > str(outdd$rcm_version_id)
# chr [1:116160] "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" "v1" ...
#' unique(outdd$rcm_version_id)
#  ###[1] "v1"
#' }



#
# [1] -2.431870e-05 -3.910065e-05 -2.336502e-05 -7.152557e-06 -2.193451e-05 -3.671646e-05 -2.098083e-05 -3.528595e-05 -1.955032e-05 -3.433228e-05
# [11] -1.811981e-05
# [1] "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmax_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_mon_198101-199012.nc"
# [1] -5.22 -5.11 -5.00 -4.89 -4.78 -4.67 -4.56 -4.45 -4.34 -4.23
# [1] -5.275 -5.165 -5.055 -4.945 -4.835 -4.725 -4.615 -4.505 -4.395 -4.285 -4.175
# Error in ncrlon[ilon[1]:ilon[2]] - unique(rlon) : non-conformable arrays
# Called from: print(ncrlon[ilon[1]:ilon[2]] - unique(rlon))
# Browse[1]> Q
# Timing stopped at: 51.26 2.866 95.76
# > ll <-  "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmax_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_mon_198101-199012.nc"
# >
#   > which(nc==ll)
# [1] 184
# >
# [1] "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmax_EUR-11_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# [1] -5.22 -5.11 -5.00 -4.89 -4.78 -4.67 -4.56 -4.45 -4.34 -4.23
# [1] -5.275 -5.165 -5.055 -4.945 -4.835 -4.725 -4.615 -4.505 -4.395 -4.285 -4.175
# Error in ncrlon[ilon[1]:ilon[2]] - unique(rlon) : non-conformable arrays
# Called from: print(ncrlon[ilon[1]:ilon[2]] - unique(rlon))
# Browse[1]>
#   Timing stopped at: 52.19 2.973 78.08
# > ll <- "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmax_EUR-11_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# > which(nc==ll)
# [1] 188
# >
# [1] "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmax_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# [1] -5.22 -5.11 -5.00 -4.89 -4.78 -4.67 -4.56 -4.45 -4.34 -4.23
# [1] -5.275 -5.165 -5.055 -4.945 -4.835 -4.725 -4.615 -4.505 -4.395 -4.285 -4.175
# Error in ncrlon[ilon[1]:ilon[2]] - unique(rlon) : non-conformable arrays
# Called from: print(ncrlon[ilon[1]:ilon[2]] - unique(rlon))
# Browse[1]>
#   Timing stopped at: 57.51 6.175 418.3
# >  197
# Error in ncrlon[ilon[1]:ilon[2]] - unique(rlon) : non-conformable arrays
# Called from: print(ncrlon[ilon[1]:ilon[2]] - unique(rlon))
# Browse[1]> Q
# Timing stopped at: 69.87 4.745 226.9
# > ll <- "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmin_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_mon_198101-199012.nc"
# > which(nc==ll)
# [1] 249
#
# [1] "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmin_EUR-11_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# [1] -5.22 -5.11 -5.00 -4.89 -4.78 -4.67 -4.56 -4.45 -4.34 -4.23
# [1] -5.275 -5.165 -5.055 -4.945 -4.835 -4.725 -4.615 -4.505 -4.395 -4.285 -4.175
# Error in ncrlon[ilon[1]:ilon[2]] - unique(rlon) : non-conformable arrays
# Called from: print(ncrlon[ilon[1]:ilon[2]] - unique(rlon))
# Browse[1]> Q
# Timing stopped at: 69.96 3.94 139.8
# > ll <- "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmin_EUR-11_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# > which(nc==ll)
# [1] 253
# >
#
# [1] "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmin_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# [1] -5.22 -5.11 -5.00 -4.89 -4.78 -4.67 -4.56 -4.45 -4.34 -4.23
# [1] -5.275 -5.165 -5.055 -4.945 -4.835 -4.725 -4.615 -4.505 -4.395 -4.285 -4.175
# Error in ncrlon[ilon[1]:ilon[2]] - unique(rlon) : non-conformable arrays
# Called from: print(ncrlon[ilon[1]:ilon[2]] - unique(rlon))
# Browse[1]>
#   Timing stopped at: 71.46 3.852 84.85
# > ll <- "/home/ecor/local/data/climate/cordex/europe/monthly_unzipped//tasmin_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_mon_208101-209012.nc"
# > which(nc==ll)
# [1] 262
# >
#

cordex_grid_cell_values_multi_sources <- function (nc,...){


  out <-  map(nc,cordex_grid_cell_values,ncatts_in_df=TRUE,...) ##%>% do.call(what="rbind")
  ###nn <- map(out,names) %>% unlist() %>% unique()

  if (length(out)>1) {
    out2 <- out[[1]]
    if ("realization" %in% names(out2)) out2$realization <- as.character(out2$realization)
    for (i in 2:length(out)) {


      nn <- !(names(out2) %in% names(out[[i]]))
      if (length(nn)>0) { out[[i]][,names(out2)[nn]] <- NA}
      nq <- !(names(out[[i]]) %in% names(out2))
      if (length(nq)>0) { out2[,names(out[[i]])[nq]] <- NA}
      out[[i]] <- out[[i]][,names(out2)]

      out2 <- rbind(out2,out[[i]])
    ##  out2 <-



    }
    out <- out2

  }

 ###


  return(out)

}
