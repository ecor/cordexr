NULL
#' Extraction of CORDEX grid  values
#'
#' @param nc file template name  provided by CORDEX / EURO-CORDEX projections. See examples
#' @param ... further arguments for \code{\link{cordex_grid_cell_values}}
#' @param rlat rotated latitude values
#'
#'
#' @export
#'
#' @examples
#'
#' nc <- ''/home/ecor/local/data/climate/cordex/europe/monthly2/projections-cordex-domains-single-levels_cccma_canesm2_clmcom_clm_cclm4_8_17_monthly_historical_1981_1990.zip
#'
#'


cordex_grid_cell_from_model <- function (nc,...,
                                         gcm_models=c('mpi_m_mpi_esm_lr','cccma_canesm2', 'mohc_hadgem2_es','miroc_miroc5','ichec_ec_earth'),
                                         rcm_models=c('clmcom_clm_cclm4_8_17','clmcom_eth_cosmo_crclim','cnrm_aladin63','gerics_remo2015','ictp_regcm4_6','smhi_rca4','uhoh_wrf361h','clmcom_clm_cclm4_8_17','dmi_hirham5','knmi_racmo22e','mohc_hadrem3_ga7_05','mpi_csc_remo2009'),
                                         ,rcm_model,rcp_scenarios=c('historical','rcp_4_5','rcp_8_5'),ensample=c(""),start_year,end_year) {


  ###path <- '/home/ecor/local/data/climate/cordex/europe/monthly/projections-cordex-domains-single-levels_XGCM_XRCM_monthly_XRCP_XSTART_XEND.zip'
  path <- '/home/ecor/local/data/climate/cordex/europe/monthly2/projections-cordex-domains-single-levels_XGCM_XRCM_monthly_XRCP_XSTART_XEND.zip'
  path2 <- '/home/ecor/local/data/climate/cordex/europe/monthly_unzipped/'
  ###
  gcm_models=c('mpi_m_mpi_esm_lr','cccma_canesm2', 'mohc_hadgem2_es','miroc_miroc5','ichec_ec_earth')
  rcm_models=c('clmcom_clm_cclm4_8_17','clmcom_eth_cosmo_crclim','cnrm_aladin63','gerics_remo2015','ictp_regcm4_6','smhi_rca4','uhoh_wrf361h','clmcom_clm_cclm4_8_17','dmi_hirham5','knmi_racmo22e','mohc_hadrem3_ga7_05','mpi_csc_remo2009')
  rcp_scenarios=c('historical','rcp_4_5','rcp_8_5')[1]
  start_year <- 1981
  end_year <- 1990
  ###
  df <- array(path,c(length(gcm_models),length(rcm_models),length(rcp_scenarios),length(start_year),length(end_year))) %>% melt()
  names(df) <- c("gcm_model","rcm_model","rcp_scenario","start_year","end_year","file")
  df$gcm_model    <- gcm_models[df$gcm_model]
  df$rcm_model    <- rcm_models[df$rcm_model]
  df$rcp_scenario <- rcp_scenarios[df$rcp_scenario]
  df$start_year <- start_year[df$start_year]
  df$end_year <- end_year[df$end_year]
  ###

  df$file <- df$file %>% str_replace_all("XGCM",df$gcm_model) %>% str_replace_all("XRCM",df$rcm_model)
  df$file <- df$file %>% str_replace_all("XRCP",df$rcp_scenario)
  df$file <- df$file %>% str_replace_all("XSTART",as.character(df$start_year)) %>% str_replace_all("XEND",as.character(df$end_year))

  df$exists <- file.exists(df$file)


  for (it in df$file[df$exist]) {

    unzip(it,exdir=path2,overwrite=TRUE)

  }
  ###
  ncs <- eurocordexr::get_inventory(path2,add_files=TRUE)

  ###
  uu <- rotpole_nc_point_to_dt(ncs$list_files[[1]],ncs$variable[1],c(11.2,11.31), c(45.9,46.5),interpolate_to_standard_calendar=TRUE,add_grid_coord=TRUE)
  uu <- map(c(11.2,11.31),point_lat=46,rotpole_nc_point_to_dt,filename=ncs$list_files[[1]],variable=ncs$variable[1],interpolate_to_standard_calendar=TRUE,add_grid_coord=TRUE)




  out <- NULL



  return(out)

}
