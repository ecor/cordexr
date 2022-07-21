NULL
#' Extraction of CORDEX netcdf attributes from several Netcdf file sources
#'
#' @param nc file template name  provided by CORDEX / EURO-CORDEX projections. See examples
#' @param ... further arguments for \code{\link{cordex_netcdf_attributes}}
#'
#
#'
#' @export
#'
#' @examples
#'
#' ncs <- '/home/ecor/local/data/climate/cordex/europe/monthly_unzipped/' %>% list.files(full.name=TRUE,pattern=".nc")
#' out <- cordex_netcdf_attributes_multi_sources(ncs)
#'
#'
#'

cordex_netcdf_attributes_multi_sources <- function (nc,...){


  out <-  map(nc,cordex_netcdf_attributes,ncatts_in_df=TRUE,...) ##%>% do.call(what="rbind")
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
