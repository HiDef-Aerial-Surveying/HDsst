#' Plot one NCDF file downloaded from GHRSST
#'
#' Reads a netCDF file and the plots the output
#'
#' @param filepath A character string for the filename. If this argument is defined, plot it.
#' @param rast A RasterLayer object. If this argument is defined, plot the raster
#' @param bbox (optional) A vector in the format Xmin, Xmax, Ymin, Ymax for plotting.
#' @return A levelplot object from the rasterVis library
#' @export
#' @import ncdf4
#' @import raster
#' @import rasterVis
#' @examples
#' P <- plot.sst(file="/ncdata.ncf",bbox=c(-15,10,45,62))

plot.sst <- function(filepath=NULL,rast=NULL,bbox=c(-15,10,45,62)) {

  if(!is.null(filepath)){
    r <- nc_to_raster(filepath,bbox)
  }
  if(!is.null(rast)){
    r <- rast
  }

  ## Return the levelplot from rasterVis
  return(levelplot(r,layers=1,margin=FALSE,contour=TRUE))

}
