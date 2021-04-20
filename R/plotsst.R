#' Plot one NCDF file downloaded from GHRSST
#'
#' Reads a netCDF file and the plots the output
#'
#' @param filepath A character string for the filename
#' @param bbox (optional) A vector in the format Xmin, Xmax, Ymin, Ymax for plotting.
#' @return A levelplot object from the rasterVis library
#' @export
#' @import ncdf4
#' @import raster
#' @import rasterVis
#' @examples
#' P <- plot.sst(file="/ncdata.ncf",bbox=c(-15,10,45,62))

plot.sst <- function(filepath,bbox=c(-15,10,45,62)) {

  r <- nc_to_raster(filepath,bbox)

  ## Return the levelplot from rasterVis
  return(levelplot(r,layers=1,margin=FALSE,contour=TRUE))

}
