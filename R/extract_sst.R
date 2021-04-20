#' Extract SST for a specific date/time
#'
#' @param year A character string.
#' @param month An integer.
#' @param day An integer.
#' @param bbox (optional) A vector in the format Xmin, Xmax, Ymin, Ymax for plotting.
#' @param plot.it A boolean. TRUE will plot the output with a contoured levelplot
#' @return A raster object
#' @export
#' @examples
#' r <- extract_sst(year,month,day,bbox=c(-15,10,45,62))
#'

extract_sst <- function(year="2013",month="01",day="01",bbox=c(-15,10,45,62)){
  ## NOTE - must be connected to VPN or will get booted
  print("extracting sst data... may take time depending on VPN connection...")
  NASdir <- "\\\\NAS2/EnvironmentalCovariates/jplMURSST41/"
  path_build <- paste0(NASdir,
                       year,
                       month,
                       day,
                       "090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1_subset.nc")
  r <- nc_to_raster(filepath = path_build,bbox=bbox)

  return(r)
}
