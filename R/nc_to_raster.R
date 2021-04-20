#' Reads a netCDF file and returns a raster
#'
#' @param filepath A character string for the filename
#' @param bbox (optional) A vector in the format Xmin, Xmax, Ymin, Ymax for plotting.
#' @return A raster object
#' @export
#' @import ncdf4
#' @import raster
#' @import rasterVis
#' @examples
#' r <- nc_to_raster(filepath="/ncdata.ncf",bbox=c(-15,10,45,62))
#'
nc_to_raster <- function(filepath,bbox=c(-15,10,45,62)){
  xmin <- bbox[1]
  xmax <- bbox[2]
  ymin <- bbox[3]
  ymax <- bbox[4]
  if(xmin < -15){
    stop("xmin out of bounds")
  }
  if(xmax > 10){
    stop("xmax out of bounds")
  }
  if(ymin < 45){
    stop("ymin out of bounds")
  }
  if(ymax > 62){
    stop("ymax out of bounds")
  }
  nc_data <- ncdf4::nc_open(filepath)
  ## Get lon and lat arrays
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  ## Get the SST array in degrees K
  sst.array <- ncdf4::ncvar_get(nc_data, "analysed_sst")
  fillvalue <- ncdf4::ncatt_get(nc_data, "analysed_sst", "_FillValue")
  ncdf4::nc_close(nc_data)
  sst.array[sst.array == fillvalue$value] <- NA

  ## This will always return the array as WGS84
  r <- raster(t(sst.array),
              xmn=min(lon),
              xmx=max(lon), ymn=min(lat), ymx=max(lat),
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

  r <- flip(r, direction='y')
  if(identical(bbox,c(-15,10,45,62))){
    return(r)
  }else{
    r <- crop(r,extent(bbox))
    return(r)
  }


}
