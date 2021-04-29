#' SST data extracted to spatial points
#'
#' @param year A character string.
#' @param month An integer.
#' @param day An integer.
#' @param ts A character string of either: daily, biweekly, monthly or custom. 'daily' will return SST for
#'           the specified date, 'biweekly' will return mean, median and sd of SST for 2 weeks around the specified
#'           date (1 week on each side), 'monthly will return mean, median and sd of SST for 4 weeks around the specified
#'           date (2 weeks on each side). 'custom' will return mean, median and sd of SST for a custom date range
#'           as defined by the "time.range" argument.
#' @param time.range (only works with ts='custom') A vector of date-time strings in the form c("YYYYMMDD","YYYYMMDD") where the first
#'           element is the start time, and the second element is the finish time.
#' @param spatd A spatial points dataframe.
#' @param sst.grad A boolean. TRUE will add the spatial gradient of SST
#' @param epsg An integer. The EPSG number for the desired projection returned by the function
#' @return A spatial points dataframe with a SST column(s) appended.
#' @importFrom sf st_crs
#' @importFrom sp spTransform
#' @importFrom sp proj4string
#' @import raster
#' @export
#' @examples
#'
#' spatd <- raster::shapefile("/shapedata.shp")
#'
#' ## Then one of:
#' spatd <- sst_to_points(year="2013",month="01",day="01",ts="daily",spatd=spatd,sst.grad=TRUE,epsg=4326)
#'
#' spatd <- sst_to_points(year="2015",month="01",day="01",ts="biweekly",spatd=spatd,sst.grad=TRUE,epsg=4326)
#'
#' spatd <- sst_to_points(year="2015",month="01",day="01",ts="monthly",spatd=spatd,sst.grad=TRUE,epsg=4326)
#'
#' spatd <- sst_to_points(year="2015",month="01",day="01",ts="custom",time.range=c('20150305','20150318'),spatd=spatd,sst.grad=TRUE,epsg=4326)
#'

sst_to_points <- function(year="2013",month="01",day="01",
                          ts = c("daily","biweekly", "monthly", "custom"),
                          time.range = NULL,
                          spatd,sst.grad=FALSE,epsg=NULL){


  ## transform to the requested EPSG
  spat.proj <- sf::st_crs(epsg)$proj4string
  spatd <- sp::spTransform(spatd,spat.proj)

  sec.p.day <- 24*60*60
  if(ts == "daily"){
    ## extract_sst function from HDsst
    r <- extract_sst(year,month,day)

  }else if(ts == "biweekly"){
    DateS <- date.sequence(year,month,day,7)
    r <- raster_aggregation(DateS)

  }else if(ts == "monthly"){
    DateS <- date.sequence(year,month,day,30)
    r <- raster_aggregation(DateS)
  }else if(ts == "custom"){

    if(typeof(time.range)!= "character"){
      stop("time range is not a character vector")
    }

    sttime <- as.POSIXct(time.range[1],format="%Y%m%d")
    entime <- as.POSIXct(time.range[2],format="%Y%m%d")

    if(sttime > entime){
      stop(paste("end time of:",time.range[2],"is earlier than start time of:",time.range[1]))
    }

    DateS <- seq(as.Date(sttime), as.Date(entime), by="days")
    r <- raster_aggregation(DateS)
  }

  ## check if EPSG of points is equal to the raster, if not, transform
  if(identical(sp::proj4string(spatd),sp::proj4string(r))==FALSE){
    cat(paste0('Warning: projections not identical, reprojecting to ESPG:',epsg,'\n'))
    r <- raster::projectRaster(r,crs=spat.proj)
  }
  if(length(names(r)) > 1){
    if(sst.grad){
      sstgrad <- raster::terrain(r$layer.1,opt="slope")
      sststack <- raster::stack(sstgrad,r)
      extrd <- as.data.frame(raster::extract(sststack,spatd))
      names(extrd) <- c("SSTgrad","SSTmean","SSTmedian","SSTstd")
    }else{
      extrd <- as.data.frame(raster::extract(r,spatd))
      names(extrd) <- c("SSTmean","SSTmedian","SSTstd")
    }
  }else if (length(names(r)) == 1){
    ## If sst gradient is requested, then calculate it
    if(sst.grad){
      sstgrad <- raster::terrain(r,opt="slope")
      sststack <- raster::stack(sstgrad,r)
      extrd <- as.data.frame(raster::extract(sststack,spatd))
      names(extrd) <- c("SSTgrad","SST")
    }else{
      extrd <- as.data.frame(raster::extract(r,spatd))
      names(extrd) <- c("SST")
    }
  }

  spatd <- cbind(spatd,extrd)
  return(spatd)

}
