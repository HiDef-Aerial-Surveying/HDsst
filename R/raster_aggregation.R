#' Create mean, median and sd rasters of SST from a sequence of dates
#'
#' @param Dates A sequence of dates output from date.sequence()
#' @return A raster stack of 3 raster layers (mean, median and standard deviation)
#' @importFrom raster stack
#' @importFrom raster calc
#' @export
#' @examples
#'
#' year = "2018"
#' month = "06"
#' day = "10"
#' DateS <- date.sequence(year,month,day,7)
#' Rs <- raster_aggregation(DateS)
#'
raster_aggregation <- function(DateS){
  rstack <- raster::stack()
  for(i in 1:length(DateS)){
    Date <- DateS[i]
    print(Date)
    vs <- strsplit(as.character(Date),"-")[[1]]
    r <- extract_sst(year=vs[1],month=vs[2],day=vs[3])
    rstack <- raster::stack(rstack,r)
  }

  MEAN_SST <- raster::calc(rstack,mean)
  MEDIAN_SST <- raster::calc(rstack,median)
  STD_SST <- raster::calc(rstack,sd)

  return(stack(MEAN_SST,MEDIAN_SST,STD_SST))
}

