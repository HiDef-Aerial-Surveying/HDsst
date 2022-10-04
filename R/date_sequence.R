#' SST data extracted to spatial points
#'
#' @param year A character string.
#' @param month An integer.
#' @param day An integer.
#' @param buffer An integer. The number of days on each side of the date you want
#' returned as a list of dates.
#' @export
#' @return A sequence of dates
#' @examples
#' date_seq <- date.sequence(year="2015", month="01", day="10", day.buffer=30)
#'

date.sequence <- function(year,month,day,day.buffer){
  mdtime <- as.POSIXct(paste0(year,month,day),format="%Y%m%d")

  sec.p.day <- 86400

  if(paste0(year,month,day) == "20130101"){
    sttime <- as.POSIXct("20130101",format="%Y%m%d")
  }else{
    sttime <- mdtime - (7*sec.p.day)
  }

  if(paste0(year,month,day) == "20210228"){
    entime <- as.POSIXct("20210228",format="%Y%m%d")
  }else{
    entime <- mdtime + (7*sec.p.day)
  }
  ## Create a sequence of dates
  dateseq <- seq(as.Date(sttime), as.Date(entime), by="days")
  return(dateseq)
}

