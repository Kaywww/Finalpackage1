#' Summary of time series
#'
#' @param timeseries a vector/column of a dataframe which is going to be changed into timeseries
#' @param startdate start date of the time series
#' @param enddate end date of the time series
#' @param freq frequency of the time series: 1=annual, 12=monthly, 4=quarterly
#' @param plot logical; present the graph of the time series or not
#' @param decompose logical; graph with the decomposition of the time series or not
#'
#' @return
#' @export
#'
#' @examples
#' summary_ts(data1[,1],startdate=c(2003,5),enddate=c(2020,10),freq=12,plot=TRUE,decompose=TRUE)
summary_ts=function(timeseries,startdate,enddate,freq,plot=FALSE,decompose=FALSE){
  timeseries=ts(timeseries,start=startdate,end=enddate,frequency = freq)
  if(plot==FALSE){
    return(summary(timeseries))
  }else if(plot==TRUE && decompose==FALSE){
    tseries::tsdisplay(timeseries)
  }else{
    stl(timeseries, s.window = "periodic")
  }
}
