#' Getdowm FRED dataset
#'
#' @param APIkey personal FRED.api to get data down
#' @param series_id Abbreviation of the name of the time series you want
#' @param observation_start start date of your data
#' @param observation_end end date of your data
#' @param frequency frenquency of the time series: a = annual, m = monthly, w=weekly, d = daily
#'
#' @return
#' @export
#'
#' @examples
#' fred_series(APIkey="54b39d1f56f337fef917b0b1592874cc",series_id="CAUR",observation_start="1979-01-01",observation_end="2019-01-01",frequency="a")
fred_series=function(APIkey,series_id,observation_start,
                     observation_end,frequency){
  URL="https://api.stlouisfed.org/fred/series/observations"
  parameters=paste0(
    "?series_id=",series_id,
    "&api_key=",APIkey,
    "&observation_start=",observation_start,
    "&observation_end=",observation_end,
    "&frequency=",frequency,
    "&file_type=json"
  )
  PATH=paste0(URL,parameters)
  initialqury=jsonlite::fromJSON(PATH)
  df=initialqury$observations
  df=df[c("value")]
  df$value=as.numeric(df$value)
  return(df)
}
