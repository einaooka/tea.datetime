
#' @title Convert hourly data to HLH/LLH Daily
#'
#' @description Given 24 hour matrix, it returns HLH/LLH Daily data frame. 
#'
#' @param date Consecutive daily date data in 'date' format.
#' @param mtx A matrix with 24 columns corresponding to each hour. Number of rows need to match the length of date.  
#'
#' @export CalcDailyPeakPeriods
#'
#' @details  WECC definition was used in HLH/LLH split. 
#'
#' @examples
#' dates <- seq(as.Date("2000-1-1"), length.out = 20, by="day")
#' mtx <- matrix(1:(24*20), ncol=24)
#' CalcDailyPeakPeriods(dates, mtx, colname="Load_aMW_")
#'

CalcDailyPeakPeriods <- function(date, mtx, colname=""){
  
  date.df <- GetDateTable(date)
  
  HLH <- rowMeans(mtx[,7:22])
  LLH <- rowMeans(mtx[,c(1:6,23,24)])
  HLH[!date.df$Peak] <- NA
  LLH[!date.df$Peak] <- rowMeans(mtx[!date.df$Peak,])
  
  df <- data.frame("Date" = date, "HLH" = HLH, "LLH" = LLH)
  names(df)[-1] <- paste0(colname, names(df)[-1])
  
  return(df)
}