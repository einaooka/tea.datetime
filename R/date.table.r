
#' @title Get Date Table
#'
#' @description Given dates, it returns a data frame with peak and off-peak information.
#'
#' @param dates an array of date objects
#' @param col.names A list of columns included in the returned table.
#' Default returns all columns: c("Date","Month","Year", "DOW", "Peak", "nHours_HLH","nHours_LLH", "weight_HLH", "weight_LLH")
#'
#' @import timeDate
#'
#' @export GetDateTable
#'
#' @examples
#' dates <- seq(as.Date("2015-1-1"), length.out = 10, by=1)
#' GetDateTable(dates)
#'
#' # Calculate number of on-peak and off-peak hours
#' dates <- seq(as.Date("2015-1-1"),as.Date("2020-1-1"), by=1)
#' date.df <- GetDateTable(dates, c("Date", "nHours_HLH", "nHours_LLH"))
#' aggregate(date.df[,-1], by=list("Date"=FirstDayOfMonth(date.df$Date)), sum)
#'
#'

GetDateTable <- function(dates, col.names = c("Date","Month","Year", "DOW", "FDOM", "Peak", "nHours_HLH","nHours_LLH", "weight_HLH", "weight_LLH")){

  date.df <- data.frame("Date" = dates)
  date.df$Month <- MonthFromDate(date.df$Date)
  date.df$Year <- YearFromDate(date.df$Date)

  # Peak Days
  date.df$DOW<-weekdays(date.df$Date)
  tradeYear <- unique(YearFromDate(date.df$Date))
  date.df$DOW<-ifelse(date.df$Date %in% as.Date(timeDate::holidayNERC(tradeYear)@Data),"Holiday",date.df$DOW)
  date.df$FDOM <- FirstDayOfMonth(date.df$Date)
  date.df$Peak<-ifelse(date.df$DOW %in% c("Holiday","Sunday"),FALSE,TRUE)

  date.df$nHours_HLH <- ifelse(date.df$Peak, 16, 0)
  date.df$nHours_LLH <- 24-date.df$nHours_HLH

  date.df$weight_HLH <- date.df$nHours_HLH/24
  date.df$weight_LLH <- date.df$nHours_LLH/24

return(date.df[,col.names])
}




