
#' @title Number of Days in a Month
#'
#' @description Given a date, it returns the number of days in the month.
#'
#' @param date Date in either 'character' or 'date' format.
#' @param month.idx An integer (1:12) indicating month index.
#' @param short A binary. If TRUE, month names are shown in abbreviated forms.
#'
#' @export NumDaysInMonth
#' @export MonthName
#' @export NumHoursInMonth
#'
#' @examples
#' dates <- seq(as.Date("2000-1-1"), length.out = 6, by="mon")
#' NumDaysInMonth(dates)
#' MonthName(MonthFromDate(dates))
#' NumHoursInMonth(dates)


NumDaysInMonth <- function(date){
  nDays <- ifelse(MonthFromDate(date) %in% c(4,6,9,11), 30, 31)
  nDays <- ifelse(MonthFromDate(date) ==2, 28, nDays )
  nDays <- ifelse(YearFromDate(date)%%4 == 0 & MonthFromDate(date) ==2, 29, nDays )
  return(nDays)
}

#' @describeIn NumDaysInMonth Given month index, returns the month name.
MonthName <- function(month.idx, short = FALSE){

  names.full <- c("January", "February", "March", "April", "May","June",
                  "July", "August","September","October","November","December")
  names.abb <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul","Aug","Sep", "Oct","Nov","Dec")
  if (short){MonthName <- names.abb[month.idx]} else {MonthName <- names.full[month.idx]}
  return(MonthName)
}

#' @describeIn NumDaysInMonth Given series of date (first days of month), it returns a data frame containing number of peak and off-peak hours. 
NumHoursInMonth <- function(Date){
  end.date <- tail(Date,1)
  dates <- seq(Date[1], end.date + NumDaysInMonth(end.date)-1, by="day")
  date.df <- GetDateTable(dates, c("Date", "pk.nHours", "opk.nHours"))
  date.df <- aggregate(date.df[,-1], by=list("Date"=FirstDayOfMonth(date.df$Date)), sum)
  date.df <- date.df[date.df$Date %in% Date,]
  return(date.df)
}





