
#' @title Format Date-Time
#'
#' @description Formatting function for date-time data.
#'
#' @param date Date in either 'character' or 'date' format.
#' @param datetime date-time in 'POSIXct' format.
#' @param HE Integer (1:24) indicating hour-ending.
#'
#' @export as.Date0
#' @export MonthFromDate
#' @export FirstDayOfMonth
#' @export YearFromDate
#' @export DateAsDateTime
#' @export DOW
#' @export HourFromDateTime
#' @export DateFromDateTime
#' @export DateAndHEAsPOSIX
#'
#' @details  DateFromDateTime is different from simply using as.Date(date-time), because date-time is stored in
#' standard time with time zone in R.
#'
#' @examples
#' as.Date0("1/1/2000")
#'
#' dates <- seq(as.Date("2000-1-1"), length.out = 6, by="mon")+15
#' dates
#' MonthFromDate(dates)
#' FirstDayOfMonth(dates)
#' YearFromDate(dates)
#' DOW(dates)
#' DateAndHEAsPOSIX(dates, c(1:10))
#'
#' datetimes <- DateAsDateTime(dates)+8*60*60
#' datetimes
#' HourFromDateTime(datetimes)
#' DateFromDateTime(datetimes)
#'

# Dates from a standard date format.
as.Date0 <- function(date){as.Date(date, "%m/%d/%Y")}

#' @describeIn as.Date0 Given a date, rerurn the month as numeric
MonthFromDate <- function(date){as.numeric(format(date, "%m"))}

#' @describeIn as.Date0 Given a date, returns the first day of the month
FirstDayOfMonth <- function(date){as.Date(format(date, "%Y-%m-01"))}

#' @describeIn as.Date0  Given a date, rerurn the year as numeric
YearFromDate <- function(date){as.numeric(format(date, "%Y"))}

#' @describeIn as.Date0 Given a date, returns 12:00am of the date in POSIXct format
DateAsDateTime <- function(date){as.POSIXct(strptime(paste(date, "0:0"), "%Y-%m-%d %H:%M"))}

#' @describeIn as.Date0 Day of week from a date
DOW <- function(date) format(as.Date(date), "%A")

#' @describeIn as.Date0 Given a date-time, return the hour as numeric
HourFromDateTime <- function(datetime){as.numeric(format(datetime, "%H"))}

#' @describeIn as.Date0 Given a date-time, returns a date.
DateFromDateTime <- function(datetime){as.Date(format(datetime, "%Y-%m-%d"))}

#' @describeIn as.Date0 Given a date and HE, returns the date-time for the beginning of the hour.
DateAndHEAsPOSIX <- function(date, HE){as.POSIXct(strptime(paste(date, (HE-1)), "%Y-%m-%d %H"))}








