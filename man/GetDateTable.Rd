% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date.table.r
\name{GetDateTable}
\alias{GetDateTable}
\title{Get Date Table}
\usage{
GetDateTable(dates, market = "PNW", col.names = c("Date", "Month", "Year",
  "DOW", "FDOM", "Peak", "nHours_HLH", "nHours_LLH", "weight_HLH",
  "weight_LLH"))
}
\arguments{
\item{dates}{an array of date objects}

\item{market}{"PNW", "CAISO" or "MISO"}

\item{col.names}{A list of columns included in the returned table.
Default returns all columns: c("Date","Month","Year", "DOW", "Peak", "nHours_HLH","nHours_LLH", "weight_HLH", "weight_LLH")}
}
\description{
Given dates, it returns a data frame with peak and off-peak information.
}
\examples{
dates <- seq(as.Date("2015-1-1"), length.out = 10, by=1)
GetDateTable(dates, market = "CAISO")

# Calculate number of on-peak and off-peak hours
dates <- seq(as.Date("2015-1-1"),as.Date("2020-1-1"), by=1)
date.df <- GetDateTable(dates, "MISO",c("Date", "nHours_HLH", "nHours_LLH"))
aggregate(date.df[,-1], by=list("Date"=FirstDayOfMonth(date.df$Date)), sum)


}

