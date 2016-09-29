library(lubridate)
library(zoo)

conv_Da2Ca_weekdays <- function(df){
  weekdays <- weekdays(df$a)
  return(weekdays)
}

conv_Da2Ca_months <- function(df){
  months <- months(df$a)
  return(months)
}

conv_Da2Ca_years <- function(df){
  years <- year(df$a)
  return(years)
}

conv_Da2Ca_qtr <- function(df){
  year_qtr <- as.yearqtr(df$a)
  return(year_qtr)
}

conv_Da2Ca_weeks <- function(df){
  weeks <- as.numeric(format(df$a+3, "%U"))
  return(weeks)
}


