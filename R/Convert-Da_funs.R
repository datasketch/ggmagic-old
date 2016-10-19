library(lubridate)
library(zoo)

#' gg_conv_Da2Ca_weekdays. : title.
#' Convert Data type to Category weekdays
#' @name gg_conv_Da2Ca_weekdays.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_conv_Da2Ca_weekdays. <- function(df){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(df$a)
  return(weekdays)
}

#' gg_conv_Da2Ca_months. : title.
#' Convert Data type to Category months
#' @name gg_conv_Da2Ca_months.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_conv_Da2Ca_months. <- function(df){
  f <- fringe(data)
  data <- f$d
  months <- months(df$a)
  return(months)
}

#' gg_conv_Da2Ca_years. : title.
#' Convert Data type to Category years
#' @name gg_conv_Da2Ca_years.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_conv_Da2Ca_years. <- function(df){
  f <- fringe(data)
  data <- f$d
  years <- year(df$a)
  return(years)
}

#' gg_conv_Da2Ca_qtr. : title.
#' Convert Data type to Category quaters
#' @name gg_conv_Da2Ca_qtr.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_conv_Da2Ca_qtr. <- function(df){
  f <- fringe(data)
  data <- f$d
  year_qtr <- as.yearqtr(df$a)
  return(year_qtr)
}

#' gg_conv_Da2Ca_weeks. : title.
#' Convert Data type to Category weeks
#' @name gg_conv_Da2Ca_weeks.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_conv_Da2Ca_weeks. <- function(df){
  f <- fringe(data)
  data <- f$d
  weeks <- as.numeric(format(df$a+3, "%U"))
  return(weeks)
}

#' gg_conv_Da2Ca_weekend. : title.
#' Convert Data type to Category weekend
#' @name gg_conv_Da2Ca_weekend.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_conv_Da2Ca_weekend. <- function(df){
  f <- fringe(data)
  data <- f$d
  weekdays <- weekdays(df$a)
  weekend <- ifelse(weekdays == "Saturday"|weekdays == "Sunday", 1, 0)
  return(weekend)
}

