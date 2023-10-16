test_that("Lines plot", {
  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> group_by(team, date) |> summarise(total = sum(x, na.rm = T))
  gg_line(data = data, vars = c("team", "date", "total"), opts = NULL)


})
