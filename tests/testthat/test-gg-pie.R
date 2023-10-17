test_that("Pie plot", {
  gg_pie(data = iris, dic = NULL, vars = c("species", "petal_width"))

  data <- lubridate::lakers
  gg_pie(data = data, dic = NULL, vars = c("team"), agg = "count")

})
