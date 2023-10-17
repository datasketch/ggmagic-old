test_that("Donut plot", {
  gg_donut(data = iris, dic = NULL, vars = c("species", "petal_width"))
  gg_donut(data = iris, dic = NULL, vars = c("species"), agg = "count")
})
