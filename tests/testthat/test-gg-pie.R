test_that("Pie plot", {
  gg_pie(data = iris, dic = NULL, vars = c("species", "petal_width"))
})
