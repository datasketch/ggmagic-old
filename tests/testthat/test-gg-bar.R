test_that("Bar plot", {
  gg_bar(data = iris, dic = NULL, vars = c("species", "petal_width"))

  gg_bar(data = txhousing, dic = NULL, vars = c("city", "year", "volume"))

})
