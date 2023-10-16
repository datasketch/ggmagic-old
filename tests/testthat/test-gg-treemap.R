test_that("Treemap plot", {
  gg_treemap(data = iris, dic = NULL, vars = c("species", "petal_width"))
})
