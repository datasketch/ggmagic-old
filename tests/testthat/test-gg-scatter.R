test_that("Scatter plot", {
  data <- lubridate::lakers |> tidyr::drop_na(x, y)
  gg_scatter(data = data, dic = NULL, vars = c("x", "y"))
  gg_scatter(data = data, dic = NULL, vars = c("x", "y", "team"), color_by = "team")
})
