test_that("Bar plot", {


  gg_bar(data = iris, dic = NULL, vars = c("species", "petal_width"), bar_graph_type = "basic")
  gg_bar(data = iris, dic = NULL, vars = c("species"), bar_graph_type = "basic", agg = "count")

  data <- txhousing |> filter(city %in% c("Dallas", "Waco"))
  gg_bar(data = data, dic = NULL, vars = c("city", "year", "volume"), color_by = "city", bar_orientation = "hor")
  gg_bar(data = data, dic = NULL, vars = c("city", "year", "volume"), color_by = "city", bar_graph_type = "stacked")


  gg_bar_Cat(iris)
  gg_bar_CatNum(iris)

  data <- txhousing |> filter(city %in% c("Dallas", "Waco")) |> select(city, year, volume)
  data$year <- as.character(data$year)
  dic <- data.frame(id = names(data), label = names(data), hdtype = c("Cat", "Cat", "Num"))
  gg_bar_CatCatNum(data, dic = dic)
  gg_bar_CatYeaNum(txhousing)

})
