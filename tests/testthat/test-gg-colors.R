test_that("Colors in plot", {
  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:10)
  data$..colors <- "#385573"
  opts <- list(bar_orientation = "hor", bar_graph_type = "basic")
  opts_color <- list(color_palette_type = "categorical", color_palette_categorical = "#385573")
  bar_plot <- gg_basic_bar(data = data, x_col = "city", y_col = "Mean", opts = opts) |>
     gg_color(opts = opts_color, data = data, viz = "bar")

  bar_plot

})
