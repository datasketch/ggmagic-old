test_that("Basic plots (first layer)", {

  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:10)

  opts <- list(bar_orientation = "hor", bar_graph_type = "basic")
  bar_plot <- gg_basic_bar(data = data, x_col = "city", y_col = "Mean", opts = opts)
  expect_true(inherits(bar_plot, "ggplot"))
  expect_true("GeomBar" %in% sapply(bar_plot$layers, function(x) class(x$geom)[1]))

  pie_plot <- gg_basic_pie(data = data, x_col = "city", y_col = "Mean")
  expect_true(inherits(pie_plot, "ggplot"))
  expect_true(inherits(pie_plot$coordinates, "CoordPolar"))

  donut_plot <- gg_basic_donut(data = data, x_col = "city", y_col = "Mean")
  expect_true(inherits(donut_plot, "ggplot"))
  expect_true("GeomRect" %in% sapply(donut_plot$layers, function(x) class(x$geom)[1]))


  data <- ggplot2::diamonds |>
    group_by(cut, color) |>
    summarise(Mean = mean(x))
  opts <- list(bar_orientation = "ver",
               bar_graph_type = "grouped")
  bar_plot <- gg_basic_bar(data = data, x_col = "cut", y_col = "Mean", fill = "color", opts = opts)
  expect_true(inherits(bar_plot, "ggplot"))
  expect_true("GeomCol" %in% sapply(bar_plot$layers, function(x) class(x$geom)[1]))
  expect_true("StatIdentity" %in% sapply(bar_plot$layers, function(x) class(x$stat)[1]))

  opts <- modifyList(opts, list(bar_graph_type = "stacked"))
  bar_plot <- gg_basic_bar(data = data, x_col = "cut", y_col = "Mean", fill = "color", opts = opts)
  expect_true(inherits(bar_plot, "ggplot"))
  expect_true("GeomBar" %in% sapply(bar_plot$layers, function(x) class(x$geom)[1]))
  #expect_true("PositionStack" %in% sapply(bar_plot$layers, function(x) class(x$positon)[1]))

  data <- ggplot2::economics |>
    group_by(date) |>
    summarise(Total = sum(pop))
  line_plot <- gg_basic_lines(data = data, x_col = "date", y_col = "Total", opts = NULL)

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> group_by(team, date) |> summarise(total = sum(x, na.rm = T))
  line_plot <- gg_basic_lines(data = data, x_col = "date", y_col = "total", fill = "team", opts = NULL)
  line_plot


  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:10)

  treemap_plot <- gg_basic_treemap(data = data, x_col = "city", y_col = "Mean", opts = NULL)


  data <- ggplot2::diamonds |>
    group_by(cut, color) |>
    summarise(Mean = mean(x))
  treemap_plot <- gg_basic_treemap(data = data, x_col = "cut", y_col = "Mean", fill = "color", opts = NULL)


  data <- lubridate::lakers |> tidyr::drop_na(x, y)
  scatter_plot <- gg_basic_scatter(data = data, x_col = "x", y_col = "y")
  scatter_plot
  data <- lubridate::lakers |> tidyr::drop_na(x, y)
  scatter_plot <- gg_basic_scatter(data = data, x_col = "x", y_col = "y", fill = "game_type")

})
