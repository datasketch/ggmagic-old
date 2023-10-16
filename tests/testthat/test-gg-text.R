test_that("Add text in ggplot chart", {
  opts_text <- list(
    datalabel_show = TRUE,
    datalabel_inside = TRUE,
    bar_orientation = "hor",
    bar_graph_type = "basic"
  )
  opts <- list(bar_orientation = "hor")
  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:10)
  data$..label <- data$Mean
  bar_plot <- gg_basic_bar(data = data, x_col = "city", y_col = "Mean", opts = opts) |>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot

  data <- data.frame(
    group=LETTERS[1:5],
    value=c(13,7,9,21,2)
  )
  data$..label <- data$value
  bar_plot <- gg_basic_bar(data = data, x_col = "group", y_col = "value", opts = opts) |>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot


  opts_text <- list(
    datalabel_show = TRUE,
    datalabel_inside = TRUE,
    bar_orientation = "ver",
    bar_graph_type = "basic"
  )
  opts <- list(bar_orientation = "ver")
  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:10)
  data$..label <- data$Mean
  bar_plot <- gg_basic_bar(data = data, x_col = "city", y_col = "Mean", opts = opts) |>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot


  opts_text <- list(
    datalabel_show = TRUE,
    datalabel_inside = F,
    bar_orientation = "hor",
    bar_type = "basic",
    bar_graph_type = "grouped"
  )
  data <- ggplot2::diamonds |>
    group_by(cut, color) |>
    summarise(Mean = mean(x))
  data$..label <- data$Mean
  opts <- list(bar_orientation = "hor",
               bar_graph_type = "grouped")
  bar_plot <- gg_basic_bar(data = data, x_col = "cut", y_col = "Mean", fill = "color", opts = opts)|>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot

  data <- data.frame(
    group = c("A", "B", "C", "B", "A"),
    color = c("yellow", "yellow", "orange", "orange", "orange"),
    value = c(13,7,9,21,2)
  )
  data$..label <- data$value
  opts_text <- list(
    datalabel_show = TRUE,
    datalabel_inside = F,
    bar_orientation = "hor",
    bar_type = "basic",
    bar_graph_type = "grouped"
  )
  opts <- list(bar_orientation = "hor",
               bar_graph_type = "grouped")

  bar_plot <- gg_basic_bar(data = data, x_col = "group", y_col = "value", fill = "color", opts = opts)|>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot

  opts_text <- list(
    datalabel_show = TRUE,
    datalabel_inside = F,
    bar_orientation = "ver",
    bar_type = "basic",
    bar_graph_type = "grouped"
  )
  opts <- list(bar_orientation = "ver",
               bar_graph_type = "grouped")

  bar_plot <- gg_basic_bar(data = data, x_col = "group", y_col = "value", fill = "color", opts = opts)|>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot


  opts_text <- list(
    datalabel_show = TRUE,
    datalabel_inside = F,
    bar_orientation = "ver",
    bar_type = "basic",
    bar_graph_type = "stacked"
  )
  opts <- list(bar_orientation = "ver",
               bar_graph_type = "stacked")

  bar_plot <- gg_basic_bar(data = data, x_col = "group", y_col = "value", fill = "color", opts = opts) |>
    gg_add_text(viz = "bar", opts = opts_text)
  bar_plot

  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:5)
  data$..label <- data$Mean
  pie_plot <- gg_basic_pie(data = data, x_col = "city", y_col = "Mean") |>
    gg_add_text(viz = "pie", opts = opts_text)
  pie_plot

  data <- data.frame(
    group = LETTERS[1:5],
    value = c(13,7,9,21,2)
  )
  data$..label <- data$value
  pie_plot <- gg_basic_pie(data = data, x_col = "group", y_col = "value") |>
    gg_add_text(viz = "pie", opts = opts_text)
  pie_plot

  donut_plot <- gg_basic_donut(data = data, x_col = "group", y_col = "value") |>
    gg_add_text(viz = "donut", opts = opts_text)
  donut_plot

  data <- txhousing |>
    group_by(city) |>
    summarise(Mean = mean(sales)) |>
    arrange(-Mean) |>
    slice(1:10)
  data$..label <- data$city
  opts_text <- list(datalabel_show = TRUE)
  treemap_plot <- gg_basic_treemap(data = data, x_col = "Mean", y_col = "Mean", opts = NULL) |>
    gg_add_text(viz = "treemap", opts = opts_text)



})
