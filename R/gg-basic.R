# funciones para graficos simples

gg_basic_bar <- function(data, x_col, y_col, fill = NULL, opts) {

  if (opts$bar_graph_type == "stacked") {
    data <- gg_stacked_prep_data(data = data, x_col = x_col, y_col = y_col)
  }


  if (is.null(fill)) {
    gg <- ggplot(data = data,
                 mapping = aes(x = .data[[x_col]], y = .data[[y_col]], fill = ..colors) )+
      geom_bar(stat = "identity")

  } else {
    gg <- ggplot(data,
                 mapping = aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[fill]]))
    gg <- switch (opts$bar_graph_type,
                  grouped = gg  +
                    geom_col(position = "dodge"),
                  stacked = gg +
                    geom_bar(stat = "identity", position = "stack")
    )

  }

  if (opts$bar_orientation == "hor") {
    gg <- gg + coord_flip()
  }

  gg

}



gg_basic_pie <- function(data, x_col, y_col) {
  data <- gg_pie_prep_data(data = data, x_col = x_col, y_col = y_col)
  ggplot(data,
         mapping = aes(x = "", y = prop, fill = .data[[x_col]])) +
    geom_bar(stat="identity") +
    coord_polar("y", start = 0)
}

gg_basic_donut <- function(data, x_col, y_col) {
  data <- gg_donut_prep_data(data = data, y_col = y_col)
  ggplot(data,
         mapping = aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = .data[[x_col]])) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(2, 4))
}



gg_basic_lines <- function(data, x_col, y_col, fill = NULL, opts) {


  if (is.null(fill)) {
    gg <- ggplot(data = data,
                 mapping = aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_line(color = unique(data$..colors))

  } else {
    gg <-  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[fill]], fill = .data[[fill]])) +
      geom_line()
  }


  gg

}


gg_basic_treemap <- function(data, x_col, y_col, fill = NULL, opts) {
  if (is.null(fill)) {
    gg <- ggplot(data = data,
                 mapping = aes(fill = .data[[x_col]], area = .data[[y_col]])) +
      treemapify::geom_treemap()
  } else {
    gg <-  ggplot(data, aes(fill = .data[[x_col]], area = .data[[y_col]], subgroup = .data[[fill]])) +
      treemapify::geom_treemap()
  }
  gg
}


gg_basic_scatter <- function(data, x_col, y_col, fill = NULL, opts) {

  if (is.null(fill)) {
    gg <- ggplot(data = data,
                 mapping = aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point()
  } else {
    gg <-  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[fill]], fill = .data[[fill]])) +
      geom_point()
  }
  gg
}


