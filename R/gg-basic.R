# funciones para graficos simples

gg_basic_bar <- function(data, x_col, y_col) {
  ggplot(data = data,
         mapping = aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_bar(stat = "identity")
}

gg_basic_pie <- function(data, x_col, y_col) {
  ggplot(data,
         mapping = aes(x = "", y = .data[[y_col]], fill = .data[[x_col]])) +
    geom_bar(stat="identity") +
    coord_polar("y", start=0)
}

gg_basic_donut <- function(data, x_col, y_col) {
  data <- gg_donut_prep_data(data = data, y_col = y_col)
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = .data[[x_col]])) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(2, 4))
}


