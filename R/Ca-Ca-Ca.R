
#' Treemap coloured by first variable
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 text = "TRUE", color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c))

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "b",
                                  label = "c"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 15,
                       group.label.min.size = 10, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "b",
                                  label = "c"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + scale_fill_manual(values = getPalette()) +
    guides(fill = guide_legend(clab)) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by second variable
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 text = "TRUE", color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c))

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "c"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 15,
                       group.label.min.size = 10, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "c"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + scale_fill_manual(values = getPalette()) +
    guides(fill = guide_legend(clab)) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by third variable
#' Treemap fill third Ca
#' @name gg_treemap_z_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_z_CaCaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 text = "TRUE", color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c))

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'c', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 15,
                       group.label.min.size = 10, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'c', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + scale_fill_manual(values = getPalette()) +
    guides(fill = guide_legend(clab)) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Scatter
#' Coloured Point
#' @name gg_point_CaCaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_CaCaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                             fillLabel = NULL, angle_x = 0, shape_type = 19, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  clab <- fillLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c))

  graph <- ggplot(data, aes(x = factor(a), y = factor(b), color = c)) +
    geom_point(shape = shape_type) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab) + theme_ds() +
    scale_x_discrete(labels = scales::comma) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}
