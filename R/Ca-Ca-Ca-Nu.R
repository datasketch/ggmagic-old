
#' gg_treemap_x_CaCaCaNu.
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'a', group = "b", label = 'c'),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = flabel)

  graph
}

#' gg_treemap_y_CaCaCaNu.
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'b', group = "a", label = "c"),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = flabel)

  graph
}

#' gg_treemap_z_CaCaCaNu.
#' Treemap fill third Ca
#' @name gg_treemap_z_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_z_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'c', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption)

  graph
}


#' gg_bar_stacked_100_hor_CaCaCaNu.
#' Stacked
#' @name gg_bar_stacked_100_hor_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu, Ca-Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)


gg_bar_stacked_100_hor_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, xlab = NULL, ylab = NULL,leg_pos = 'right', ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$d <- abs(data$d)

  data_graph <- data %>%
    #tidyr::drop_na(a,c) %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = sum(d, na.rm = TRUE)) %>%
    tidyr::spread(c, count) %>%
    tidyr::gather(c,count,c(-a,-b) )

  data_graph[is.na(data_graph)] <- 0


  graph <- ggplot(data_graph,aes(x = c, y = count,fill = a)) +
    geom_bar(position = "fill",stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    theme_ds() +
    facet_grid(b ~ .) +
    #facet_grid(b ~ ., switch = 'y') +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(legend.position = leg_pos)

  graph
}
