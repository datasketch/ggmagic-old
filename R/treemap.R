#' Treemap coloured by first variable
#' Treemap fill by first Cat
#' @name gg_treemap_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_Cat. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            text = TRUE, color_text = "black", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"),
                       group.label.colour = color_text, group.label.size = 20, group.label.min.size = 10)

  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"), group.labels = FALSE)
  }

  graph <- graph + guides(fill=FALSE) +
    scale_fill_manual(values = getPalette()) + labs(title = titleLabel, subtitle = subtitle, caption =  caption)

  graph
}

#' Treemap coloured by first variable
#' Treemap fill by first Cat
#' @name gg_treemap_x_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                 text = TRUE, color_text = "black", aggregation = "sum", ...){

  f <- fringe(data)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"),
                       group.label.colour = color_text, group.label.size = 30, group.label.min.size = 10)

  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"), group.labels = FALSE)
  }

  graph <- graph + guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette())

  graph
}

#' Treemap density by first numeric variable
#' Treemap Density by Num
#' @name gg_treemap_density_y_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CatNum. <- function(data, titleLabel = "", subtitle = "",
                                         caption = "", fillLabel = NULL, reverse = FALSE,
                                         text = TRUE, color_text = "black", aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a"),
                       group.label.colour = color_text, group.label.size = 30, group.label.min.size = 10)

  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a"), group.labels = FALSE)
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) +
    guides(fill = guide_legend(clab))

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph
}


#' Treemap coloured by first variable
#' Treemap fill first Cat
#' @name gg_treemap_x_CatCatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CatCatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                       aggregation = "sum", text = "TRUE", color_text = "black",
                                       leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
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
#' Treemap fill second Cat
#' @name gg_treemap_y_CatCatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CatCatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                       aggregation = "sum", text = "TRUE", color_text = "black",
                                       leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
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
#' Treemap fill third Cat
#' @name gg_treemap_z_CatCatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_z_CatCatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                       aggregation = "sum", text = "TRUE", color_text = "black",
                                       leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
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
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    guides(fill = guide_legend(clab)) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}


#' Treemap coloured by first variable
#' Treemap fill first Cat
#' @name gg_treemap_x_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 text = "TRUE", color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                  label = "b"), group.labels = FALSE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) +
    scale_fill_manual(values = getPalette()) +
    guides(fill = guide_legend(clab)) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by second variable
#' Treemap fill second Cat
#' @name gg_treemap_y_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 text = TRUE, color_text = "black",
                                 leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 0, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) +
    scale_fill_manual(values = getPalette()) +
    guides(fill = guide_legend(clab)) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by first variable
#' Treemap Fill by first Cat
#' @name gg_treemap_x_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                    label_size = 5, aggregation = "sum", text = "TRUE", color_text = "black",
                                    leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c))


  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                  label = "b"), group.labels = FALSE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + guides(fill = guide_legend(clab)) + theme(legend.position = leg_pos)

  graph
}

#' Treemap coloured by second variable
#' Treemap Fill by second Cat
#' @name gg_treemap_y_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                    aggregation = "sum", text = TRUE, color_text = "black",
                                    leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "a"), group.labels = FALSE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b', group = "a",
                                  label = "a"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + guides(fill = guide_legend(clab)) + theme(legend.position = leg_pos)

  graph
}

#' Treemap density by numeric variable
#' Treemap Density by Num
#' @name gg_treemap_density_z_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_z_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", reverse = FALSE,
                                            fillLabel = NULL, text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  # graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count',
  #                               group = "a", label = "b"), group.label.colour = "white",
  #                    label.colour = "white", label.size.factor = 2,
  #                    group.label.size.threshold = 1) +

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + theme_ds() + theme_ds_clean() +
    theme(legend.position=leg_pos) + guides(fill = guide_legend(clab))

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph
}


#' Treemap density first numeric variable
#' Treemap Fill by second Num
#' @name gg_treemap_density_x_CatCatNumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_x_CatCatNumNum. <- function(data, titleLabel = "",  subtitle = "", caption = "",
                                               fillLabel = NULL, reverse = FALSE,
                                               text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + theme_ds() + theme_ds_clean() +
    guides(fill = guide_legend(clab)) +
    theme(legend.position=leg_pos)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}

#' Treemap density second numeric variable
#' Treemap Density by second Num
#' @name gg_treemap_density_y_CatCatNumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CatCatNumNum. <- function(data, titleLabel = "",  subtitle = "", caption = "",
                                               fillLabel = NULL,reverse = FALSE,
                                               text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% paste(aggregation, nms[4], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + theme_ds() + theme_ds_clean() +
    guides(fill = guide_legend(clab)) +
    theme(legend.position=leg_pos)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}

#' Treemap density first numeric variable
#' Treemap Fill by second Num
#' @name gg_treemap_density_x_CatCatNumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_x_CatCatNumNum. <- function(data, titleLabel = "",  subtitle = "", caption = "",
                                               fillLabel = NULL, reverse = FALSE,
                                               text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + theme_ds() + theme_ds_clean() +
    guides(fill = guide_legend(clab)) +
    theme(legend.position=leg_pos)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}

#' Treemap density second numeric variable
#' Treemap Density by second Num
#' @name gg_treemap_density_y_CatCatNumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_density_y_CatCatNumNum. <- function(data, titleLabel = "",  subtitle = "", caption = "",
                                               fillLabel = NULL,reverse = FALSE,
                                               text = TRUE, color_text = "black", aggregation = "sum", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% paste(aggregation, nms[4], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)


  if(text == TRUE){

    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = TRUE, group.label.colour = color_text, group.label.size = 20,
                       group.label.min.size = 15, label.colour = color_text, label.size = 10, label.min.size = 5)  #guides(fill = FALSE) +
  }else{
    graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'count', group = "a",
                                  label = "b"), group.labels = FALSE, label.size = 0)  #guides(fill = FALSE) +
  }

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab) + theme_ds() + theme_ds_clean() +
    guides(fill = guide_legend(clab)) +
    theme(legend.position=leg_pos)


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  graph
}

#' Treemap coloured by first variable
#' Treemap fill first Cat
#' @name gg_treemap_x_CatCatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CatCatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
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
#' Treemap fill second Cat
#' @name gg_treemap_y_CatCatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CatCatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
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
#' Treemap fill third Cat
#' @name gg_treemap_z_CatCatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_z_CatCatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
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

