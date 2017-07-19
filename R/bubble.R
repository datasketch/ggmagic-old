#' Bubble
#' bubbles
#' @name gg_bubble_Cat2.
#' @param x A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_Cat2. <- function(data, titleLabel = "", subtitle = "", caption = "",  sep = 3, lim_inf =-40,
                            lim_sup = 40, fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data <- data %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  data$b <- rescale(data$b, to = c(5,15))
  ncircles <- dim(data)[1]
  limits <- c(lim_inf , lim_sup)
  inset <- diff(limits) / sep

  set.seed(7321)
  xyr <- data.frame(
    x = runif(ncircles, min(limits) + inset, max(limits) - inset),
    y = runif(ncircles, min(limits) + inset, max(limits) - inset),
    r = (data$b)) %>% arrange(desc(r))

  res <- circleLayout(xyr, limits, limits, maxiter = 1000)

  dat.after <- circlePlotData(res$layout, npoints = 1000)

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(id = 1:n(), categoria = a) %>%
    dplyr::select(id, categoria)

  fi <- data.frame(id = 1:dim(data)[1], categoria = data$a)
  fi <- inner_join(data_graph, dat.after)

  cent <- fi %>% dplyr::group_by(categoria) %>%
    dplyr::summarise(x = mean(x), y = mean(y))


  graph <- ggplot(fi) +
    geom_polygon(aes(x, y, group=id, fill = categoria)) +
    scale_fill_manual(values = getPalette()) +
    coord_equal(xlim=limits, ylim=limits) +
    geom_text(data=cent, aes(x, y, label=categoria)) +
    theme_ds() + theme_ds_clean() +
    labs(title=titleLabel, subtitle = subtitle, caption = caption) + guides(fill = FALSE)

  return(graph)

}

#' Bubble
#' Bubble
#' @name gg_bubble_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CatNum.  <- function(data, titleLabel = "", subtitle = "", caption = "",
                               xLabel = NULL, text = TRUE, type = 'count',
                               color_text = "black", leg_pos = "right", aggregation = "sum", angle_x = 0,
                               shape_type = 19,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count, color = ""))
  graph <- graph + geom_point(show.legend = FALSE, shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "") +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first variable
#' Coloured Bubble
#' @name gg_bubble_coloured_x_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CatNum.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          text = TRUE, type = 'count', color_text = "black", leg_pos = "right",
                                          aggregation = "sum", angle_x = 0, shape_type = 19,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = a), shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "")

  graph <- graph + scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first numeric variable
#' Coloured Bubble
#' @name gg_bubble_density_y_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_density_y_CatNum.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, fillLabel = NULL,
                                         text = TRUE, type = 'count', color_text = "black", leg_pos = "right",
                                         aggregation = "sum", angle_x = 0, shape_type = 19, reverse = FALSE, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = count), shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = "", color = clab) + guides(size = FALSE)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank()) +
    theme(legend.position=leg_pos)


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3)) + guides(text = FALSE, size = FALSE)
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3)) + guides(text = FALSE, size = FALSE)
    }else{
      return(graph + guides(text = FALSE, size = FALSE))
    }
  }
  return(graph + guides(text = FALSE, size = FALSE))
}


#' Bubbles
#' Bubbles
#' @name gg_bubble_CatNum2.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CatNum2. <- function(data, titleLabel = "", subtitle = "", caption = "",  sep = 3, lim_inf =-80,
                               lim_sup = 80, xLabel = NULL, aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data <- data %>% dplyr::group_by(a) %>% dplyr::summarise(b = agg(aggregation,b))
  data$b <- rescale(data$b, to = c(5, 30))
  ncircles <- dim(data)[1]
  limits <- c(lim_inf , lim_sup)
  inset <- diff(limits) / sep

  set.seed(7321)
  xyr <- data.frame(
    x = runif(ncircles, min(limits) + inset, max(limits) - inset),
    y = runif(ncircles, min(limits) + inset, max(limits) - inset),
    r = (data$b)) %>% arrange(desc(r))

  res <- circleLayout(xyr, limits, limits, maxiter = 1000)

  dat.after <- circlePlotData(res$layout)

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(id = 1:n(), categoria = a) %>%
    dplyr::select(id, categoria)

  fi <- data.frame(id = 1:dim(data)[1], categoria = data$a)
  fi <- inner_join(data_graph, dat.after)

  cent <- fi %>% dplyr::group_by(categoria) %>%
    dplyr::summarise(x = mean(x), y = mean(y))


  graph <- ggplot(fi) +
    geom_polygon(aes(x, y, group=id, fill = categoria)) +
    scale_fill_manual(values = getPalette()) +
    coord_equal(xlim=limits, ylim=limits) +
    geom_text(data=cent, aes(x, y, label=categoria)) +
    theme_ds() + theme_ds_clean() +
    labs(title=titleLabel) + guides(fill = FALSE)

  return(graph)

}

#' Bubbles
#' bubbles
#' @name gg_bubbles_DatNum.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubbles_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                               xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b, size = b) )+
    geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette()) +
    theme_ds() +
    theme(legend.position="none") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Bubbles density by first numeric variable
#' bubbles
#' @name gg_bubbles_density_x_DatNum.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubbles_density_x_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                         xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19,
                                         leg_pos = "right", reverse = FALSE, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b, size = b) )+
    geom_point(shape = shape_type, aes(color = b))

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph +
    theme_ds() + guides(size = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Bubble
#' Bubble
#' @name gg_bubble_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_Cat.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            shape_type = 19, angle_x = 0, text = TRUE, type = 'count',
                            color_text = "black",  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count, color = ""))
  graph <- graph + geom_point(show.legend = FALSE, shape = shape_type)
  graph <- graph + labs(title = titleLabel, x = xlab, y = "", subtitle = subtitle, caption = caption) +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured
#' Coloured Bubble
#' @name gg_bubble_coloured_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_Cat.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     fillLabel = NULL, shape_type = 19, angle_x = 0,  text = TRUE, type = 'count',
                                     color_text = "black",  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = a), show.legend = FALSE, shape = shape_type)
  graph <- graph + labs(title = titleLabel, x = xlab, y = "", fill = flab, subtitle = subtitle, caption = caption)

  graph <- graph + scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = 0, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -1.3))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = 0, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -1.3))
    }else{
      graph
    }
  }
  graph
}

#' Bubble
#' Bubble
#' @name gg_bubble_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CatCat.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, text = TRUE, color_text = "black", type = "count",
                               angle_x = 0, shape_type = 19,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count, color = "")) +
    geom_point(shape = shape_type)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_color_manual(values = getPalette()) +
    guides(size = FALSE, colour = FALSE)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first variable
#' Coloured Bubble
#' @name gg_bubble_coloured_x_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CatCat.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, text = TRUE, color_text = "black", type = "count",
                                          angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / total, 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count)) +
    geom_point(aes(color = a), shape = shape_type)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_color_manual(values = getPalette()) +
    guides(colour = FALSE, size = FALSE)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by second variable
#' Coloured Bubble
#' @name gg_bubble_coloured_y_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_y_CatCat.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, text = TRUE, color_text = "black", type = "count",
                                          angle_x = 0,  shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / total, 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count)) +
    geom_point(aes(color = b), shape = shape_type)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_color_manual(values = getPalette()) +
    guides(colour = FALSE, size = FALSE)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble
#' Bubble
#' @name gg_bubble_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CatCatNum.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, aggregation = "sum", angle_x = 0,
                                  text = TRUE, color_text = "black", type = "count",
                                  shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = count))
  graph <- graph + geom_point(aes(colour = ""), shape = shape_type)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph  + theme(legend.position="none") +
    theme_ds() + scale_color_manual(values = getPalette()) +
    guides(size = FALSE, colour = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by first variable
#' Coloured Bubble first Cat
#' @name gg_bubble_coloured_x_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_x_CatCatNum.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, aggregation = "sum", angle_x = 0,
                                             text = TRUE, color_text = "black", type = "count",
                                             shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(suma = agg(aggregation, c)) %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(total = sum(suma)) %>%
    dplyr::mutate(pos = suma*9/10, percent = 100 * round(suma/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  suma = ifelse(suma == 0, NA, suma))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = suma))
  graph <- graph + geom_point(aes(color = a), shape = shape_type) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

#' Bubble coloured by second variable
#' Coloured Bubble second Cat
#' @name gg_bubble_coloured_y_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_y_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL,  aggregation = "sum", angle_x = 0,
                                            text = TRUE, color_text = "black", type = "count",
                                            shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(suma = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(suma)) %>%
    dplyr::mutate(pos = suma*9/10, percent = 100 * round(suma/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  suma = ifelse(suma == 0, NA, suma))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = suma)) +
    geom_point(aes(color = b), shape = shape_type) +
    scale_color_manual(values = getPalette())
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    theme_ds() +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = b, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text, vjust = -0.5))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = b, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, vjust = -0.5))
    }else{
      graph
    }
  }
  graph
}

