#' Pie
#' Pie
#' @name gg_pie_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                           text = TRUE, type = 'count', color_text = "black",
                           leg_pos="right", aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = count, fill = a)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph + guides(text = FALSE)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Pie
#' Pie
#' @name gg_pie_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_Cat. <- function(data, titleLabel = "", subtitle = "", caption = "",
                        fillLabel = NULL, text = TRUE, type = 'count', color_text = "black", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), y = count, fill = a)) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab,  subtitle = subtitle, caption = caption) +
    guides(text = FALSE)
  graph <- graph  + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Pie facet
#' Facet Pie
#' @name gg_pie_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 leg_pos="right", text = TRUE, type = 'count', color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = cumsum(c) - c/2,
                  percent = 100 * round(c / total, 4))  %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = c,  fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    guides(text = FALSE)
  graph <- graph +
    theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~b)
  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Pie facet by second variable
#' Facet Pie
#' @name gg_pie_facet_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_facet_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, leg_pos = "right",
                                    aggregation = "sum", text = TRUE, type = 'count', color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = cumsum(c) - c/2,
                  percent = 100 * round(c / total, 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    theme(legend.position=leg_pos) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}


#' Bullseye
#' Bullseye
#' @name gg_bullseye_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                leg_pos="right", aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b)) %>% dplyr::arrange(count)
  graph <- ggplot(data = data_graph,
                  aes(x = factor(1), fill = a, y = reorder(a, count))) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}

#' Bullseye
#' Bullseye
#' @name gg_bullseye_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_Cat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                             leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)

  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  graph
}

#' Circular bar
#' Circular Bar
#' @name gg_bar_circular_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_Cat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                 leg_pos="right", width = 0.85, text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = count*9.7/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Bullseye facet
#' Facet Bullseye
#' @name gg_bullseye_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    guides(text = FALSE)
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~b)

  graph
}
#' Bullseye facet
#' Facet Bullseye
#' @name gg_bullseye_facet_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_facet_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                         fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = 1) +
    coord_polar(theta = "x")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_wrap(~b)

  graph
}






