#' Horizontal 100% stacked bar by first variable + facet by second variable
#' Stacked
#' @name gg_bar_stacked_100_hor_CaCaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCaYeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                             aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% filter(d >= 0)

  data_graph <- data %>% dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    dplyr::group_by(b, c) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / total, 4))

  data <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    tidyr::spread(c, count) %>%
    tidyr::gather(c,count,c(-a,-b) ) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c", "count")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data_graph,aes(x = c, y = count,fill = a)) +
    geom_bar(position = "fill",stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    theme_ds() +
    facet_wrap(~b) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    #facet_wrap(~b) +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
    theme(legend.position = leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = percent/100, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Vertical 100% stacked bar by first variable + facet by second variable
#' Stacked
#' @name gg_bar_stacked_100_ver_CaCaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCaYeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                             aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% filter(d >= 0)

  data_graph <- data %>% dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    dplyr::group_by(b, c) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / total, 4))

  data <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    tidyr::spread(c, count) %>%
    tidyr::gather(c,count,c(-a,-b) ) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c", "count")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data_graph,aes(x = c, y = count,fill = a)) +
    geom_bar(position = "fill",stat = "identity") +
    scale_y_continuous(labels = percent) +
    theme_ds() +
    facet_wrap(~b) +
    #facet_wrap(~b) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
    theme(legend.position = leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = percent/100, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Vertical stacked bar by first variable + facet by second variable
#' Stacked
#' @name gg_bar_stacked_ver_CaCaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCaYeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                         fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                         aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% filter(d >= 0)

  data_graph <- data %>% dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    dplyr::group_by(b, c) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / total, 4))

  data <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = agg(aggregation, d)) %>%
    tidyr::spread(c, count) %>%
    tidyr::gather(c,count,c(-a,-b) ) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c", "count")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data_graph,aes(x = c, y = count,fill = a)) +
    geom_bar(position = "stack",stat = "identity") +
    theme_ds() +
    facet_wrap(~b) +
    #facet_wrap(~b) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
    theme(legend.position = leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal stacked bar by first variable + facet by second variable
#' Stacked
#' @name gg_bar_stacked_hor_CaCaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCaYeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                         fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                         aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0, ...){

  graph <- gg_bar_stacked_ver_CaCaYeNu.(data, titleLabel, subtitle, caption, fillLabel, xLabel,
                                        yLabel, leg_pos, aggregation, text, type, color_text, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}
