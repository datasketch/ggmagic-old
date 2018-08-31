###
#### BARRAS
###


#' Vertical bar coloured by first variable
#' vertical bar
#' @name gg_bar_coloured_x_ver_CatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_bar_coloured_x_ver_CatNum(sampleData("Cat-Num"))
gg_bar_coloured_x_ver_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, line_mean = FALSE, text = TRUE,
                                         type = 'count', leg_pos = "right", aggregation = "sum", angle_x = 0, color_text = "black", ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = factor(a))) +
    geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(line_mean){
    graph <- graph + geom_hline(aes(yintercept= mean(data_graph$count)), linetype="dashed")
  }else{
    graph <- graph
  }

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }


}



#' Vertical bar density by first numeric variable
#' vertical bar
#' @name gg_bar_density_y_ver_CatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_bar_density_y_ver_CatNum(sampleData("Cat-Num"))
gg_bar_density_y_ver_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                        fillLabel = NULL, reverse = FALSE, line_mean = FALSE,
                                        text = TRUE, type = 'count', color_text = "black",
                                        leg_pos = "right", aggregation = "sum", angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2], sep = " ")
  clab <- fillLabel %||% paste(aggregation, nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = count)) +
    geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(line_mean){
    graph <- graph + geom_hline(aes(yintercept= mean(data_graph$count)), linetype="dashed")
  }else{
    graph <- graph
  }

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}


# ------------------------------------------------------------------------------------------------
###
#### POLAR BAR
###

#' Polar bar
#' Polar Bar
#' @name gg_bar_polar_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_CatNum. <- function(data, width = 0.95, titleLabel = "", subtitle = "", aggregation = "sum",
                                 caption = "", fillLabel = NULL, leg_pos = "right", text = TRUE, color_text = "black",
                                 type = "count", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b)) %>%
    dplyr::arrange(count) %>%
    dplyr::mutate(pos = count*8/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = a, weight = count, fill = a)) + geom_bar(width = width) +
    coord_polar() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

# ------------------------------------------------------------------------------------------------
###
#### CIRCULAR BAR
###

#' Circular bar
#' Circular Bar
#' @name gg_bar_circular_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                    leg_pos="right", width = 0.85, aggregation = 'sum',  text = TRUE,
                                    type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation,b))  %>%
    dplyr::arrange(count) %>%
    dplyr::mutate(pos = count*9.7/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a,count), y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}


#' Vertical stacked bar
#' Single Vertical Stacked Bar
#' @name gg_bar_single_stacked_ver_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_bar_single_stacked_ver_CatNum(sampleData("Cat-Num"))
gg_bar_single_stacked_ver_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, leg_pos="right", width = 0.3, aggregation = 'sum',
                                             text = TRUE, type = "count", color_text = "black", ...){

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
    dplyr::mutate(pos = cumsum(count) - (count/2),
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(x = factor(1), fill = a, weight = count)) +
    geom_bar(width = width)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
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


#' Horizontal stacked bar
#' Single Horizontal Stacked Bar
#' @name gg_bar_single_stacked_hor_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_bar_single_stacked_hor_CatNum(sampleData("Cat-Num"))
gg_bar_single_stacked_hor_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, leg_pos="right", width = 0.3, aggregation = 'sum',
                                             text = TRUE, type = "count", color_text = "black", angle_x = 0, ...){

  graph <- gg_bar_single_stacked_ver_CatNum(data, titleLabel, subtitle, caption,  fillLabel, leg_pos, width, aggregation,
                                            text, type, color_text, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}


















#' Horizontal 100% stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_100_hor_CatCatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' gg_bar_stacked_100_hor_CatCatCatNum(sampleData("Cat-Cat-Cat-Num"))
gg_bar_stacked_100_hor_CatCatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                                aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                                angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste("%", aggregation, nms[4], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

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

  graph <- ggplot(data_graph,aes(x = b, y = count,fill = a)) +
    geom_bar(position = "fill",stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    theme_ds() +
    facet_wrap(~c) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    #facet_wrap(~b) +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
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

#' Vertical 100% stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_100_ver_CatCatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' gg_bar_stacked_100_ver_CatCatCatNum(sampleData("Cat-Cat-Cat-Num"))
gg_bar_stacked_100_ver_CatCatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                                aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                                angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste("%", aggregation, nms[4], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

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

  graph <- ggplot(data_graph,aes(x = b, y = count,fill = a)) +
    geom_bar(position = "fill",stat = "identity") +
    scale_y_continuous(labels = percent) +
    theme_ds() +
    facet_wrap(~c) +
    #facet_wrap(~b) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
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

#' Vertical stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_ver_CatCatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' gg_bar_stacked_ver_CatCatCatNum(sampleData("Cat-Cat-Cat-Num"))
gg_bar_stacked_ver_CatCatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                            aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                            angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[4], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

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

  graph <- ggplot(data_graph,aes(x = b, y = count,fill = a)) +
    geom_bar(position = "stack",stat = "identity") +
    theme_ds() +
    facet_wrap(~c) +
    #facet_wrap(~b) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
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

#' Horizontal stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_hor_CatCatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' gg_bar_stacked_hor_CatCatCatNum(sampleData("Cat-Cat-Cat-Num", nrow=100))
gg_bar_stacked_hor_CatCatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                            aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                            angle_x = 0, ...){

  graph <- gg_bar_stacked_ver_CatCatCatNum(data, titleLabel, subtitle, caption, fillLabel, xLabel,
                                           yLabel, leg_pos, aggregation, text, type, color_text, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical coloured bar
#' Vertical coloured bars
#' @name gg_bar_coloured_ver_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_coloured_ver_Cat(sampleData("Cat"))
gg_bar_coloured_ver_Cat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                    xLabel = NULL, yLabel = NULL,
                                    text = TRUE, type = 'count', color_text = "black",
                                    leg_pos = "right", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(y = count , x = a, fill = factor(a))) + geom_bar(stat = 'identity')

  graph <- graph +
    guides(fill = FALSE) +
    theme(legend.position = leg_pos) +
    theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, x= xlab, y = ylab, subtitle = subtitle, caption = caption)

  if (text == TRUE & type == 'count') {
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), color = color_text, check_overlap = TRUE))
  } else {
    if (text == TRUE & type == 'percent') {
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), color = color_text, check_overlap = TRUE))
    } else {
      graph
    }
  }
}


#' Horizontal coloured bar
#' Horizontal coloured Bars
#' @name gg_bar_coloured_hor_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_coloured_hor_Cat(sampleData("Cat"))
gg_bar_coloured_hor_Cat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                    xLabel = NULL, yLabel = NULL,
                                    text = TRUE, type = 'count', color_text = "black",
                                    leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_coloured_ver_Cat(data, titleLabel, subtitle, caption, xLabel,
                                   yLabel, text, type, color_text, leg_pos, angle_x, ...) +  coord_flip()
  return(graph)

}

#' Vertical bar highlighting some parameter
#' Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_coloured_parameter_ver_Cat(sampleData("Cat"))
gg_bar_coloured_parameter_ver_Cat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                              xLabel = NULL, yLabel = NULL, parameter = NULL,
                                              text = TRUE, type = 'count', color_text = "black",
                                              leg_pos = "right", angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  #p <-  parameter %||% sample(unique(data[,nms[1]]), 1)
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  p <- parameter %||% (data_graph %>% filter(count == max(count)))$a

  graph <- ggplot(data_graph, aes(x = a, y = count)) +
    geom_bar(stat="identity", aes(fill = a %in% p ))
  graph <- graph +
    labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph + guides(fill = FALSE) + theme(legend.position = leg_pos) +
    theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }

}

#' Horizontal bar highlighting some parameter
#' Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_coloured_parameter_hor_Cat(sampleData("Cat"))
gg_bar_coloured_parameter_hor_Cat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                              xLabel = NULL, yLabel = NULL, parameter = NULL,
                                              text = TRUE, type = 'count', color_text = "black",
                                              leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_coloured_parameter_ver_Cat(data, titleLabel, subtitle, caption, xLabel, yLabel,
                                             parameter, text, type, color_text, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical bar
#' Vertical bars
#' @name gg_bar_ver_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_ver_Cat(sampleData("Cat"))
gg_bar_ver_Cat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                           text = TRUE, type = 'count', color_text = "black",
                           leg_pos = "right", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph , aes(y = count ,x = factor(a), fill = "")) + geom_bar(stat ='identity') +
    labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph + theme(legend.position=leg_pos) + guides(fill = FALSE) +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Horizontal bar
#' Horizontal Bars
#' @name gg_bar_hor_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_hor_Cat(sampleData("Cat"))
gg_bar_hor_Cat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                           text = TRUE, type = 'count', color_text = "black",
                           leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_ver_Cat(data, titleLabel, subtitle, caption, xLabel, yLabel, text, type,
                          color_text, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()
  graph
}

#' Ordered vertical bar
#' Ordered vertical Bars
#' @name gg_bar_ordered_ver_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_ordered_ver_Cat(sampleData("Cat"))
gg_bar_ordered_ver_Cat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, text = TRUE, type = 'count', color_text = "black",
                                   leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4),
                  pos = count*9/10) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count, fill = "")) +
    geom_bar(stat = "identity")
  graph <- graph +
    labs(title = titleLabel, y = ylab, x = xlab, subtitle = subtitle, caption = caption) +
    theme(legend.position=leg_pos) +
    theme_ds() + scale_fill_manual(values = getPalette()) + guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = pos, label = round(count,2)), check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}

#' Ordered horizontal bar
#' Ordered horizontal Bars
#' @name gg_bar_ordered_hor_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_ordered_hor_Cat
gg_bar_ordered_hor_Cat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, text = TRUE, type = 'count', color_text = "black",
                                   leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_ordered_ver_Cat(data, titleLabel, subtitle, caption, xLabel, yLabel, text, type, color_text,
                                  leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical stacked bar
#' Single Vertical Stacked Bar
#' @name gg_bar_single_stacked_ver_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_single_stacked_ver_Cat(sampleData("Cat"))
gg_bar_single_stacked_ver_Cat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          fillLabel = NULL, leg_pos = "right", width = 0.3,
                                          text = TRUE, type = "count", color_text = "black", ...){

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

  graph <- ggplot(data = data_graph, aes(x = factor(1), y = count, fill = a)) +
    geom_bar(stat = "identity", width = width)

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())
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

#' Horizontal stacked bar
#' Single Horizontal Stacked Bar
#' @name gg_bar_single_stacked_hor_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_single_stacked_hor_Cat(sampleData("Cat"))
gg_bar_single_stacked_hor_Cat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          fillLabel = NULL, leg_pos = "right", width = 0.3,
                                          text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_single_stacked_ver_Cat(data, titleLabel, subtitle, caption, fillLabel, leg_pos, width,
                                         text, type, color_text)
  graph <- graph + coord_flip()

  graph
}

# Cat-Yea-Num BARS

#' Vertical bar facet
#' Barras vertical
#' @name gg_bar_facet_ver_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_facet_ver_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_facet_ver_CatYeaNum <- function(data,...){
  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data
  graph <- gg_bar_facet_ver_CatCatNum(selectFringeCols(f,c(2,1,3)), ...)
  graph
}


#' Horizontal bar facet
#' Barras stacked
#' @name gg_bar_facet_hor_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_facet_hor_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_facet_hor_CatYeaNum <- function(data,...){

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data

  graph <- gg_bar_facet_hor_CatCatNum(selectFringeCols(f,c(2,1,3)), ...)

  graph
}

#' Vertical grouped bar by first variable
#' Barras stacked
#' @name gg_bar_grouped_ver_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_grouped_ver_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_grouped_ver_CatYeaNum <- function(data,...){

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data

  graph <- gg_bar_grouped_ver_CatCatNum(selectFringeCols(f,c(2,1,3)), ...)

  graph
}

#' Horizontal grouped bar by first variable
#' Barras stacked
#' @name gg_bar_grouped_hor_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_grouped_hor_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_grouped_hor_CatYeaNum <- function(data,...){
  graph <- gg_bar_grouped_ver_CatYeaNum(data, ...) +
    coord_flip()
  graph
}


#' Vertical grouped bar by second variable
#' Barras stacked
#' @name gg_bar_grouped2_ver_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_grouped2_ver_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_grouped2_ver_CatYeaNum <- function(data,...){
  gg_bar_grouped_ver_CatCatNum(data, ...)
}

#' Horizontal grouped bar by second variable
#' Barras stacked
#' @name gg_bar_grouped2_hor_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_grouped2_hor_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_grouped2_hor_CatYeaNum <- function(data, ...){
  graph <- gg_bar_grouped2_ver_CatYeaNum(data, ...) +
    coord_flip()
  graph
}


#' Vertical stacked bar by first variable
#' Barras stacked
#' @name gg_bar_stacked_ver_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_stacked_ver_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_stacked_ver_CatYeaNum <- function(data,...){

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data

  graph <- gg_bar_stacked_ver_CatCatNum(selectFringeCols(f,c(2,1,3)), ...)
  graph
}


#' Horizontal stacked bar by first variable
#' Barras stacked
#' @name gg_bar_stacked_hor_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_stacked_hor_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_stacked_hor_CatYeaNum <- function(data, ...){

  graph <- gg_bar_stacked_ver_CatYeaNum(data, ...)
  graph + coord_flip()
}


#' Vertical 100% stacked bar by first variable
#' 100 vertical stacked bar graph
#' @name gg_bar_stacked_100_ver_CatYeaNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_stacked_100_ver_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_stacked_100_ver_CatYeaNum <- function(data,...) {

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data
  graph <- gg_bar_stacked_100_ver_CatCatNum(selectFringeCols(f,c(2,1,3)), ...)
  graph
}


#' Horizontal 100% stacked bar by first variable
#' Barras horizontal stacked
#' @name gg_bar_stacked_100_hor_CatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' gg_bar_stacked_100_hor_CatYeaNum(sampleData("Cat-Yea-Num"))
gg_bar_stacked_100_hor_CatYeaNum <- function(data, ...) {

  graph <- gg_bar_stacked_100_ver_CatYeaNum(data, ...)
  graph + coord_flip()
}

## Cat-Dar

#' Vertical stacked bar
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CatDatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_bar_stacked_ver_CatDatNum(sampleData("Cat-Dat-Num"))
gg_bar_stacked_ver_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, aggregation = "sum",
                                         hline = NULL, text = TRUE, type = "count", color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3])
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) %>%
    dplyr::mutate(b = as.Date(b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(percent = 100 * round(c / total, 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(b = as.Date(b)) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent)) %>%
    dplyr::mutate(percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(x = reorder(b, c), y = c, fill = a)) + geom_bar(stat = "identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.9)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.9)))
    }else{
      graph
    }
  }

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  graph
}

#' Horizontal stacked bar
#' horizontal stacked bar graph
#' @name gg_bar_stacked_hor_CatDatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_bar_stacked_hor_CatDatNum(sampleData("Cat-Dat-Num"))
gg_bar_stacked_hor_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, aggregation = "sum",
                                         hline = NULL, text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_stacked_ver_CatDatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x,
                                        aggregation, hline, text, type, color_text, ...)
  graph <- graph + coord_flip()

  graph
}


#' Vertical bar facet coloured by first variable
#' Facet Vertical coloured bars
#' @name gg_bar_coloured_facet_x_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_coloured_facet_x_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_coloured_facet_x_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                               text = TRUE, type = 'count', color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data_graph, aes(x = a, y = c, fill = a)) + geom_bar(stat = "identity")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    guides(text = FALSE)
  graph <- graph +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal bar facet coloured by first variable
#' Facet Horizontal coloured Bars
#' @name gg_bar_coloured_facet_x_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_coloured_facet_x_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_coloured_facet_x_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                               text = TRUE, type = 'count', color_text = "black", ...){

  graph <- gg_bar_coloured_facet_x_ver_CatCat(data, titleLabel, subtitle, caption, xLabel,
                                              yLabel, fillLabel, leg_pos, angle_x, text, type, color_text, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical bar facet coloured by second variable
#' Facet Vertical coloured bars
#' @name gg_bar_coloured_facet_y_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_coloured_facet_y_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_coloured_facet_y_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                               text = TRUE, type = 'count', color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data_graph, aes(x = a, y = c, fill = b)) + geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal bar facet coloured by second variable
#' Facet Horizontal coloured Bars
#' @name gg_bar_coloured_facet_y_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_coloured_facet_y_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_coloured_facet_y_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                               text = TRUE, type = 'count', color_text = "black", ...){

  graph <- gg_bar_coloured_facet_y_ver_CatCat(data, titleLabel, subtitle, caption, xLabel,
                                              yLabel, fillLabel, leg_pos, angle_x, text, type, color_text, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical bar facet highlighting some parameter
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_facet_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_coloured_parameter_facet_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_coloured_parameter_facet_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                       xLabel = NULL, yLabel = NULL,
                                                       parameter1 = NULL, parameter2 = NULL,
                                                       leg_pos = "right", angle_x = 0,
                                                       text = TRUE, type = "count", color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  parameters <- data %>% dplyr::group_by(a,b) %>% dplyr::summarise(count = n()) %>%
    dplyr::group_by(b) %>% dplyr::filter(count == max(count)) %>% dplyr::mutate(color = TRUE)
  p_a <-  parameter1 %||% parameters$a
  p_b <-  parameter2 %||% parameters$b

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::left_join(., parameters, by = c("a", "b", "count")) %>%
    dplyr::mutate(color = ifelse(is.na(color), FALSE, color)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = color)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(count,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal bar facet highlighting some parameter
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_facet_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_coloured_parameter_facet_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_coloured_parameter_facet_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                       xLabel = NULL, yLabel = NULL,
                                                       parameter1 = NULL, parameter2 = NULL,
                                                       leg_pos = "right", angle_x = 0,
                                                       text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_coloured_parameter_facet_ver_CatCat(data, titleLabel, subtitle, caption, xLabel,
                                                      yLabel, parameter1, parameter2, leg_pos, angle_x, text, type, color_text, ...)

  graph <- graph + coord_flip()
  graph
}

#' Vertical stacked bar by second variable
#' Stacked vertical Bar
#' @name gg_bar_stacked_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                      type = "count", color_text = "black", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(c) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::arrange(c) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data_graph, aes(x = reorder(a, c), y = c, fill=b)) + geom_bar(stat = "identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Horizontal stacked bar by second variable
#' Stacked horizontal Bar
#' @name gg_bar_stacked_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_hor_CatCat <- function(data, titleLabel = "", subtitle ="", caption = "", xLabel = NULL,
                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                      type = "count", color_text = "black", angle_x = 0,...){

  graph <- gg_bar_stacked_ver_CatCat(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel,
                                     leg_pos, text, type, color_text, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Ordered vertical stacked bar by second variable
#' Ordered Stacked vertical Bar
#' @name gg_bar_stacked_ordered_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_ordered_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_ordered_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel =  NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                              type = "count", color_text = "black", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  # graph <- ggplot(data, aes(x=reorder(data$b, rep(1, length(data$b)), sum), fill = a)) +
  #           geom_bar()

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data_order <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n()) %>%
    dplyr::group_by(a) %>% dplyr::summarise(sum = sum(count))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent))

  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data_graph, aes(x = reorder(a, c, sum), y = c, fill=b)) +
    geom_bar(stat = "identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

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

#' Ordered horizontal stacked bar by second variable
#' Ordered Stacked horizontal Bar
#' @name gg_bar_stacked_ordered_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_ordered_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_ordered_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel =  NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                              type = "count", color_text = "black", angle_x = 0,...){

  graph <- gg_bar_stacked_ordered_ver_CatCat(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel,
                                             leg_pos, text, type, color_text, angle_x)

  graph <- graph + coord_flip()

  graph
}

#' Vertical stacked dot bar
#' Stacked Vertical dot Bar
#' @name gg_bar_stacked_dot_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_dot_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_dot_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  graph <- ggplot(data = data, aes(a, fill = b)) +
    geom_dotplot(stackgroups = TRUE, binpositions = "all") +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal stacked dot bar
#' Stacked horizontal dot Bar
#' @name gg_bar_stacked_dot_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_dot_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_dot_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x =0, ...){

  graph <- gg_stacked_dot_bar_ver_CatCat(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x)

  graph <- graph + coord_flip()

  graph
}

#' Vertical grouped bar by second variable
#' Unstacked Coloured vertical Bar
#' @name gg_bar_grouped_coloured_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_grouped_coloured_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_grouped_coloured_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                               type = "count", color_text = "black", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = count*9/10,
                  percent = 100 * round(count / sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::spread(b, count, fill = 0) %>%
    tidyr::gather(b, count, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "count")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data, aes(a, weight=count, fill=b)) + geom_bar(position = "dodge")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    guides(text = FALSE)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 0.5)) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(count,2)),
                             check_overlap = TRUE, color = color_text, position = position_dodge(width=0.9)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_dodge(width=1)))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal grouped bar by second variable
#' Unstacked Coloured horizontal Bar
#' @name gg_bar_grouped_coloured_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_grouped_coloured_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_grouped_coloured_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                               type = "count", color_text = "black", angle_x = 0,...){

  graph <- gg_bar_grouped_coloured_ver_CatCat(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos,
                                              text, type, color_text, angle_x, ... )

  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked bar by second variable
#' Stacked 100pct vertical Bar
#' @name gg_bar_stacked_100_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_100_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_100_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE,
                                          type = "count", color_text = "black", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% paste("%", "count", nms[2], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent))

  graph <- ggplot(data, aes(a, y = c, fill=b)) +
    geom_bar(stat="identity", position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(c,2)),
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

#' Horizontal 100% stacked bar by second variable
#' Stacked 100pct horizontal Bar
#' @name gg_bar_stacked_100_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_stacked_100_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_stacked_100_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, leg_pos = "right",text = TRUE,
                                          type = "count", color_text = "black", angle_x = 0,...){


  graph <- gg_bar_stacked_100_ver_CatCat(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel,
                                         leg_pos, text, type, color_text, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal 100% stacked bar by first variable + facet by second variable
#' Stacked
#' @name gg_bar_stacked_100_hor_CatCatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Yea-Num
#' @examples
#' gg_bar_stacked_100_hor_CatCatYeaNum(sampleData("Cat-Cat-Yea-Num"))
gg_bar_stacked_100_hor_CatCatYeaNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                                aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                                angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[3]
  ylab <- yLabel %||% paste("%", aggregation, nms[4], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

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
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
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
#' @name gg_bar_stacked_100_ver_CatCatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Yea-Num
#' @examples
#' gg_bar_stacked_100_ver_CatCatYeaNum(sampleData("Cat-Cat-Yea-Num"))
gg_bar_stacked_100_ver_CatCatYeaNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                                aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                                angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[3]
  ylab <- yLabel %||% paste("%", aggregation, nms[4], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

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
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
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
#' @name gg_bar_stacked_ver_CatCatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Yea-Num
#' @examples
#' gg_bar_stacked_ver_CatCatYeaNum(sampleData("Cat-Cat-Yea-Num"))
gg_bar_stacked_ver_CatCatYeaNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                            aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                            angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[3]
  ylab <- yLabel %||% paste(aggregation, nms[4])
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c), !is.na(d))

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
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
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
#' @name gg_bar_stacked_hor_CatCatYeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Yea-Num
#' @examples
#' gg_bar_stacked_hor_CatCatYeaNum(sampleData("Cat-Cat-Yea-Num"))
gg_bar_stacked_hor_CatCatYeaNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                            fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                            aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                            angle_x = 0, ...){

  graph <- gg_bar_stacked_ver_CatCatYeaNum(data, titleLabel, subtitle, caption, fillLabel, xLabel,
                                           yLabel, leg_pos, aggregation, text, type, color_text, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}


#' Vertical grouped bar by second variable
#' vertical unstacked bargraph
#' @name gg_bar_grouped_ver_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_grouped_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_grouped_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                         xLabel = NULL, yLabel = NULL, leg_pos = "right", aggregation = "sum",
                                         text = TRUE, color_text = "black", type = "count", angle_x = 0,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(x=a, y=c, fill=b)) +
    geom_bar(stat="identity", position = "dodge")
  graph <- graph +  theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) + guides(text = FALSE)
  graph <- graph + scale_fill_manual(values = getPalette())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_dodge(width=1)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_dodge(width=1)))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal grouped bar by second variable
#' horizontal bar graph
#' @name gg_bar_grouped_hor_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_grouped_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_grouped_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                         xLabel = NULL, yLabel = NULL,
                                         leg_pos = "right",
                                         aggregation = "sum", text = TRUE, color_text = "black", type = "count",
                                         angle_x = 0,...){

  graph <- gg_bar_grouped_ver_CatCatNum(data, titleLabel, subtitle, caption, fillLabel,
                                        xLabel, yLabel, leg_pos,
                                        aggregation, text, color_text, type, angle_x, ...)

  graph + coord_flip()

}

#' Vertical grouped bar by first variable
#' Barras grouped
#' @name gg_bar_grouped2_ver_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_grouped2_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_grouped2_ver_CatCatNum <- function(data,...){
  data <- fringe(data)
  gg_bar_grouped_ver_CatCatNum(selectFringeCols(data, c(2,1,3)),...)
}

#' Horizontal grouped bar by first variable
#' Barras grouped
#' @name gg_bar_grouped2_hor_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_grouped2_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_grouped2_hor_CatCatNum <- function(data, ...){
  graph <- gg_bar_grouped2_ver_CatCatNum(data,...) +
    coord_flip()
  graph
}


#' Vertical stacked bar by second variable
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                         aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "stack")
  graph <- graph + theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph  + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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

#' Horizontal stacked bar by second variable
#' horizontal stacked bar graph
#' @name gg_bar_stacked_hor_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                         aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0, ...){
  graph <- gg_bar_stacked_ver_CatCatNum(data, titleLabel, subtitle, caption,
                                        xLabel, yLabel, fillLabel, leg_pos,
                                        aggregation, text, type, color_text, angle_x, ...)
  graph + coord_flip()
}


#' Vertical stacked bar by first variable
#' Barras grouped
#' @name gg_bar_stacked2_ver_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked2_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked2_ver_CatCatNum <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_ver_CatCatNum(selectFringeCols(data,c(2,1,3)),...)
}

#' Horizontal stacked bar by first variable
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_stacked2_hor_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked2_hor_CatCatNum(sampleData("Cat-Cat-Num))
gg_bar_stacked2_hor_CatCatNum <- function(data, ...){
  graph <- gg_bar_stacked2_ver_CatCatNum(data,...) +
    coord_flip()
  graph
}

#' Vertical 100% stacked bar by second variable
#' 100 vertical stacked bar graph
#' @name gg_bar_stacked_100_ver_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked_100_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked_100_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                             aggregation = "sum", text = TRUE, color_text = "black", type = "count",
                                             angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste("%", aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  data <- data %>% filter(!is.na(a),!is.na(b))

  graph <- ggplot(data, aes(a, y = c, fill=b)) +
    geom_bar(stat="identity", position = "fill") + theme_ds()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(c,2)),
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

#' Horizontal 100% stacked bar by second variable
#' 100 horizontal stacked bar graph
#' @name gg_bar_stacked_100_hor_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked_100_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked_100_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                             yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                             aggregation = "sum", text = TRUE, color_text = "black", type = "count",
                                             angle_x = 0, ...){


  graph <- gg_bar_stacked_100_ver_CatCatNum(data, titleLabel, subtitle, caption,
                                            xLabel, yLabel, fillLabel, leg_pos,
                                            aggregation, text, color_text, type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked bar by first variable
#' Barras grouped
#' @name gg_bar_stacked2_100_ver_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked2_100_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked2_100_ver_CatCatNum <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_100_ver_CatCatNum(selectFringeCols(data,c(2,1,3)),...)
}

#' Horizontal 100% stacked bar by first variable
#' Barras grouped
#' @name gg_bar_stacked2_100_hor_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_stacked2_100_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_stacked2_100_hor_CatCatNum <- function(data, ...){
  graph <- gg_bar_stacked2_100_ver_CatCatNum(data,...) +
    coord_flip()
  graph
}

#' Vertical facet bar
#' Facet vertical bargraph
#' @name gg_bar_facet_ver_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_facet_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_facet_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                       xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", angle_x = 0,
                                       aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = "")) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    facet_wrap(~b) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  return(graph)
}


#' Horizontal facet bar
#' Horizontal bar graph
#' @name gg_bar_facet_hor_CatCatNum
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_facet_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_facet_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                       xLabel = NULL, yLabel = NULL, leg_pos = "right",
                                       angle_x = 0, aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_facet_ver_CatCatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, aggregation,
                                      text, color_text, type, ...)

  graph <- graph + coord_flip()

  graph

}

#' Vertical facet bar coloured by first variable
#' vertical bar
#' @name gg_bar_coloured_facet_x_ver_CatCatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_coloured_facet_x_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_coloured_facet_x_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                  yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                  aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = a)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() +
    #theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}


#' Horizontal facet bar coloured by first variable
#' horizontal bar
#' @name gg_bar_coloured_facet_x_hor_CatCatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_coloured_facet_x_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_coloured_facet_x_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                  yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                  aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_coloured_facet_x_ver_CatCatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos,
                                                 angle_x, aggregation, text, color_text, type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical facet bar coloured by second variable
#' vertical bar
#' @name gg_bar_coloured_facet_y_ver_CatCatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_coloured_facet_y_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_coloured_facet_y_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                  yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                  aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = b)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) + theme_ds() +
    #theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }

  graph
}


#' Horizontal facet bar coloured by second variable
#' horizontal bar
#' @name gg_bar_coloured_facet_y_hor_CatCatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_coloured_facet_y_hor_CatCatNumsampleData("Cat-Cat-Num"))
gg_bar_coloured_facet_y_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                  yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0,
                                                  aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_coloured_facet_y_ver_CatCatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x,
                                                 aggregation, text, color_text, type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical facet bar density by first numeric variable
#' Facet coloured vertical bar
#' @name gg_bar_density_facet_z_ver_CatCatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_density_facet_z_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_density_facet_z_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                 yLabel = NULL, fillLabel = NULL, reverse = FALSE, leg_pos = "right", angle_x = 0,
                                                 aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% paste(aggregation, nms[3], sep = " ")
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

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = suma)) +
    geom_bar(stat = "identity") + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(suma,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal facet bar density by first numeric variable
#' Facet Coloured horizontal bar
#' @name gg_bar_density_facet_z_hor_CatCatNum
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_density_facet_z_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_density_facet_z_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                 yLabel = NULL, fillLabel = NULL, reverse = FALSE, leg_pos = "right", angle_x = 0,
                                                 aggregation = "sum", text = TRUE, color_text = "black", type = "count", ...){

  graph <- gg_bar_density_facet_z_ver_CatCatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, reverse, leg_pos,
                                                angle_x, aggregation, text, color_text, type, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical facet bar coloured highliting some parameter
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_facet_ver_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_coloured_parameter_facet_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_coloured_parameter_facet_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                          xLabel = NULL, yLabel = NULL,
                                                          parameter1 = NULL, parameter2 = NULL,
                                                          leg_pos = "right", angle_x = 0, aggregation = "sum",
                                                          text = TRUE, type = "count", color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  parameters <- data %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>% dplyr::filter(c == max(c)) %>% dplyr::mutate(color = TRUE)
  p_a <-  parameter1 %||% parameters$a
  p_b <-  parameter2 %||% parameters$b

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9/10, percent = 100 * round(c/total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c)) %>%
    dplyr::left_join(., parameters, by = c("a", "b", "c")) %>%
    dplyr::mutate(color = ifelse(is.na(color), FALSE, color),
                  percent = ifelse(percent == 0, NA, percent),
                  pos = ifelse(pos == 0, NA, pos),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data_graph, aes(x = a, y = c, fill = color)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data_graph, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Horizontal facet bar coloured highliting some parameter
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_facet_hor_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_bar_coloured_parameter_facet_hor_CatCatNum(sampleData("Cat-Cat-Num"))
gg_bar_coloured_parameter_facet_hor_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                          xLabel = NULL, yLabel = NULL,
                                                          parameter1 = NULL, parameter2 = NULL,
                                                          leg_pos = "right", angle_x = 0, aggregation = "sum",
                                                          text = TRUE, type = "count", color_text = "black", ...){

  graph <- gg_bar_coloured_parameter_facet_ver_CatCatNum(data, titleLabel, subtitle, caption, xLabel,
                                                         yLabel, parameter1, parameter2, leg_pos, angle_x, aggregation,
                                                         text, type, color_text)

  graph <- graph + coord_flip()
  graph
}

#' Circular bar facet
#' Circular Bar
#' @name gg_bar_circular_facet_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_facet_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                             leg_pos="right", aggregation = "sum", width = 0.85,
                                             text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::arrange(desc(c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(pos = c*9.7/10,
                  percent = 100 * round(c/total, 4),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(x = a, y = c , fill = a )) +
    geom_bar(width = width, stat="identity") +
    coord_polar(theta = "y")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()

  graph <- graph + theme(legend.position=leg_pos) + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Stacked polar bar
#' Stacked Polar Bar
#' @name gg_bar_stacked_polar_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_CatCatNum. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                            leg_pos= "right", aggregation = "sum", text = TRUE, color_text = "black",
                                            type = "count", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "stack", stat = "identity") +
    coord_polar()
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph +
    theme_ds() +
    #theme_ds_clean() +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

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

#' Stacked 100% polar bar
#' Stacked Polar Bar 100
#' @name gg_bar_stacked_polar_100_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_100_CatCatNum. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                                                fillLabel = NULL, leg_pos= "right",
                                                aggregation = "sum", text = TRUE, color_text = "black",
                                                type = "count", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "fill", stat = "identity") +
    coord_polar() + theme_ds() +
    #theme_ds_clean() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)
  graph <- graph + theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab, x = "", y = "") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(c,2)),
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


#' Vertical dot bar
#' Vertical Dot Bar
#' @name gg_bar_dot_ver_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_bar_dot_ver_CatNum(sampleData("Cat-Num"))
gg_bar_dot_ver_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", sort = "no", topn = NULL,
                                  xLabel = NULL, yLabel = NULL, leg_pos="right", angle_x = 0, aggregation ="sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[2])
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  if (nrow(data) == 0)
    return()
  if (sort == "top") {
    data <- data %>% dplyr::arrange(desc(b))
    if (!is.null(topn)) {
      data <- dplyr::slice(data, 1:topn)
    } else {
      data <- data
    }
  }

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(suma = agg(aggregation, b))

  data_graph <- data_graph %>%
    dplyr::mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = a, fill = a)) + geom_dotplot(method = "histodot", show.legend = FALSE)
  #graph <- ggplot(data_graph, aes(x = as.factor(a), fill = a, y = suma)) + geom_dotplot(stackgroups = TRUE, method = "histodot")
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph + scale_y_continuous(breaks = NULL) +
    theme(legend.position = leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal dot bar
#' Horizontal Dot Bar
#' @name gg_bar_dot_hor_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_bar_dot_hor_CatNum(sampleData("Cat-Num"))
gg_bar_dot_hor_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", sort = "no", topn = NULL,
                                  xLabel = NULL, yLabel = NULL, leg_pos = "right", angle_x = 0, aggregation = "sum", ...){

  graph <- gg_bar_dot_ver_CatNum(data, titleLabel, subtitle, caption, sort, topn, xLabel, yLabel, leg_pos, angle_x, aggregation, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical dot bar
#' Vertical Dot Bar
#' @name gg_bar_dot_ver_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_dot_ver_Cat(sampleData("Cat"))
gg_bar_dot_ver_Cat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())
  data_graph <- data_graph %>%
    mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = a, fill = a)) + geom_dotplot(method="histodot", show.legend = FALSE) +
    labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)
  graph <- graph +
    scale_y_continuous(breaks = NULL) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal dot bar
#' Horizontal Dot Bar
#' @name gg_bar_dot_hor_Cat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' gg_bar_dot_hor_Cat(sampleData("Cat"))
gg_bar_dot_hor_Cat <- function(data, titleLabel = "", xLabel = NULL, yLabel = NULL,
                               subtitle = "", caption = "", leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_dot_ver_Cat(data, titleLabel, xLabel, yLabel, subtitle = subtitle, caption = caption,
                              leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}





#' Polar bar
#' Polar Bar
#' @name gg_bar_polar_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_Cat. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                              fillLabel = NULL, leg_pos= "right", text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(pos = count*8/10,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data=data_graph, aes(a, fill = a, weight = count)) +
    geom_bar(width = width) + coord_polar()
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

#' Vertical dot bar facet
#' Facet Vertical Dot Bar
#' @name gg_bar_dot_facet_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_dot_facet_ver_CatCat(sampleData("Cat-Cat"))
gg_bar_dot_facet_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right",
                                        angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  data_graph <- data_graph %>%
    mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = a, fill = a)) + geom_dotplot(method="histodot", show.legend = FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) + facet_wrap(~b)


  graph
}

#' Horizontal dot bar facet
#' Facet Horizontal Dot Bar
#' @name gg_bar_dot_facet_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_bar_dot_facet_hor_CatCat(sampleData("Cat-Cat"))
gg_bar_dot_facet_hor_CatCat <- function(data, titleLabel = "", subtitle ="", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_bar_dot_facet_ver_CatCat(data, titleLabel, subtitle, caption,  xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#Width debe de ser un parámetro.  0 < width < 1.
#' Donut facet
#' Facet Donut
#' @name gg_donut_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                   width = 0.3, leg_pos="right", text = TRUE, type = 'count', color_text = "black", ...){

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
                  percent = 100 * round(c / total, 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data=data_graph, aes(x = factor(1), weight = c, fill = a)) +
    geom_bar(width = width) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    guides(text = FALSE)
  graph <- graph +
    theme_ds() +
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
#' Polar 100% stacked bar
#' Stacked Polar  100pct
#' @name gg_bar_polar_stacked_100_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_stacked_100_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                             leg_pos = "right", width = 0.95, text = TRUE, color_text = "black",
                                             type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))



  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "fill", stat = "identity") +
    coord_polar() + theme_ds() +
    #theme_ds_clean() +
    scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = percent/100, label = round(c,2)),
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



#' Circular bar facet
#' Circular Bar
#' @name gg_bar_circular_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                          leg_pos="right", width = 0.85,
                                          text = TRUE, type = "count", color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::mutate(pos = count*9.7/10,
                  percent = 100 * round(count/total, 4),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data, aes(x = a, y = count , fill = a )) +
    geom_bar(width = width, stat="identity") +
    coord_polar(theta = "y")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()

  graph <- graph + theme(legend.position=leg_pos) + facet_wrap(~b)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(count,2)),
                             check_overlap = TRUE, color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
  graph
}

#' Donut facet
#' Facet Donut
#' @name gg_donut_facet_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_facet_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                      width = 0.3, leg_pos = "right",
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

  graph <- ggplot(data=data, aes(x = factor(1), fill = a, weight = c)) +
    geom_bar(width = width) +
    coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
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

#' Pyramid
#' pyramid
#' @name gg_pyramid_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pyramid_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, aggregation = "sum",
                                  yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, text = TRUE, type = "count",
                                  color_text = "black", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  xlab <- xLabel %||% nms[2]
  clab <- fillLabel %||% nms[1]
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
                  suma = ifelse(suma == 0, NA, suma)) %>%
    dplyr::filter(a %in% unique(.$a)[1:2])

  data_graph$suma <- ifelse(data_graph$a %in% unique(data_graph$a)[1], -data_graph$suma, data_graph$suma)
  data_graph$pos <- ifelse(data_graph$a %in% unique(data_graph$a)[1], -data_graph$pos, data_graph$pos)

  graph <- ggplot(data_graph, aes(x = b, y = suma, fill = a)) +
    geom_bar(data = subset(data_graph, a %in% unique(data_graph$a)[1]), stat = "identity")

  if(text == TRUE & type == 'count'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[1]), aes(y = pos, label = abs(round(suma,2))),
                               check_overlap = TRUE, color = color_text)
  }else if(text == TRUE & type == 'percent'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[1]), aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text)
  }

  graph <- graph +
    geom_bar(data = subset(data_graph, a %in% unique(data_graph$a)[2]), stat = "identity", position = "identity")

  if(text == TRUE & type == 'count'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[2]), aes(y = pos, label = abs(round(suma,2))),
                               check_overlap = TRUE, color = color_text, position = "identity")
  }else if(text == TRUE & type == 'percent'){
    graph <- graph + geom_text(data = subset(data_graph, a %in% unique(data_graph$a)[2]), aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = "identity")
  }

  graph <- graph + theme_ds() +
    scale_fill_manual(values=getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    scale_y_continuous(labels = abs) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    coord_flip()

  graph
}

#' Polar stacked bar
#' Stacked Polar bar
#' @name gg_bar_polar_stacked_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_stacked_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", width = 0.95,
                                         fillLabel = NULL, text = TRUE, type = 'count', color_text = "black", leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <-fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    dplyr::mutate(pos = c*9/10,
                  percent = 100 * round(c / sum(c), 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data = data, aes(x = a, y = c, fill = b)) +
    geom_bar(width = width, position = "stack", stat = "identity") +
    coord_polar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab)
  graph <- graph + theme_ds() + #theme_ds_clean() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = pos, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = pos, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.5)))
    }else{
      graph
    }
  }
  return(graph)
}

