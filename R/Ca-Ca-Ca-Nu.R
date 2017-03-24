
#' Treemap coloured by first variable
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
gg_treemap_x_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                   aggregation = "sum", text = "TRUE", color_text = "black",
                                   leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

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
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by second variable
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
gg_treemap_y_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                   aggregation = "sum", text = "TRUE", color_text = "black",
                                   leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[2]
  data <- f$d

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
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}

#' Treemap coloured by third variable
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
gg_treemap_z_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                   aggregation = "sum", text = "TRUE", color_text = "black",
                                   leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[3]
  data <- f$d

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
    theme_ds() + theme_ds_clean() + theme(legend.position=leg_pos)

  graph
}


#' Horizontal 100% stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_100_hor_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                             aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

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

#' Vertical 100% stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_100_ver_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                             aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

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

#' Vertical stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_ver_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                             aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                             angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

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

#' Horizontal stacked bar by first variable + facet by third variable
#' Stacked
#' @name gg_bar_stacked_hor_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                         fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right',
                                         aggregation = "sum", text = TRUE, type = "count", color_text = "black",
                                         angle_x = 0, ...){

  graph <- gg_bar_stacked_ver_CaCaCaNu.(data, titleLabel, subtitle, caption, fillLabel, xLabel,
                                        yLabel, leg_pos, aggregation, text, type, color_text, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}


#' Sunburst
#' sunburst
#' @name gg_sunburst_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_sunburst_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", aggregation = "sum",
                                  fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right', ...){



  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  data$a <- as.factor(data$a)
  data$b <- as.factor(data$b)
  data$c <- as.factor(data$c)

  #angulos

  pred_ang <-  function(perc){
    angle = -1

    if(perc < 0.25) # 1st q [90,0]
      angle = 90 - (perc/0.25) * 90
    else if(perc < 0.5) # 2nd q [0, -90]
      angle = (perc-0.25) / 0.25 * -90
    else if(perc < 0.75) # 3rd q [90, 0]
      angle = 90 - ((perc-0.5) / 0.25 * 90)
    else if(perc < 1.00) # last q [0, -90]
      angle = ((perc -0.75)/0.25) * -90

    if(perc < 0.5) # 1st half [90, -90]
      angle = (180 - (perc/0.5) * 180) - 90
    else # 2nd half [90, -90]
      angle = (90 - ((perc - 0.5)/0.5) * 180)

    return(angle)
  }

  #primer nivel

  part1 <- data %>%
           dplyr::group_by(a) %>%
           dplyr::summarise(total1 = agg(aggregation, d)) %>%
           dplyr::mutate(running = cumsum(total1), pos = running - total1/2) %>%
           dplyr::group_by(1:n()) %>%
           dplyr::mutate(angle = pred_ang((pos)/total1)) %>%
           dplyr::arrange(a, -total1)

  sunb0 <- ggplot(part1)
  sunb1 <- sunb0 +
           geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
           geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle), check_overlap = TRUE) +
           scale_fill_continuous(low = '#009EE3', high = '#E5007D')

  #segundo nivel

  part2 <- data %>%
           dplyr::group_by(a,b) %>%
           dplyr::summarise(total1 = agg(aggregation, d)) %>%
           ungroup() %>%
           dplyr::group_by(a) %>%
           dplyr::arrange(a,-total1) %>%
           ungroup() %>%
           mutate(running = cumsum(total1), pos = running - total1/2) %>%
           group_by(1:n()) %>%
           mutate(angle = pred_ang((running - total1/2)/total1))


  sunb2 <- sunb1 +
           geom_bar(data = na.omit(part2), aes(x=2, y = total1,  fill = total1),na.rm = TRUE,stat = 'identity', color = 'white', position = 'stack') +
           geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle), check_overlap = TRUE)



  #tercer nivel

  part3 <- data %>%
           #tidyr::drop_na(c) %>%
           dplyr::group_by(a,b,c) %>%
           dplyr::summarise(total1 = agg(aggregation, d)) %>%
           dplyr::arrange(a,c,-total1) %>%
           ungroup() %>%
           mutate(running = cumsum(total1), pos = running - total1/2) %>%
           group_by(1:n()) %>%
           mutate(angle = pred_ang((running - total1/2)/total1))

 part3$total1[is.na(part3$c)] <- NA


  graph <- sunb2 +
    geom_bar(data = part3, aes(x=3, y = total1,  fill = total1), stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part3, aes(label=part3$c, x=3, y=pos, angle=angle), check_overlap = TRUE)

  graph +
  coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)

}

