#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_ver_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                                          xLabel = NULL, yLabel = NULL, leg_pos = "right", aggregation = "sum",
                                          text = TRUE, color_text = "black", type = "count", angle_x = 0,...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
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



  gg_bar_stacked_ver_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
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


  gg_bar_stacked_100_ver_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
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
