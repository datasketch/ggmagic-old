#' Donut
#' dount
#' @name gg_donut_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                             width = 0.3, leg_pos="right", aggregation = 'sum',
                             text = TRUE, color_text = "black", type = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = agg(aggregation, b))  %>%
    dplyr::arrange(desc(a)) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4)) %>%
    dplyr::mutate(pos = ifelse(pos == 0, NA, pos),
                  percent = ifelse(percent == 0, NA, percent),
                  count = ifelse(count == 0, NA, count))

  graph <- ggplot(data = data_graph, aes(x = factor(1), fill = a, y = count)) +
    geom_bar(stat = "identity", width = width) + coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = clab) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()
  graph <- graph + theme(legend.position = leg_pos)

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

#Width debe de ser un parámetro.  0 < width < 1.
#' Donut
#' Donut
#' @name gg_donut_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_Cat. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                          width = 0.3, text = TRUE, type = 'count', color_text = "black", leg_pos = "right", ...){

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
    geom_bar(stat = "identity", width = width) + coord_polar(theta = "y")
  graph <- graph +
    labs(title = titleLabel, x = "", y = "", fill = clab, subtitle = subtitle, caption = caption)
  graph <- graph + theme_ds_clean() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(y = pos, label = round(count,2)), check_overlap = TRUE, stat = "identity", position = "identity",  color = color_text))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(y = pos, label = paste(percent, "%", sep = "")), check_overlap = TRUE, color = color_text))
    }else{
      graph
    }
  }
}
