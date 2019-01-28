#' Steam
#' Steam
#' @name gg_steam_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "ndice"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n()) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data_graph, aes(x = xorder, y = b, group = a, fill = a)) +
    stat_steamgraph() +
    labs(tittle = titleLabel, x = xlab, y = ylab, fill = clab) +
    scale_fill_manual(values = getPalette()) + theme_ds()
  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Steam
#' Steam
#' @name gg_steam_CatYeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CatYeaNum. <-  function(data, titleLabel = "",  subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)  +
    theme(legend.position = leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Steam
#' Steamgraph
#' @name gg_steam_CatNumNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CatNumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, aggregation = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)
  data_graph$c <- as.numeric(data_graph$c)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(legend.position = leg_pos)

  graph
}

#' Steam
#' Steam
#' @name gg_steam_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CatDatNum. <- function (data, titleLabel = "", subtitle = "", caption = "",
                                 xLabel = NULL, yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
    tidyr::drop_na(a,b) %>%
    dplyr::group_by(a) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a, fill = a)) +
    stat_steamgraph() +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x,hjust = 1)) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(legend.position = leg_pos)
  graph
}

