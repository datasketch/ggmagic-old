









#Width debe de ser un parámetro.  0 < width < 1.

#' Horizon
#' Horizon
#' @name gg_horizon_YeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_YeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel =  NULL, leg_pos = "right",reverse = FALSE, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph + theme_ds() +
    labs(tittle = titleLabel, subtitle = subtitle, caption =caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
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
#' Horizon
#' Horizon
#' @name gg_horizon_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel =  NULL, leg_pos = "right", reverse = FALSE, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  data_graph <- data %>% mutate(xorder = 1:nrow(.))

  graph <- ggplot_horizon(data_graph, 'xorder', 'a')
  graph <- graph + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption =caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
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



# Gauge media, moda (discutir con JP)



#' Horizon
#' Horizon
#' @name gg_horizon_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, reverse = FALSE,
                               yLabel =  NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot_horizon(data, 'a', 'b') + theme_ds() +
    labs(title = titleLabel, subtitle, caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position = leg_pos)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  graph
}

#' Horizon
#' Horizon
#' @name gg_horizon_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel =  NULL, leg_pos = "right", reverse = FALSE,
                               angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
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

















#' Waffle
#' Waffle
#' @name gg_waffle_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waffle_Cat. <- function(data, square_size = 1, rows_number = 5, titleLabel = "", fillLabel = NULL,
                           subtitle = "", caption = "", leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  parts <- as.vector(data_graph$count)
  graph <- waffle(parts / square_size, rows = rows_number, colors=getPalette()) + theme_ds() +
    theme_ds_clean()  +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = clab)

  graph <- graph + scale_fill_manual(values = getPalette(),
                                     breaks = LETTERS[1:length(unique(data_graph$a))],
                                     labels = unique(data_graph$a)) + theme(legend.position = leg_pos)

  graph
}




















#Width debe de ser un parámetro.  0 < width < 1.

