
#' Vertical boxplot
#' Boxplot
#' @name gg_boxplot_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
    geom_boxplot(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal boxplot
#' Boxplot flipped
#' @name gg_boxplot_flip_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_flip_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  graph <- gg_boxplot_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical boxplot + jitter
#' Boxplot + dot jitter
#' @name gg_boxplot_dot_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_dot_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) + geom_jitter(color = "#D55E00", show.legend = FALSE) +
    geom_boxplot(show.legend = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal boxplot + jitter
#' Boxplot + dot jitter flipped
#' @name gg_boxplot_dot_flip_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_boxplot_dot_flip_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){


  graph <- gg_boxplot_dot_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}


#' Vertical boxplot
#' Box plot
#' @name gg_box_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
                        xLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "indice"
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  graph <- ggplot(data, aes(x=factor(""), y=a)) + geom_boxplot(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Horizontal boxplot
#' Box plot flipped
#' @name gg_box_flip_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
                             xLabel = NULL, angle_x = 0, ...){

  graph <- gg_box_Num.(data, titleLabel, subtitle, caption, yLabel, xLabel, angle_x = 0, ...)
  graph <- graph + coord_flip()

  graph
}

#' Boxplot
#' @name gg_box_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                           xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data) + geom_boxplot(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
    scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = comma) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

