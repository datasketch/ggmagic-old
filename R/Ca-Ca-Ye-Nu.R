#' gg_bar_stacked_100_hor_CaCaYeNu.
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
                                             fillLabel = NULL, xlab = NULL, ylab = NULL,leg_pos = 'right', ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data$d <- abs(data$d)

  data_graph <- data %>%
    #tidyr::drop_na(a,c) %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(count = sum(d, na.rm = TRUE)) %>%
    tidyr::spread(c, count) %>%
    tidyr::gather(c,count,c(-a,-b) )

  data_graph[is.na(data_graph)] <- 0


  graph <- ggplot(data_graph,aes(x = c, y = count,fill = a)) +
    geom_bar(position = "fill",stat = "identity") +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    theme_ds() +
    facet_grid(b ~ .) +
    #facet_grid(b ~ ., switch = 'y') +
    scale_fill_manual(values=getPalette())

  graph <-  graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(legend.position = leg_pos)

  graph
}

