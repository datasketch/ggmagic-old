#' gg_bubble_CaCa.
#' Bubble
#' @name gg_bubble_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_CaCa.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(Count = n()) %>%
                dplyr::arrange(desc(Count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = Count, color = "")) +
             geom_point()
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
           theme_ds() +
           scale_color_manual(values = getPalette()) +
           guides(size = FALSE, colour = FALSE)

  graph
}

#' gg_bubble_coloured_CaCa.
#' Coloured Bubble
#' @name gg_bubble_coloured_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_coloured_CaCa.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(Count = n()) %>%
                dplyr::arrange(desc(Count))

  graph <- ggplot(data_graph, aes(x = a, y = b, size = Count)) +
           geom_point(aes(color = a))
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
           theme_ds() +
           scale_color_manual(values = getPalette()) +
           guides(colour = FALSE, size = FALSE)

  graph
}

#' gg_dot_bar_ver_facet_CaCa.
#' Facet Vertical Dot Bar
#' @name gg_dot_bar_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, ncol = 1, fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a) %>%
                dplyr::summarise(count = n())

  data_graph <- data_graph %>%
                mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a))) + geom_dotplot(method="histodot") +
           scale_fill_manual(values = getPalette())
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab,  fill = flabel)
  graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
           theme(legend.position=leg_pos) + facet_grid(. ~b)


  graph
}

#' gg_dot_bar_hor_facet_CaCa.
#' Facet Horizontal Dot Bar
#' @name gg_dot_bar_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle ="", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  graph <- gg_dot_bar_ver_facet_CaCa.(data, titleLabel, subtitle, caption,  xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_pie_facet_CaCa.
#' Facet Pie
#' @name gg_pie_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                               leg_pos="right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
             geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "", fill = flabel) +
           theme(legend.position=leg_pos) +
           guides(text = FALSE)
  graph <- graph +
           theme_ds_clean() +
           scale_fill_manual(values = getPalette())
  graph <- graph + facet_grid(.~b)

  graph
}

#Width debe de ser un parámetro.  0 < width < 1.
#' gg_donut_facet_CaCa.
#' Facet Donut
#' @name gg_donut_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                 width = 0.3, leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) +
             geom_bar(width = width) +
             coord_polar(theta = "y")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
           theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph +
           theme_ds() +
           theme_ds_clean() +
           scale_fill_manual(values = getPalette())
  graph <- graph + facet_grid(.~b)

  graph
}

#' gg_bullseye_facet_CaCa.
#' Facet Bullseye
#' @name gg_bullseye_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", leg_pos="right", ...){

  f <- fringe(data)
  data <- f$d


  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
             geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
           theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph +
           theme_ds() +
           theme_ds_clean() +
           scale_fill_manual(values = getPalette())
  graph <- graph + facet_grid(.~b)

  graph
}

#' gg_bar_coloured_x_ver_facet_CaCa.
#' Facet Vertical coloured bars
#' @name gg_bar_coloured_x_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = 'Count', leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data = data, aes(x = a, fill = factor(a))) + geom_bar()
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
           theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph +
           theme_ds() +
           scale_fill_manual(values = getPalette()) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
  graph <- graph + facet_grid(.~b)

  graph
}

#' gg_bar_coloured_x_hor_facet_CaCa.
#' Facet Horizontal coloured Bars
#' @name gg_bar_coloured_x_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = 'Count', leg_pos = "right", ...){

  graph <- gg_bar_coloured_x_ver_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel,
                                   yLabel, leg_pos)

  graph <- graph + coord_flip()
  graph
}

#' gg_bar_coloured_y_ver_facet_CaCa.
#' Facet Vertical coloured bars
#' @name gg_bar_coloured_y_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = 'Count', leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, fill = factor(b))) + geom_bar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  graph <- graph + facet_grid(.~b)
  graph
}

#' gg_bar_coloured_y_hor_facet_CaCa.
#' Facet Horizontal coloured Bars
#' @name gg_bar_coloured_y_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_y_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = 'Count', leg_pos = "right", ...){

  graph <- gg_bar_coloured_y_ver_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel,
                                             yLabel, leg_pos)

  graph <- graph + coord_flip()
  graph
}

#' gg_bar_coloured_parameter_ver_facet_CaCa.
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                      xLabel = NULL, yLabel = 'Count',
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  p_a <-  parameter1 %||% sample(unique(data[,nms[1]]), length(unique(data[,nms[2]])))
  p_b <-  parameter2 %||% sample(unique(data[,nms[2]]), length(unique(data[,nms[2]])))
  data <- f$d
  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(count = n())

  list_df <- apply(cbind(p_a, p_b), 1, function(x){
                                       df <- data_graph %>%
                                             mutate(color = ifelse(a==x[1] & b==x[2], TRUE, FALSE))
                                       df[df$color,]
              })
  df <- bind_rows(list_df)
  data_graph <- left_join(data_graph, df, by = c("a", "b", "count"))
  data_graph[is.na(data_graph)] <- FALSE

  graph <- ggplot(data_graph, aes(a, weight = count)) +
             geom_bar(position ="dodge", aes(fill =  color %in% TRUE))
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
           theme(legend.position=leg_pos) + guides(fill = FALSE)
  graph <- graph +
           theme_ds() +
           scale_fill_manual(values = getPalette()) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
  graph <- graph + facet_grid(.~b)

  graph
}

#' gg_bar_coloured_parameter_hor_facet_CaCa.
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                      xLabel = NULL, yLabel = 'Count',
                                                      parameter1 = NULL, parameter2 = NULL,
                                                      leg_pos = "right", ...){

  graph <- gg_bar_coloured_parameter_ver_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel,
                                             yLabel, parameter1, parameter2, leg_pos)

  graph <- graph + coord_flip()
  graph
}

#' gg_bar_stacked_ver_CaCa.
#' Stacked vertical Bar
#' @name gg_bar_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                yLabel = 'Count', leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(a, fill=b)) + geom_bar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    theme(legend.position=leg_pos) + guides(text = FALSE, fill=guide_legend(ncol=l_ncol))
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())

  graph
}

#' gg_bar_stacked_hor_CaCa.
#' Stacked horizontal Bar
#' @name gg_bar_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCa. <- function(data, titleLabel = "", subtitle ="", caption = "", xLabel = NULL,
                                  yLabel = 'Count', leg_pos = "right",l_ncol = 1, ...){

  graph <- gg_bar_stacked_ver_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos,l_ncol)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_ordered_stacked_hor_CaCa.
#' Ordered Stacked horizontal Bar
#' @name gg_bar_ordered_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_stacked_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel =  'Count', leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data, aes(x=reorder(data$b, rep(1, length(data$b)), sum), fill = a)) +
            geom_bar()

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = yLabel, y = xLabel) +
    theme(legend.position=leg_pos) + guides(text = FALSE, fill=guide_legend(ncol=l_ncol))
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())

  graph
}

#' gg_bar_ordered_stacked_ver_CaCa.
#' Ordered Stacked Vertical Bar
#' @name gg_bar_ordered_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_stacked_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel =  'Count', leg_pos = "right",l_ncol = 1, ...){

  graph <- gg_bar_ordered_stacked_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, l_ncol)

  graph <- graph + coord_flip()

  graph
}

#' gg_stacked_dot_bar_hor_CaCa.
#' Stacked horizontal dot Bar
#' @name gg_stacked_dot_bar_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_dot_bar_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = 'Count', leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(a, fill = factor(b))) +
                  geom_dotplot(stackgroups = TRUE, binpositions = "all") +
           scale_fill_manual(values = getPalette())

  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
  graph <- graph + theme_ds() + scale_y_continuous(breaks = NULL) +
            theme(legend.position=leg_pos)

  graph
}

#' gg_stacked_dot_bar_ver_CaCa.
#' Stacked vertical dot Bar
#' @name gg_stacked_dot_bar_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stacked_dot_bar_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = 'Count', leg_pos = "right", ...){

  graph <- gg_stacked_dot_bar_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

  graph
}

#' gg_bar_grouped_coloured_hor_CaCa.
#' Unstacked Coloured horizontal Bar
#' @name gg_bar_grouped_coloured_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_coloured_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = "Count", leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  graph <- ggplot(data_graph, aes(a, weight=count, fill=b)) + geom_bar(position = "dodge")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
           theme(legend.position=leg_pos) + guides(text = FALSE, fill=guide_legend(ncol=l_ncol))
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())


  graph
}

#' gg_bar_grouped_coloured_ver_CaCa.
#' Unstacked Coloured vertical Bar
#' @name gg_bar_grouped_coloured_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_coloured_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = "Count", leg_pos = "right", l_ncol = 1, ...){
  graph <- gg_bar_grouped_coloured_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos,l_ncol)

  graph <- graph + coord_flip()

  graph
}

#' gg_line_hor_facet_CaCa.
#' Horizontal Line
#' @name gg_line_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = "Count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b, colour = "")) + geom_line() +
    facet_grid(. ~b)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) + guides(color = FALSE)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette())


  graph
}

#' gg_line_ver_facet_CaCa.
#' Vertical Line
#' @name gg_line_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = "Count", ...){

  graph <- gg_line_hor_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  graph
}

#' gg_line_point_hor_facet_CaCa.
#' Horizontal Line Point
#' @name gg_line_point_hor_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_hor_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = "Count", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b, colour = "")) + geom_line() +
             geom_point() + facet_grid(. ~b)
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
           guides(color = FALSE)
  graph <- graph +
           theme_ds() +
           scale_color_manual(values = getPalette())

  graph
}

#' gg_line_point_ver_facet_CaCa.
#' Vertical Line Point
#' @name gg_line_point_ver_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_ver_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = "Count", ...){

  graph <- gg_line_point_hor_facet_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_stacked_100_ver_CaCa.
#' Stacked 100pct vertical Bar
#' @name gg_bar_stacked_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = 'Percent', leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data, aes(a, fill = b)) + geom_bar(position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
           guides(color = FALSE, fill = guide_legend(ncol=l_ncol)) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
           scale_y_continuous(labels = percent)

  graph
}

#' gg_bar_stacked_100_hor_CaCa.
#' Stacked 100pct horizontal Bar
#' @name gg_bar_stacked_100_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = 'Percent', leg_pos = "right", l_ncol = 1, ...){


  graph <- gg_bar_stacked_100_ver_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, l_ncol)
  graph <- graph + coord_flip()

  graph
}

#' gg_area_stacked_hor_CaCa.
#' Stacked horizontal Area
#' @name gg_area_stacked_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = 'Count', leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d


  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)
  graph <- graph + theme(legend.position=leg_pos) + guides(text = FALSE, fill=guide_legend(ncol=l_ncol))
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())


  graph
}

#' gg_area_stacked_ver_CaCa.
#' Stacked vertical Area
#' @name gg_area_stacked_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = 'Count', leg_pos = "right", l_ncol = 1, ...){

  graph <- gg_area_stacked_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, l_ncol)
  graph <- graph + coord_flip()

  graph
}

#' gg_area_stacked_100_hor_CaCa.
#' Stacked horizontal Area 100pct
#' @name gg_area_stacked_100_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d


  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)
  graph <- graph + theme(legend.position=leg_pos) + guides(fill=guide_legend(ncol=l_ncol))
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
          scale_y_continuous(labels = percent)

  graph
}

#' gg_area_stacked_100_ver_CaCa.
#' Stacked vertical Area 100pct
#' @name gg_area_stacked_100_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", l_ncol = 1, ...){

  graph <- gg_area_stacked_100_hor_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, l_ncol)
  graph <- graph + coord_flip()

  graph
}

#' gg_marimekko_ver_CaCa.
#' Vertical Marimekko
#' @name gg_marimekko_ver_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_ver_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, leg_pos = "right", l_ncol = 1, ...){
  f <- fringe(data)
  data <- f$d
  xvar <- deparse(substitute(a))
  yvar <- deparse(substitute(b))
  mytable <- table(data)
  widths <- c(0, cumsum(apply(mytable, 1, sum)))
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))})

  alldata <- data.frame()
  allnames <- data.frame()
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]))
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")

  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)))
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable))

  graph <- ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(color="black", aes_string(fill=yvar)) +
    labs(x = xLabel, y = yLabel, title = titleLabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme(legend.position=leg_pos) + guides(fill=guide_legend(ncol=l_ncol))
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())

  graph
}

#' gg_marimekko_hor_CaCa.
#' Horizontal Marimekko
#' @name gg_marimekko_hor_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_hor_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, leg_pos = "right", l_ncol = 1, ...){
  graph <- gg_marimekko_ver_CaCa.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos,l_ncol)
  graph <- graph + coord_flip()
  graph
}

#' gg_bar_polar_stacked_CaCa.
#' Stacked Polar bar
#' @name gg_bar_polar_stacked_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_stacked_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, text = TRUE, type = 'percent', text_size = 3, leg_pos = "right", l_ncol = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(a, fill = b, weight = count)) + geom_bar(width = 1) +
    coord_polar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)
  graph <- graph + theme_ds() +
           scale_fill_manual(values = getPalette()) +
           guides(fill = guide_legend(ncol=l_ncol)) +
           theme(axis.line.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.title.y = element_blank())
  return(graph)

}

#' gg_bar_polar_stacked_100_CaCa.
#' Stacked Polar  100pct
#' @name gg_bar_polar_stacked_100_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_stacked_100_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, text = TRUE, type = 'percent', text_size = 2,
                                           leg_pos = "right", width = 1, l_ncol = 1,  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a,b) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::arrange(desc(count)) %>%
                dplyr::mutate(percent = 100 * round(count/sum(count), 4)) %>%
                dplyr::mutate(pos = (scales::rescale(count, c(0.2,1)))-0.1)



  graph <- ggplot(data_graph, aes(a, fill = b, weight = count)) +
             geom_bar(width = 1, position = "fill")  + coord_polar()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel)
  graph <- graph + theme(legend.position=leg_pos) + guides(fill=guide_legend(ncol=l_ncol)) + theme_ds()
  graph <- graph +
           scale_fill_manual(values = getPalette()) +
           theme(axis.line.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank())


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(y = pos, label = count), size = text_size))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(y = pos, label = paste(percent, "%", sep = "")), size = text_size))
    }else{
      graph
    }
  }
}



#' gg_bar_circular_facet_CaCa.
#' Circular Bar
#' @name gg_bar_circular_facet_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_facet_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                        leg_pos="right", width = 0.85, ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = a, y = count , fill = a )) +
             geom_bar(width = width, stat="identity") +
             coord_polar(theta = "y")
graph <- graph +
         labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph +
           theme(legend.position=leg_pos) +
           theme_ds() +
           theme_ds_clean() +
           scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  graph
}

#' gg_treemap_x_CaCa.
#' Treemap fill first Ca
#' @name gg_treemap_x_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_x_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a',
                                group = "a", label = 'b'),
                     group.label.colour = "white",
                     label.colour = "white", label.size.factor = 2,
                     group.label.size.threshold = 1) + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean()

  graph
}

#' gg_treemap_y_CaCa.
#' Treemap fill second Ca
#' @name gg_treemap_y_CaCa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_y_CaCa. <- function(data, titleLabel = "", subtitle = "", caption = "", ...){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
                dplyr::group_by(a, b) %>%
                dplyr::summarise(count = n()) %>%
                dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'b',
                                group = "a", label = "b"),
                     group.label.colour = "white",
                     label.colour = "white", label.size.factor = 2,
                     group.label.size.threshold = 1) + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) + scale_fill_manual(values = getPalette()) +
    theme_ds() + theme_ds_clean()

  graph
}
