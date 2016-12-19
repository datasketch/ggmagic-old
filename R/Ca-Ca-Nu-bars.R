
#' gg_bar_grouped_ver_CaCaNu.
#' vertical unstacked bargraph
#' @name gg_bar_grouped_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right",
                                       aggregation = "mean", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a)


  graph <- ggplot(data, aes(a, weight=c, fill=b)) +
    geom_bar(position = "dodge")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(legend.position=leg_pos) + guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())

  graph
}

#' gg_bar_grouped_hor_CaCaNu.
#' horizontal bar graph
#' @name gg_bar_grouped_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right",
                                       aggregation = "mean",...){

  graph <- gg_bar_grouped_ver_CaCaNu.(data, titleLabel, subtitle, caption,
                                      xLabel, yLabel, leg_pos,
                                      aggregation = aggregation)

  graph + coord_flip()

}

#' gg_bar_grouped2_ver_CaCaNu.: title.
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_grouped2_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
gg_bar_grouped2_ver_CaCaNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_grouped_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' gg_bar_grouped2_hor_CaCaNu.: title.
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_grouped2_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped2_hor_CaCaNu. <- function(data, ...){
  graph <- gg_bar_grouped2_ver_CaCaNu.(data,...) +
    coord_flip()
  graph
}


#' gg_bar_stacked_ver_CaCaNu.
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right",
                                       aggregation = "mean", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a)

  graph <- ggplot(data, aes(a, y = c, fill=b)) + geom_bar(stat="identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette())
  graph
}

#' gg_bar_stacked_hor_CaCaNu.
#' horizontal stacked bar graph
#' @name gg_bar_stacked_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right",
                                       aggregation = "mean", ...){
  graph <- gg_bar_stacked_ver_CaCaNu.(data, titleLabel, subtitle, caption,
                                      xLabel, yLabel, leg_pos,
                                      aggregation = aggregation)
  graph + coord_flip()
}


#' gg_bar_stacked2_ver_CaCaNu.: title.
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_stacked2_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
gg_bar_stacked2_ver_CaCaNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' gg_bar_stacked2_hor_CaCaNu.: title.
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_stacked2_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked2_hor_CaCaNu. <- function(data, ...){
  graph <- gg_bar_stacked2_ver_CaCaNu.(data,...) +
    coord_flip()
  graph
}



#' gg_bar_stacked_100_ver_CaCaNu.
#' 100 vertical stacked bar graph
#' @name gg_bar_stacked_100_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos = "right",
                                           aggregation = "mean", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a)

  graph <- ggplot(data, aes(a, y = c, fill=b)) +
    geom_bar(stat="identity", position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds()  + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent)

  graph
}

#' gg_bar_stacked_100_hor_CaCaNu.
#' 100 horizontal stacked bar graph
#' @name gg_bar_stacked_100_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos = "right",
                                           aggregation = "mean", ...){


  graph <- gg_bar_stacked_100_ver_CaCaNu.(data, titleLabel, subtitle, caption,
                                          xLabel, yLabel, leg_pos,
                                          aggregation = aggregation)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_stacked2_100_ver_CaCaNu.: title.
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_stacked2_100_ver_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
gg_bar_stacked2_100_ver_CaCaNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' gg_bar_stacked2_100_hor_CaCaNu.: title.
#' Barras grouped
#' Tiene múltiples líneas
#' @name gg_bar_stacked2_100_hor_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked2_100_hor_CaCaNu. <- function(data, ...){
  graph <- gg_bar_stacked2_100_ver_CaCaNu.(data,...) +
    coord_flip()
  graph
}




#' gg_bar_facet_ver_CaCaNu.
#' Facet vertical bargraph
#' @name gg_bar_facet_ver_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_ver_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL,
                                     yLabel = NULL, leg_pos = "right", angle_x = 45,
                                     aggregation = "mean",
                                     fillMissing = TRUE, fillMissingValue = 0,
                                     groupLabelPosition = "top",
                                     ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xLabel <- xLabel %||% nms[1]
  yLabel <- yLabel %||% nms[3]
  data <- f$d

  if(groupLabelPosition == "top")
    groupLabelPosition <- NULL
  else
    groupLabelPosition <- "both"

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c))

  if(fillMissing){
    data <- data %>%
      tidyr::spread(b, c, fill = fillMissingValue) %>%
      tidyr::gather(b, c, -a)
  }

  graph <- ggplot(data, aes(a, weight=c)) +
    geom_bar(position = "dodge", aes(fill = ""), show.legend = FALSE)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption,
         x = xLabel, y = yLabel) +
    theme(legend.position=leg_pos) + guides(text = FALSE) + facet_grid(. ~a)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph + facet_grid(. ~b,scale="free_x", space="free",switch = groupLabelPosition)
}


#' gg_bar_facet_hor_CaCaNu.
#' Horizontal bar graph
#' @name gg_bar_facet_hor_CaCaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_hor_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, leg_pos = "right", angle_x = 0,
                                     aggregation = "mean",
                                     fillMissing = TRUE, fillMissingValue = 0,
                                     groupLabelPosition = "right",
                                     ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xLabel <- xLabel %||% nms[1]
  yLabel <- yLabel %||% nms[3]
  data <- f$d

  if(groupLabelPosition == "right")
    groupLabelPosition <- NULL
  else
    groupLabelPosition <- "both"

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c))

  if(fillMissing){
    data <- data %>%
      tidyr::spread(b, c, fill = fillMissingValue) %>%
      tidyr::gather(b, c, -a)
  }

  graph <- ggplot(data, aes(a, weight=c)) +
    geom_bar(position = "dodge", aes(fill = ""), show.legend = FALSE)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption,
         x = xLabel, y = yLabel) +
    theme(legend.position=leg_pos) + guides(text = FALSE) + facet_grid(. ~a)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_grid(b~.,scale="free_y", space="free",switch = groupLabelPosition) +
    coord_flip()
  graph
}





#' gg_bar_coloured_ver_x_facet_CaCaNu.
#' vertical bar
#' @name gg_bar_coloured_ver_x_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_x_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = a, weight = c, fill = factor(a))) +
    geom_bar() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_grid(. ~b)

  graph
}


#' gg_bar_coloured_hor_x_facet_CaCaNu.
#' horizontal bar
#' @name gg_bar_coloured_hor_x_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_x_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_coloured_ver_x_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_coloured_ver_y_facet_CaCaNu.
#' vertical bar
#' @name gg_bar_coloured_ver_y_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_y_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  graph <- ggplot(data, aes(x = a, weight = c, fill = factor(b))) +
    geom_bar() +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_grid(. ~b)

  graph
}


#' gg_bar_coloured_hor_y_facet_CaCaNu.
#' horizontal bar
#' @name gg_bar_coloured_hor_y_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_y_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos = "right", ...){

  graph <- gg_bar_coloured_ver_y_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_coloured_ver_z_facet_CaCaNu.
#' Facet coloured vertical bar
#' @name gg_bar_coloured_ver_z_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_z_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, reverse = FALSE, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(suma=sum(c))

  graph <- ggplot(data_graph, aes(x = a, y = suma, fill = suma)) +
    geom_bar(stat = "identity") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds()

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }

  graph <- graph + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid(. ~b)

  graph
}

#' gg_bar_coloured_hor_z_facet_CaCaNu.
#' Facet Coloured horizontal bar
#' @name gg_bar_coloured_hor_z_facet_CaCaNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_z_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, reverse = FALSE, leg_pos = "right", ...){

  graph <- gg_bar_coloured_ver_z_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, reverse, leg_pos)
  graph <- graph + coord_flip()

  graph
}

#' gg_bar_coloured_parameter_ver_facet_CaCaNu.
#' Facet Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                        xLabel = NULL, yLabel = NULL,
                                                        parameter1 = NULL, parameter2 = NULL,
                                                        leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  p_a <-  parameter1 %||% sample(unique(data[,nms[1]]), length(unique(data[,nms[2]])))
  p_b <-  parameter2 %||% sample(unique(data[,nms[2]]), length(unique(data[,nms[2]])))
  data <- f$d
  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = sum(c))

  list_df <- apply(cbind(p_a, p_b), 1, function(x){
    df <- data_graph%>% mutate(color = ifelse(a==x[1] & b==x[2], TRUE, FALSE))
    df[df$color,]
  })
  df <- bind_rows(list_df)
  data_graph <- left_join(data_graph, df, by = c("a", "b", "count"))
  data_graph[is.na(data_graph)] <- FALSE

  graph <- ggplot(data_graph, aes(a, weight = count)) +
    geom_bar(position ="dodge", aes(fill =  color %in% TRUE))
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph +
    guides(fill=FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph +
    theme_minimal() +
    theme(legend.position=leg_pos) +
    facet_grid(.~b)

  graph
}

#' gg_bar_coloured_parameter_hor_facet_CaCaNu.
#' Facet Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                        xLabel = NULL, yLabel = NULL,
                                                        parameter1 = NULL, parameter2 = NULL,
                                                        leg_pos = "right", ...){

  graph <- gg_bar_coloured_parameter_ver_facet_CaCaNu.(data, titleLabel, subtitle, caption, xLabel,
                                                       yLabel, parameter1, parameter2, leg_pos)

  graph <- graph + coord_flip()
  graph
}









#' gg_bar_circular_facet_CaCaNu.
#' Circular Bar
#' @name gg_bar_circular_facet_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_facet_CaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          leg_pos="right", width = 0.85, ...){

  f <- fringe(data)
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = c , fill = a )) +
    geom_bar(width = width, stat="identity") +
    coord_polar(theta = "y")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "") +
    scale_fill_manual(values = getPalette()) +
    theme_ds() +
    theme_ds_clean()

  graph <- graph + theme(legend.position=leg_pos) + facet_grid(. ~b)

  graph
}



#' gg_bar_stacked_polar_CaCaNu.
#' Stacked Polar Bar
#' @name gg_bar_stacked_polar_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_CaCaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                                         leg_pos= "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  data <- f$d

  graph <- ggplot(data = data, aes(x = a, weight = c, fill = b)) +
    geom_bar(width = width, position = "stack") +
    coord_polar()
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = "", y = "")
  graph <- graph +
    theme(legend.position=leg_pos) +
    theme_ds() +
    theme_ds_clean() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos)

  graph
}

#' gg_bar_stacked_polar_100_CaCaNu.
#' Stacked Polar Bar 100
#' @name gg_bar_stacked_polar_100_CaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_polar_100_CaCaNu. <- function(data, width = 0.95, titleLabel = "", subtitle = "", caption = "",
                                             fillLabel = NULL, leg_pos= "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data = data, aes(x = a, weight = c, fill = b)) +
    geom_bar(width = width, position = "fill") +
    coord_polar() + theme(legend.position=leg_pos) + theme_ds() +
    theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption)

  graph
}
