#' Vertical area (ordered categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Oca
#' @examples
#' gg_area_Oca(sampleData("Cat", nrow = 10))
#' @export gg_area_Oca
gg_area_Oca <- function(data,
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        horLabel = NULL,
                        verLabel = NULL,
                        yLine = NULL,
                        yLineLabel = NULL,
                        dropNa = FALSE,
                        order = NULL,
                        theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% paste("count", nms[1])
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  order[is.na(order)] <- "NA"
  order <- union(order, unique(d$a))
  order <- unique(d$a)[order(match(unique(d$a), order))]

  gg <- ggplot(d, aes(x = as.character(a), y = b, group = 1)) +
    geom_area(aes(fill = "", alpha = .87), show.legend = FALSE) +
    geom_line(aes(colour = ""), show.legend = FALSE) +
    geom_point(aes(colour = ""), show.legend = FALSE) +
    geom_hline(yintercept = ifelse(is.null(yLine), 0, yLine),
               color = ifelse(is.null(yLine), "transparent", "black"),
               linetype = "dashed",
               size = 1) +
    labs(title = title, subtitle = subtitle, caption = caption, x = horLabel, y = verLabel) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(limits = as.character(order)) +
    scale_fill_manual(values = getPalette()) +
    scale_colour_manual(values = getPalette()) +
    theme_ds()
  gg
}

#' Vertical area (ordered categories, numbers)
#'
#' Compare quantities among categories
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' gg_area_OcaNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_area_OcaNum
gg_area_OcaNum <- function(data,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           horLabel = NULL,
                           verLabel = NULL,
                           yLine = NULL,
                           yLineLabel = NULL,
                           agg = "sum",
                           dropNa = FALSE,
                           order = NULL,
                           percentage = FALSE,
                           theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||%  ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  if (percentage) {
    d <- d %>%
      dplyr::mutate(b = b / sum(b))
    verLabel <- paste("%", verLabel)
  }

  order[is.na(order)] <- "NA"
  order <- union(order, unique(d$a))
  order <- unique(d$a)[order(match(unique(d$a), order))]

  gg <- ggplot(d, aes(x = as.character(a), y = b, group = 1)) +
    geom_area(aes(fill = "", alpha = .87), show.legend = FALSE) +
    geom_line(aes(colour = ""), show.legend = FALSE) +
    geom_point(aes(colour = ""), show.legend = FALSE) +
    geom_hline(yintercept = ifelse(is.null(yLine), 0, yLine),
               color = ifelse(is.null(yLine), "transparent", "black"),
               linetype = "dashed",
               size = 1) +
    labs(title = title, subtitle = subtitle, caption = caption, x = horLabel, y = verLabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = as.character(order)) +
    scale_fill_manual(values = getPalette()) +
    scale_colour_manual(values = getPalette()) +
    theme_ds()
  if (percentage) gg <- gg +
    scale_y_continuous(labels = percent)
  gg
}


#' Vertical area (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' gg_area_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export gg_area_YeaNum
gg_area_YeaNum <- gg_area_OcaNum


#' Vertical area (dates, numbers)
#'
#' Compare quantities over dates
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Dat-Num
#' @examples
#' gg_area_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export gg_area_DatNum
gg_area_DatNum <- gg_area_OcaNum


#' Vertical stacked area (categories, ordered categories)
#'
#' Compare stacked categories
#'
#' @param data A data.frame
#' @return Ggplo visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' gg_area_stacked_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export gg_area_stacked_CatOca
gg_area_stacked_CatOca <- function(data,
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   horLabel = NULL,
                                   verLabel = NULL,
                                   yLine = NULL,
                                   yLineLabel = NULL,
                                   dropNa = FALSE,
                                   order = NULL,
                                   theme = NULL,
                                   export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% "count"
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- 0

  order <- union(order, unique(d$b))
  order[is.na(order)] <- "NA"
  order <- unique(d$b)[order(match(unique(d$b), order))]

  gg <- ggplot(d, aes(x = b, y = c, group = a)) +
    geom_area(aes(fill = a), position = "stack") +
    geom_hline(yintercept = ifelse(is.null(yLine), 0, yLine),
               color = ifelse(is.null(yLine), "transparent", "black"),
               linetype = "dashed",
               size = 1) +
    guides(text = FALSE) +
    labs(title = title, subtitle = subtitle, caption = caption, x = horLabel, y = verLabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = as.character(order)) +
    scale_fill_manual(values = getPalette()) +
    scale_colour_manual(values = getPalette()) +
    theme_ds()
  gg
}


#' Vertical %100 stacked area (categories, ordered categories)
#'
#' Compare %100 stacked categories
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' gg_area_stacked_100_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export gg_area_stacked_100_CatOca
gg_area_stacked_100_CatOca <- function(data,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       horLabel = NULL,
                                       verLabel = NULL,
                                       yLine = NULL,
                                       yLineLabel = NULL,
                                       dropNa = FALSE,
                                       order = NULL,
                                       theme = NULL,
                                       export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% "count"
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- 0

  order <- union(order, unique(d$b))
  order[is.na(order)] <- "NA"
  order <- unique(d$b)[order(match(unique(d$b), order))]

  gg <- ggplot(d, aes(x = b, y = c, group = a)) +
    geom_area(aes(fill = a), position = "fill") +
    geom_hline(yintercept = ifelse(is.null(yLine), 0, yLine),
               color = ifelse(is.null(yLine), "transparent", "black"),
               linetype = "dashed",
               size = 1) +
    guides(text = FALSE) +
    labs(title = title, subtitle = subtitle, caption = caption, x = horLabel, y = verLabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(limits = as.character(order)) +
    scale_fill_manual(values = getPalette()) +
    scale_colour_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme_ds()
  gg
}


#' Filled density distribution (categories, numbers)
#'
#' Compare categories distributions
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Num
#' @examples
#' gg_area_density_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_area_density_CatNum
gg_area_density_CatNum <- function(data,
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   horLabel = NULL,
                                   verLabel = NULL,
                                   yLine = NULL,
                                   yLineLabel = NULL,
                                   dropNa = FALSE,
                                   theme = NULL,
                                   export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% "density"
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA)))

  gg <- ggplot(d, aes(x = b)) +
    geom_density(aes(fill = a, alpha = .64)) +
    geom_hline(yintercept = ifelse(is.null(yLine), 0, yLine),
               color = ifelse(is.null(yLine), "transparent", "black"),
               linetype = "dashed",
               size = 1) +
    guides(text = FALSE) +
    labs(title = title, subtitle = subtitle, caption = caption, x = horLabel, y = verLabel) +
    scale_fill_manual(values = getPalette()) +
    theme_ds()
  gg
}



#' Vertical area facet
#' Facet Vertical Area
#' @name gg_area_ver_facet_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_ver_facet_CatNum(sampleData("Cat-Num"))
gg_area_ver_facet_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data = data_count, aes(x=xorder, y=b, group=a)) + geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Horizontal area facet
#' Facet Horizontal Area
#' @name gg_area_hor_facet_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_hor_facet_CatNum(sampleData("Cat-Num"))
gg_area_hor_facet_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, angle_x = 0, ...) {

  graph <- gg_area_ver_facet_CatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}




#' Vertical stacked area
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CatDatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_area_stacked_ver_CatDatNum(sampleData("Cat-Dat-Num"))
gg_area_stacked_ver_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, aggregation = "sum",
                                          leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    tidyr::spread(b, count) %>%
    tidyr::gather(b, count, -a)

  data_graph$b <- lubridate::as_date(data_graph$b)
  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes( b, count))
  graph <- graph + geom_area(aes(fill= a), position = 'stack')
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values = getPalette())
  graph
}

#' Horizontal stacked area
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CatDatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_area_stacked_hor_CatDatNum(sampleData("Cat-Dat-Num"))
gg_area_stacked_hor_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, aggregation = "sum",
                                          leg_pos = "right", angle_x = 0,...){

  graph <- gg_area_stacked_ver_CatDatNum(data, titleLabel, subtitle, caption, xLabel, yLabel,
                                         fillLabel, aggregation, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CatDatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_area_stacked_100_ver_CatDatNum(sampleData("Cat-Dat-Num"))
gg_area_stacked_100_ver_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, fillLabel = NULL, aggregation = "sum",
                                              leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste("%", nms[3])
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(percent = 100 * round(count / sum(count), 4))
  # data_graph <- data %>%
  #   dplyr::group_by(a, b) %>%
  #  # dplyr::mutate(percent = 100 * round(c / sum(c), 4)
  #   tidyr::spread(b, c) %>%
  #   tidyr::gather(b, c, -a) %>%
  #   dplyr::summarise(percent = 100 * round(c / sum(c), 4))

  # data_graph <- data %>%
  #   tidyr::drop_na(a,b) %>%
  #   tidyr::spread(b, c) %>%
  #   tidyr::gather(b, c, -a)
  data_graph$b <- lubridate::as_date(data_graph$b)
  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes(b, percent))
  graph <- graph + geom_area(aes(fill= a), position = 'stack')
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values = getPalette())
  graph

  #
  #
  # data_graph$b <- lubridate::as_date(data_graph$b)
  # data_graph[is.na(data_graph)] <- 0
  #
  # graph <- ggplot(data = data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a)) +
  #   geom_area(aes(fill = a), position = "fill")
  # graph <- graph +
  #   labs(title = titleLabel, subtitle = subtitle, caption =  caption, x = xlab, y = ylab, fill = clab)
  # graph <- graph +
  #   theme_ds() +
  #   theme(legend.position=leg_pos) +
  #   scale_y_continuous(labels = percent) +
  #   theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
  #   scale_fill_manual(values = getPalette())
  # graph
}

#' Horizontal 100% stacked area
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CatDatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL,
                                               leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_ver_CatDatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}


#' Vertical stacked area
#' Stacked vertical Area
#' @name gg_area_stacked_ver_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_area_stacked_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_area_stacked_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

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
    dplyr::summarise(count=agg(aggregation, c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "stack")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())

  graph
}

#' Horizontal stacked area
#' Stacked area
#' @name gg_area_stacked_hor_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,...){


  graph <- gg_area_stacked_ver_CatCatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked vertical Area 100
#' @name gg_area_stacked_100_ver_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste("%", aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=agg(aggregation,c)) %>%
    tidyr::spread(b, count) %>%
    tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "fill")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal 100% stacked area
#' Stacked area 100
#' @name gg_area_stacked_100_hor_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){


  graph <- gg_area_stacked_100_ver_CatCatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}
