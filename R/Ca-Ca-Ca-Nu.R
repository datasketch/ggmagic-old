
#' gg_treemap_x_CaCaCaNu.
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
gg_treemap_x_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'a', group = "b", label = 'c'),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = flabel)

  graph
}

#' gg_treemap_y_CaCaCaNu.
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
gg_treemap_y_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[2]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'b', group = "a", label = "c"),
                     group.label.colour = "black", label.colour = "black") + #guides(fill=FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, fill = flabel)

  graph
}

#' gg_treemap_z_CaCaCaNu.
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
gg_treemap_z_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a, b, c) %>%
    dplyr::summarise(Sum = sum(d)) %>%
    dplyr::arrange(desc(Sum))

  data_graph$a <- as.factor(data_graph$a)
  data_graph$b <- as.factor(data_graph$b)
  data_graph$c <- as.factor(data_graph$c)

  graph <- ggplotify(treemapify(data_graph, area = "Sum", fill = 'c', group = "a", label = "b"),
                     group.label.colour = "black", label.colour = "black") +
    labs(title = titleLabel, subtitle = subtitle, caption = caption)

  graph
}


#' gg_bar_stacked_100_hor_CaCaCaNu.
#' Stacked
#' @name gg_bar_stacked_100_hor_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Ca-Nu, Ca-Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)


gg_bar_stacked_100_hor_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
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




#' gg_sunburst_CaCaCaNu.
#' sunburst
#' @name gg_sunburst_CaCaCaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_sunburst_CaCaCaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                  fillLabel = NULL, xlab = NULL, ylab = NULL,leg_pos = 'right', ...){



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
           dplyr::summarise(total1 = sum(d)) %>%
           dplyr::mutate(running = cumsum(total1), pos = running - total1/2) %>%
           dplyr::group_by(1:n()) %>%
           dplyr::mutate(angle = pred_ang((pos)/total1)) %>%
           dplyr::arrange(a, -total1)

  sunb0 <- ggplot(part1)
  sunb1 <- sunb0 +
           geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
           geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle)) +
           scale_fill_continuous(low = '#009EE3', high = '#E5007D')

  #segundo nivel

  part2 <- data %>%
           dplyr::group_by(a,b) %>%
           dplyr::summarise(total1 = sum(d)) %>%
           ungroup() %>%
           dplyr::group_by(a) %>%
           dplyr::arrange(a,-total1) %>%
           ungroup() %>%
           mutate(running = cumsum(total1), pos = running - total1/2) %>%
           group_by(1:n()) %>%
           mutate(angle = pred_ang((running - total1/2)/total1))


  sunb2 <- sunb1 +
           geom_bar(data = na.omit(part2), aes(x=2, y = total1,  fill = total1),na.rm = TRUE,stat = 'identity', color = 'white', position = 'stack') +
           geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle))



  #tercer nivel

  part3 <- data %>%
           #tidyr::drop_na(c) %>%
           dplyr::group_by(a,b,c) %>%
           dplyr::summarise(total1 = sum(d)) %>%
           dplyr::arrange(a,c,-total1) %>%
           ungroup() %>%
           mutate(running = cumsum(total1), pos = running - total1/2) %>%
           group_by(1:n()) %>%
           mutate(angle = pred_ang((running - total1/2)/total1))

 part3$total1[is.na(part3$c)] <- NA


  graph <- sunb2 +
    geom_bar(data = part3, aes(x=3, y = total1,  fill = total1), stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part3, aes(label=part3$c, x=3, y=pos, angle=angle))

  graph +
  coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE)

}
