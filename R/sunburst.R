#' Sunburst
#' sunburst
#' @name gg_sunburst_CatCatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_sunburst_CatCatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", aggregation = "sum",
                                      fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right', ...){



  f <- fringe(data)
  nms <- getClabels(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c)) %>%
    dplyr::filter(!is.na(d))

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
    dplyr::summarise(total1 = agg(aggregation, d)) %>%
    dplyr::mutate(running = cumsum(total1), pos = running - total1/2) %>%
    dplyr::group_by(1:n()) %>%
    dplyr::mutate(angle = pred_ang((pos)/total1)) %>%
    dplyr::arrange(a, -total1)

  sunb0 <- ggplot(part1)
  sunb1 <- sunb0 +
    geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle), check_overlap = TRUE) +
    scale_fill_continuous(low = '#009EE3', high = '#E5007D')

  #segundo nivel

  part2 <- data %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(total1 = agg(aggregation, d)) %>%
    ungroup() %>%
    dplyr::group_by(a) %>%
    dplyr::arrange(a,-total1) %>%
    ungroup() %>%
    mutate(running = cumsum(total1), pos = running - total1/2) %>%
    group_by(1:n()) %>%
    mutate(angle = pred_ang((running - total1/2)/total1))


  sunb2 <- sunb1 +
    geom_bar(data = na.omit(part2), aes(x=2, y = total1,  fill = total1),na.rm = TRUE,stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle), check_overlap = TRUE)



  #tercer nivel

  part3 <- data %>%
    #tidyr::drop_na(c) %>%
    dplyr::group_by(a,b,c) %>%
    dplyr::summarise(total1 = agg(aggregation, d)) %>%
    dplyr::arrange(a,c,-total1) %>%
    ungroup() %>%
    mutate(running = cumsum(total1), pos = running - total1/2) %>%
    group_by(1:n()) %>%
    mutate(angle = pred_ang((running - total1/2)/total1))

  part3$total1[is.na(part3$c)] <- NA


  graph <- sunb2 +
    geom_bar(data = part3, aes(x=3, y = total1,  fill = total1), stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part3, aes(label=part3$c, x=3, y=pos, angle=angle), check_overlap = TRUE)

  graph +
    coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)

}


#' Sunburst
#' sunburst
#' @name gg_sunburst_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_sunburst_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                   fillLabel = NULL, aggregation = "sum", ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

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
    dplyr::summarise(total1 = agg(aggregation, c))



  part1 <- part1 %>%
    mutate(running = cumsum(total1), pos = running - total1/2) %>%
    group_by(1:n()) %>%
    mutate(angle = pred_ang((pos)/total1))

  sunb0 <- ggplot(part1)
  sunb1 <- sunb0 +
    geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle), check_overlap = TRUE) +
    scale_fill_continuous(low = '#009EE3', high = '#E5007D')

  #segundo nivel

  cols_col <- data %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(total1 = agg(aggregation, c))


  part2 <- cols_col %>%
    ungroup(a,b) %>%
    mutate(running = cumsum(total1), pos = running - total1/2) %>%
    group_by(1:n()) %>%
    mutate(angle = pred_ang((running - total1/2)/total1))

  sunb2 <- sunb1 +
    geom_bar(data = part2, aes(x=2, y = total1,  fill = total1),stat = 'identity', color = 'white', position = 'stack') +
    geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle), check_overlap = TRUE)


  graph <- sunb2 + coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)

  graph

}


