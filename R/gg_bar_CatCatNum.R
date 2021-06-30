#' Bar Chart Cat Cat Num
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @inherit dsvizopts::dsviz_default_opts
#' @inheritDotParams dsvizopts::dsviz_default_opts
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' gg_bar_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export
gg_bar_CatCatNum <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- as_Cat(data[[1]])
  data[[2]] <- as_Cat(data[[2]])
  data[[3]] <- as_Num(data[[3]])


  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, extra_pattern = ".", plot =  "bar", ftype = "Cat-Cat-Num")
  default_theme <- c(opts$theme, orientation = l$orientation)
  d <- l$d

  ggpos <- "stack"
  if(l$extra$graph_type == "grouped")
    ggpos <- position_dodge2(width = 0.9, preserve = "single")

  #gg <- ggplot(l$d, aes(x = a, y = c, fill = ..colors ))
  gg <- ggplot(l$d, aes(x = b, y = value, fill = a )) +
    geom_bar(stat = "identity", position = ggpos) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         fill = l$titles$legend) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_fill_manual(values=unique(l$d$..colors), labels = l$formats$f_cat)


  if (l$dataLabels$show) {
    if (l$extra$graph_type == "grouped") {
      gg <- gg +
        geom_text(
          aes(label = l$dataLabels$f_nums(l$d$value), y = l$d$value + 0.05),
          position = position_dodge(0.9),
          vjust = 0,
          check_overlap = TRUE,
          size = l$dataLabels$size,
          color = l$dataLabels$color
        )
    } else {
      gg <- gg + geom_text(label = l$dataLabels$f_nums(l$d$value),
                           position = position_stack(vjust = 0.5),
                           check_overlap = TRUE,
                           size = l$dataLabels$size,
                           color = l$dataLabels$color)
    }
  }

  if (l$orientation == "hor")
    gg <- gg + coord_flip()

  gg <- gg + add_ggmagic_theme(default_theme)
  add_branding_bar(gg, opts$theme)

}

#' Bar Chart Cat Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @examples
#' gg_bar_CatCat(sample_data("Cat-Cat", nrow = 10))
#' @export
gg_bar_CatCat <- function(data, ...){

  if (is.null(data)) stop("need dataset to visualize")
  data[[1]] <- as_Cat(data[[1]])
  data[[2]] <- as_Cat(data[[2]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- ggmagic_prep(data, opts, extra_pattern = ".", plot =  "bar", ftype = "Cat-Cat")
  default_theme <- c(opts$theme, orientation = l$orientation)
  d <- l$d

  ggpos <- "stack"
  if(l$extra$graph_type == "grouped")
    ggpos <- position_dodge2(width = 0.9, preserve = "single")

  #gg <- ggplot(l$d, aes(x = a, y = c, fill = ..colors ))
  gg <- ggplot(l$d, aes(x = b, y = value, fill = a )) +
    geom_bar(stat = "identity", position = ggpos) +
    labs(title = l$titles$title,
         subtitle = l$titles$subtitle,
         caption = l$titles$caption,
         x = l$titles$x,
         y = l$titles$y,
         fill = l$titles$legend) +
    scale_y_continuous(labels = l$formats$f_nums) +
    scale_fill_manual(values=unique(l$d$..colors), labels = l$formats$f_cat)


  if (l$dataLabels$show) {
    if (l$extra$graph_type == "grouped") {
      gg <- gg +
        geom_text(
          aes(label = l$dataLabels$f_nums(l$d$value), y = l$d$value + 0.05),
          position = position_dodge(0.9),
          vjust = 0,
          check_overlap = TRUE,
          size = l$dataLabels$size,
          color = l$dataLabels$color
        )
    } else {
      gg <- gg + geom_text(label = l$dataLabels$f_nums(l$d$value),
                           position = position_stack(vjust = 0.5),
                           check_overlap = TRUE,
                           size = l$dataLabels$size,
                           color = l$dataLabels$color)
    }
  }

  if (l$orientation == "hor")
    gg <- gg + coord_flip()

  gg <- gg + add_ggmagic_theme(default_theme)
  add_branding_bar(gg, opts$theme)

}

