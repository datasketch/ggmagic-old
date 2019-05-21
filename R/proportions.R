#' Pie (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_pie_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_pie_CatNum
gg_pie_CatNum <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = ggmagic::agg(opts$agg, b)) %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$nDigits))

  d <- ggmagic::sortSlice(d, "b", "a", "ver", opts$sort, opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", "ver", opts$order, opts$label_wrap)
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, NULL, NULL, opts$label_wrap)

  d$ct <- cumsum(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)]) -
    d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)] / 2
  d <- d %>%
    dplyr::mutate(b = ifelse(b == 0, NA, b),
                  percent = ifelse(percent == 0, NA, percent))

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  gg <- ggplot(d, aes(x = 1, y = b, fill = a)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(y = ct,
                  x = opts$label_ratio,
                  label = paste0(opts$prefix,
                                 format(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)],
                                        big.mark = opts$marks[1],
                                        decimal.mark = opts$marks[2],
                                        digits = opts$nDigits,
                                        nsmall = opts$nDigits),
                                 opts$suffix)),
              check_overlap = TRUE,
              size = opts$text_size,
              color = ifelse(opts$text_show, opts$text_color, "transparent")) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = "", y = "") +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  if (is.null(opts$theme)) {
    gg <- gg +
      ggmagic::tma() +
      theme_ds_clean()
  } else {
    gg <- gg +
      opts$theme +
      theme_ds_clean()
  }

  gg +
    theme_leg() +
    theme(legend.position = opts$legend_position,
          plot.caption = element_text(hjust = 1)) +
    guides(fill = guide_legend(nrow = 1))
}


#' Pie (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_pie_Cat(sampleData("Cat", nrow = 10))
#' @export gg_pie_Cat
gg_pie_Cat <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label))

  gg <- gg_pie_CatNum(data = , opts = opts, ...)
  gg
}



#' Donut (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat-Num, Dat-Num, Yea-Num
#' @examples
#' gg_donut_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_donut_CatNum
gg_donut_CatNum <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  opts$title <-  opts$title %||% ""
  opts$subtitle <- opts$subtitle %||% ""
  opts$caption <- opts$caption %||% ""

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = ggmagic::agg(opts$agg, b)) %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$nDigits))

  d <- ggmagic::sortSlice(d, "b", "a", "ver", opts$sort, opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", "ver", opts$order, opts$label_wrap)
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, NULL, NULL, opts$label_wrap)

  d$ct <- cumsum(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)]) -
    d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)] / 2
  d <- d %>%
    dplyr::mutate(b = ifelse(b == 0, NA, b),
                  percent = ifelse(percent == 0, NA, percent))

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  gg <- ggplot(d, aes(x = 1, y = b, fill = a)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    xlim(c(-0.5, 1.5)) +
    geom_text(aes(y = ct,
                  x = opts$label_ratio,
                  label = paste0(opts$prefix,
                                 format(d[[ifelse(opts$percentage, "percent", "b")]][order(d$a, decreasing = TRUE)],
                                        big.mark = opts$marks[1],
                                        decimal.mark = opts$marks[2],
                                        digits = opts$nDigits,
                                        nsmall = opts$nDigits),
                                 opts$suffix)),
              check_overlap = TRUE,
              size = opts$text_size,
              color = ifelse(opts$text_show, opts$text_color, "transparent")) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, x = "", y = "") +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  if (is.null(opts$theme)) {
    gg <- gg +
      ggmagic::tma() +
      theme_ds_clean()
  } else {
    gg <- gg +
      opts$theme +
      theme_ds_clean()
  }

  gg +
    theme_leg() +
    theme(legend.position = opts$legend_position,
          plot.caption = element_text(hjust = 1)) +
    guides(fill = guide_legend(nrow = 1))
}



#' Donut (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Ggplot2 visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_donut_Cat(sampleData("Cat", nrow = 10))
#' @export gg_donut_Cat
gg_donut_Cat <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label))

  gg <- gg_pie_CatNum(data = , opts = opts, ...)
  gg
}
