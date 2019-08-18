#' Treemap (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' gg_treemap_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export gg_treemap_CatNum
gg_treemap_CatNum <- function(data = NULL, opts = NULL, ...) {

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
    dplyr::summarise(b = ggmagic::agg(opts$agg, b))  %>%
    dplyr::mutate(percent = round(b * 100 / sum(b, na.rm = TRUE), opts$nDigits))

  d <- ggmagic::sortSlice(d, "b", "a", "ver", "desc", opts$sliceN)
  d <- ggmagic::orderCategory(d, "a", "ver", unique(d$a), opts$label_wrap)
  fillCol <- fillColors(data = d, "a", colors = opts$colors, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "b")

  gg <- ggplot(d, aes(area = c,
                     fill = a,
                     label =  paste0(d$a, "\n", paste0(opts$prefix,
                                                       format(d[[varP]],
                                                              big.mark = opts$marks[1],
                                                              decimal.mark = opts$marks[2],
                                                              digits = opts$nDigits),
                                                       opts$suffix)))) +
    treemapify::geom_treemap() +
    geom_treemap_text(min.size = 0, colour = ifelse(opts$text_show, opts$text_colorV[1], "transparent"), size = opts$text_sizeV[1]) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption) +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma()
  } else {
    gg <- gg + opts$theme
  }

  gg +
    theme(legend.position = ifelse(opts$legend_show, opts$legend_position, "none"),
          plot.caption = element_text(hjust = 1)) +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE)) +
    theme_leg()
}

#' Treemap (categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' gg_treemap_Cat(sampleData("Cat", nrow = 10))
#' @export gg_treemap_Cat
gg_treemap_Cat <-  function(data = NULL, opts = NULL, ...) {

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
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label[1]))

  gg <- gg_treemap_CatNum(data = d, opts = opts)
  gg
}



#' Treemap (categories, categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num, Cat-Dat-Num,
#' @examples
#' gg_treemap_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export gg_treemap_CatCatNum
gg_treemap_CatCatNum <- function(data = NULL, opts = NULL, ...) {

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

  if (any(opts$drop_naV))
    d <- d %>%
    tidyr::drop_na(which(opts$drop_naV))

  opts$nDigits <- ifelse(!is.null(opts$nDigits), opts$nDigits, 0)

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = ggmagic::agg(opts$agg, c))  %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(percent = round(c * 100 / sum(c, na.rm = TRUE), opts$nDigits)) %>%
    drop_na(c)

  d <- ggmagic::orderCategory(d, "a", "ver", unique(d$a), opts$label_wrapV[1])
  d <- ggmagic::orderCategory(d, "b", "ver", unique(d$b), opts$label_wrapV[2])
  fillCol <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, NULL, NULL, opts$label_wrap)

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  varP <- ifelse(opts$percentage, "percent", "c")

  gg <- ggplot(d, aes(area = c, fill = a, subgroup = b, label =  paste0(d$a, "\n", paste0(opts$prefix,
                                                                                          format(d[[varP]],
                                                                                                 big.mark = opts$marks[1],
                                                                                                 decimal.mark = opts$marks[2],
                                                                                                 digits = opts$nDigits),
                                                                                          opts$suffix)))) +
    treemapify::geom_treemap() +
    geom_treemap_subgroup_border(color = opts$group_color) +
    geom_treemap_subgroup_text(place = "topleft",  colour = opts$text_colorV[2], min.size = 0, reflow = T, size = opts$text_sizeV[2]) +
    geom_treemap_text(colour = opts$text_colorV[1], place = "bottomleft", min.size = 0, size = opts$text_sizeV[2]) +
    labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption) +
    scale_fill_manual(values = fillCol, name = opts$legend_title)

  if (is.null(opts$theme)) {
    gg <- gg + ggmagic::tma()
  } else {
    gg <- gg + opts$theme
  }

  gg +
    theme(legend.position = ifelse(opts$legend_show, opts$legend_position, "none"),
          plot.caption = element_text(hjust = 1)) +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE)) +
    theme_leg()
}



#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Ggplot visualization
#' @section ctypes:
#' Cat-Cat, Cat-Yea, Cat-Dat,
#' @examples
#' gg_treemap_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export gg_treemap_CatCat
gg_treemap_CatCat <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))

  gg <- gg_treemap_CatCatNum(data = d, opts = opts)
  gg
}




#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP, Yea-NumP, Dat-NumP
#' @examples
#' gg_treemap_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export gg_treemap_CatNumP

gg_treemap_CatNumP <- function(data = NULL, opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  d <- d %>%
    gather("categories", "count", names(d)[-1])

  gg <- gg_treemap_CatCatNum(data = d, opts = opts)
  gg
}
