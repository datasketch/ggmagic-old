#' #' Bubbles Cat Num
#' #'
#' #' @param data
#' #'
#' #' @export
#' hgch_bubbles_CatNum <- function(data = NULL,
#'                                 opts = NULL, ...) {
#'
#'   if (is.null(data)) {
#'     stop("Load an available dataset")
#'   }
#'   data <- sampleData('Cat-Num')
#'   opts <- getOptions(opts = opts)
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   if (opts$color_scale == 'discrete') {
#'     colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
#'     colorDefault <- ggmagic::discreteColorSelect(colorDefault, d)
#'   } else if (opts$color_scale == "no"){
#'     colorDefault <- rep("#3DB26F", length(unique(d$a)))
#'   } else {
#'     colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
#'   }
#'
#'
#'   if (!is.null(opts$colors)) {
#'     opts$colors <- ggmagic::fillColors(d, "a", opts$colors, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)
#'   } else {
#'     opts$colors <- ggmagic::fillColors(d, "a", colorDefault, opts$color_scale, opts$highlight_value, opts$highlight_valueColor, opts$label_wrap)
#'   }
#'
#'   if (opts$drop_na)
#'     d <- d %>%
#'     tidyr::drop_na()
#'
#'   d <- d  %>%
#'     tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
#'                            b = NA)) %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(b = agg(opts$agg, b)) %>%
#'     dplyr::mutate(id = 1:n(), categoria = a)
#'
#'   d$a <- as.character(d$a)
#'   d$a[is.na(d$a)] <- 'NA'
#'
#'   d <- ggmagic::orderCategory(d, "a", 'ver', opts$order, opts$label_wrap)
#'   d <- ggmagic::sortSlice(d, "b", "a", 'ver', opts$sort, opts$sliceN)
#'
#'
#'   d$z <- rescale(d$b, to = c(5, 30))
#'   ncircles <- dim(d)[1]
#'   limits <- c(-100 , 100)
#'   inset <- diff(limits) / 3
#'
#'
#'   if (is.null(opts$nDigits)) {
#'     nDig <- 0
#'   } else {
#'     nDig <- opts$nDigits
#'   }
#'
#'   if (opts$percentage) {
#'     d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
#'   }
#'
#'   d$b <- round(d$b, nDig)
#'
#'   set.seed(7321)
#'   xyr <- data.frame(
#'     x = runif(ncircles, min(limits) + inset, max(limits) - inset),
#'     y = runif(ncircles, min(limits) + inset, max(limits) - inset),
#'     r = (d$z)) %>% arrange(desc(r))
#'
#'   res <- circleLayout(xyr, limits, limits, maxiter = 1000)
#'
#'   dat.after <- circlePlotData(res$layout)
#'
#'
#'   fi <- data.frame(id = 1:dim(d)[1], categoria = d$a)
#'   fi <- inner_join(d, dat.after)
#'
#'   cent <- fi %>% dplyr::group_by(categoria) %>%
#'     dplyr::summarise(x = mean(x), y = mean(y))
#'
#'
#'   gg <- ggplot(fi) +
#'     geom_polygon(aes(x, y, group=id, fill = categoria)) +
#'     scale_fill_manual(values = opts$colors) +
#'     labs(title = opts$title, subtitle = opts$subtitle, caption = opts$caption, fill = " ")
#'     #coord_equal(xlim=limits, ylim=limits)
#'   if (is.null(opts$theme)) {
#'     gg <- gg + ggmagic::tma() + theme_ds_clean()
#'   } else {
#'     gg <- gg + opts$theme
#'   }
#'
#' gg
#'
#' }
