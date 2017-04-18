#' Radar
#' point
#' @name gg_radar_CatNumP.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-NumP
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_radar_CatNumP. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            leg_pos="right",rescale = FALSE,
                            aggregation = "mean",...){
  f <- fringe(data)
  nms <- getClabels(f)
  data <- f$data

  data <- data %>%
    dplyr::select(group = 1,everything())

  data <- data %>% group_by(group) %>%
    dplyr::summarise_each(funs(agg(aggregation,.)))

  # if(rescale){
  #   data <- data %>%
  #     mutate_each(funs(rescale), -1)
  # }

  data <- data %>%
        mutate_each(funs(rescale), -1)

  graph <- ggradar(data) +
    scale_color_manual(values = getPalette()) + theme_ds() + theme_ds_clean() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption) +
    theme(legend.position=leg_pos)
  graph
}
