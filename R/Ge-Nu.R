
#' gg_choropleth_co_GeNu.
#' Choropleth of Colombia's deptos
#' @name gg_choropleth_co_GeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_choropleth_co_GeNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                                   yLabel = NULL, fillLabel = NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_deptos <- suppressMessages(read_csv(system.file("geo/deptos_co.csv", package = "ggmagic"), col_names = TRUE))
  names(data_deptos)[which(names(data_deptos) == "id")] <- "a"
  data_complete <- data.frame(a = unique(data_deptos$a))
  data <- suppressMessages(dplyr::left_join(data_complete, data))

  data_graph <- merge(data, data_deptos, by = "a")
  names(data_graph)[which(names(data_graph) == "a")] <- "id"
  graph <- ggplot(data_graph) +
    geom_map(map = data_graph, aes(map_id = id, x = long, y = lat, group = group, fill = b),
             color="white", size=0.25) + coord_fixed() + theme_minimal() +
    theme_void() + scale_fill_continuous(guide = guide_legend(title = flab))

  options(warn=0)

  return(graph)
}
