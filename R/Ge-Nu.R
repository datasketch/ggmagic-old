
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
gg_choropleth_co_GeNu. <- function(data, titleLabel = "Report",
                                   fillLabel = NULL, leg_pos = "right"){

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

#' gg_choropleth_depto_GeNu.
#' Choropleth by filtering deptos
#' @name gg_choropleth_depto_GeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_choropleth_depto_GeNu. <- function(data, titleLabel = "Report", depto_ = "05",
                                      fillLabel = NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv", package = "ggmagic"), col_names = TRUE))
  names(data_mpios)[which(names(data_mpios) == "id")] <- "a"
  data_mpios <- data_mpios %>% filter(depto == depto_)
  data_complete <- data.frame(a = unique(data_mpios$a))
  data <- suppressMessages(dplyr::left_join(data_complete, data))

  data_graph <- merge(data, data_mpios, by = "a")
  names(data_graph)[which(names(data_graph) == "a")] <- "id"
  graph <- ggplot(data_graph) +
    geom_map(map = data_graph, aes(map_id = id, x = long, y = lat, group = group, fill = b),
             color="white", size=0.25) + coord_fixed() + theme_minimal() +
    theme_void() + scale_fill_continuous(guide = guide_legend(title = flab))

  options(warn=0)

  return(graph)
}

#' gg_point_co_GeNu.
#' Points inside Colombia's deptos map
#' @name gg_point_co_GeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_co_GeNu. <- function(data, titleLabel = "Report",
                              fillLabel = NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flab <- fillLabel %||% nms[3]
  data <- f$d

  options(warn=-1)
  data_deptos <- suppressMessages(read_csv(system.file("geo/deptos_co.csv", package = "ggmagic"), col_names = TRUE))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n())
  graph <- ggplot(data_deptos) +
    geom_map(map = data_deptos,
             aes(map_id = id, x = long, y = lat, group = group),
             color="white", size=0.25) +
    coord_fixed() + theme_minimal() +
    theme_void()

  graph <- graph + geom_point(data = data_graph, aes(x = a, y = b)) + coord_fixed()
  options(warn=0)

  return(graph)
}

#' gg_point_depto_GeNu.
#' Points inside Colombia's mpios map
#' @name gg_point_depto_GeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_depto_GeNu. <- function(data, titleLabel = "Report", depto_ = "05",
                                      fillLabel = NULL, leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flab <- fillLabel %||% nms[2]
  data <- f$d

  options(warn=-1)
  data_mpios <- suppressMessages(read_csv(system.file("geo/mpios_depto_co.csv", package = "ggmagic"), col_names = TRUE))
  data_mpios <- data_mpios %>% filter(depto == depto_)

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n())
  graph <- ggplot(data_mpios) +
    geom_map(map = data_mpios,
             aes(map_id = id, x = long, y = lat, group = group),
             color="white", size=0.25) +
    coord_fixed() + theme_minimal() +
    theme_void()

  graph <- graph + geom_point(data = data_graph, aes(x = a, y = b)) + coord_fixed()

  options(warn=0)

  return(graph)
}
