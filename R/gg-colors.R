
gg_data_color <- function(data, opts, viz) {
  if ("..colors" %in% names(data)) return(data)
  color_palette_type <- opts$color_palette_type

  if (is.null(color_palette_type)) color_palette_type <- "categorical"
  color_palette <- opts[[paste0("color_palette_", color_palette_type)]]
  if (is.null(color_palette)) color_palette <- c("#385573", "#ffa92a", "#f06142", "#99e8b3", "#32a8ce", "#996295", "#e59fd7")
  color_by <- opts$color_by

  if (is.null(color_by)) {
    data$..colors <- rep(color_palette[1], nrow(data))
  } else {
    color_var_length <- length(unique(data[[color_by]]))
    d_colors <- data.frame(a = unique(data[[color_by]]))
    names(d_colors) <- color_by

    if ( length(color_palette) < color_var_length) {
      d_colors$..colors <- generate_colors(color_palette, color_var_length)
    } else {
      d_colors$..colors <- color_palette[1:nrow(d_colors)]
    }
    data <- left_join(data, d_colors, by = color_by)
  }
  data
}


gg_color <- function(gg, opts, data, viz) {
  color_palette_type <- opts$color_palette_type
  if (is.null(color_palette_type)) color_palette_type <- "categorical"
  color_palette <- opts[[paste0("color_palette_", color_palette_type)]]
  color_var_length <- NULL
  color_by <- opts$color_by
  if (is.null(color_by)) {
    if (viz %in% c("scatter", "treemap")) {
      l_color <- find_extreme_colors(color_palette)
      gg <- gg + scale_color_gradient(low = l_color$low, high = l_color$high)
    } else if (viz == "bar") {
      gg <- gg + scale_fill_identity()
    } else {
      gg <- gg + scale_color_manual(color_palette)
    }
  } else {
    if (is.character(data[[color_by]]) | is.factor(data[[color_by]])) {
      if (viz == "bar") {
        if (opts$bar_graph_type != "basic") {
          gg <- gg + scale_fill_manual(values = unique(data$..colors))
        } else {
          gg <- gg + scale_fill_identity()
        }
      } else {
        gg <- gg + scale_color_manual(unique(data$..colors))
      }
    } else {
      l_color <- find_extreme_colors(color_palette)
      gg <- gg + scale_color_gradient(low = l_color$low, high = l_color$high)
    }
  }
  gg
}
