gg_color <- function(gg, opts, data, viz) {
  if (is.null(opts$color_palette_type)) return(gg)
  color_palette <- opts[[paste0("color_palette_", opts$color_palette_type)]]
color_palette
  color_var_length <- NULL
  if (is.null(opts$color_by)) {
    if (viz %in% c("scatter", "treemap")) {
      l_color <- find_extreme_colors(color_palette)
      gg <- gg + scale_color_gradient(low = l_color$low, high = l_color$high)
    } else if (viz == "bar") {
      print("hola")
      gg <- gg + scale_fill_identity()
    } else {
      gg <- gg + scale_color_manual(color_palette)
    }
  } else {
    if (is.character(data[[color_by]]) | is.factor(data[[color_by]])) {
      color_var_length <- length(data[[color_by]])
      if ( length(color_palette) < color_var_length) {
        color_palette <- generate_colors(color_palette, color_var_length)
      }
      gg <- gg + scale_color_manual(color_palette)
    } else {
      l_color <- find_extreme_colors(color_palette)
      gg <- gg + scale_color_gradient(low = l_color$low, high = l_color$high)
    }
  }
  gg
}
