

merge_theme_options <- function(opts){
  theme_vars <- names(default_theme_opts())
  opts_theme <- removeNulls(opts[theme_vars])
  str(opts_theme)
  opts_theme <- removeNulls(modifyList(opts$theme, opts_theme))
}

default_theme_opts <- function(){

  datasketch_colors <- c("#f9e853", "#9cec5b", "#50c5b7",
                         "#b33f90", "#ff9a2c", "#54419b",
                         "#f75e64", "#5d6ae9")
  list(
    logo = "",
    logo_position = "",
    palette_colors = datasketch_colors,
    background_color = '#FaFaF5',
    accent_color = "#d2a045",
    text_size = 11,
    text_color = "#6D8089",
    text_family = "Ubuntu",
    line_color = "#DDDDF9",
    line_size = 1,
    title_color = "#444444",
    title_allign = "left", # left - center - right
    title_family = "Montserrat",
    subtitle_color = "#999999",
    subtitle_allign = "left", # left - center - right
    caption_color = "#AAAAAA",
    caption_allign = "right", # left - center - right
    axis_title_color = '#5A6B72',
    axis_line_color = '#DDDDF9',
    axis_ticks_color = '#DDDDF9',
    grid_color =  "#fafafa"
  )
}


theme_datasketch <- function(opts = NULL){

  thm <- list(
    line_colour = opts$line_color,
    line_size = opts$line_size,
    rect_colour = opts$rect_color %||% opts$background_color,
    text_colour = opts$text_color,
    text_size = opts$text_size,
    axis_text_colour = opts$axis_text_color %||% opts$text_color,
    axis_line_colour = opts$axis_line_color %||% opts$line_color,
    axis_line_x_colour = opts$axis_line_x_color %||% opts$line_color,
    axis_line_y_colour = opts$axis_line_y_color %||% opts$line_color,
    axis_text_x_colour = opts$axis_text_x_color %||% opts$text_color,
    axis_text_y_colour = opts$axis_text_y_color  %||% opts$text_color,
    axis_title_colour = opts$axis_title_color %||% opts$text_color,
    axis_title_x_colour = opts$axis_title_x_color %||% opts$text_color,
    # axis_title_x_angle = opts$axis_title_x_angle,
    axis_title_y_colour = opts$axis_title_y_color%||% opts$text_color,
    # axis_title_y_angle = opts$axis_title_y_angle,
    axis_ticks_colour = opts$axis_ticks_color %||% opts$line_color, # transparent
    legend_background_colour = opts$legend_background_color %||% opts$text_color,
    legend_background_fill = opts$legend_background_fill %||% opts$background_color,
    legend_key_colour = opts$legend_key_color %||% opts$background_color,
    legend_key_fill = opts$legend_key_fill %||% opts$background_color,
    legend_text_colour = opts$legend_text_color %||% opts$text_color,
    panel_background_fill = opts$panel_background_fill %||% opts$background_color,
    panel_border_size = opts$panel_border_size %||% opts$line_size,
    # panel_border_colour = opts$panel_border_color,
    panel_grid_major_colour = opts$panel_grid_major_color %||% opts$grid_color,
    panel_grid_minor_colour = opts$panel_grid_minor_color %||% opts$grid_color,
    strip_background_fill = opts$strip_background_fill %||% opts$accent_color,
    plot_background_colour = opts$plot_background_color %||% opts$background_color,
    plot_background_fill = opts$plot_background_fill%||% opts$background_color,
    plot_title_family = opts$plot_title_family %||% opts$title_family,
    plot_title_colour = opts$plot_title_color %||% opts$text_color,
    plot_subtitle_family = opts$plot_subtitle_family %||% opts$text_family,
    plot_subtitle_colour = opts$plot_subtitle_color %||% opts$text_color,
    plot_caption_family = opts$plot_caption_family %||% opts$text_family,
    plot_caption_colour = opts$plot_caption_color %||% opts$text_color
  )


  theme(
    line = element_line(
      colour = thm$line_colour,
      size = thm$line_size,
      linetype = 1,
      lineend = "butt"),
    rect = element_rect(
      fill = "white",
      colour = thm$rect_colour,
      size = 0.5,
      linetype = 1),
    text = element_text(
      debug=FALSE,
      margin=margin(),
      family = '',
      face = "plain",
      colour = thm$text_colour,
      size = thm$text_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 1.2),
    axis.text = element_text(
      debug=FALSE,
      margin=margin(6, 0, 6, 0),
      size = rel(0.8),
      colour = thm$axis_text_color),
    axis.line = element_line(
      colour = thm$axis_line_colour,
      #size = thm$line_size
      size = rel(0.5)
      ),
    axis.line.x = element_line(
      colour = thm$axis_line_x_colour),
    axis.line.y = element_line(
      colour = thm$axis_line_y_colour),
    axis.text.x = element_text(
      debug=FALSE,
      # margin=margin(6, 0, 6, 0),
      vjust = 1,
      colour = thm$axis_text_x_colour,
      angle = thm$axis_text_x_angle,
      face='plain'),
    axis.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      hjust = 1,
      colour = thm$axis_text_y_colour,
      angle = thm$axis_text_y_angle,
      face='plain'),
    axis.title = element_text(
      face='plain',
      lineheight = 1.5,
      colour = thm$axis_title_colour),
    axis.title.x = element_text(
      debug=FALSE,
      colour = thm$axis_title_x_colour,
      # margin=margin(),
      vjust=1),
    axis.title.y = element_text(
      debug=FALSE,
      colour = thm$axis_title_y_colour,
      # margin=margin(),
      angle = 90,
      vjust=1),
    axis.ticks = element_line(
      colour = thm$axis_ticks_colour %||% thm$axis_line_colour),
    # axis.ticks.length = grid::unit(0.15, "cm"),
    # axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
    # axis.ticks.length.x.top = grid::unit(0.15, "cm"),
    # axis.ticks.length.y.left = grid::unit(0.15, "cm"),
    # axis.ticks.length.y.right = grid::unit(0.15, "cm"),
    legend.background = element_rect(
      colour = thm$legend_background_colour,
      fill = thm$legend_background_fill),
    #legend.margin = #grid::unit(0.2 * spacing, "cm"),
    legend.key = element_rect(
      colour = thm$legend_key_colour,
      fill = thm$legend_key_fill),
    # legend.key.size = grid::unit( 1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(
      debug=FALSE,
      colour = thm$legend_text_colour,
      margin=margin(),
      size = rel(0.8)),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(
      fill = thm$panel_background_fill,
      colour = NA),
    panel.border = element_rect(
      size = thm$panel_border_size,
      fill = 'transparent',
      colour = thm$panel_border_colour),
    panel.grid.major = element_line(
      # size = NULL,
      linetype='solid',
      colour = thm$panel_grid_major_colour),
    # panel.grid.major.y = element_line(
    #   size = 0.5,
    #   linetype = 'solid',
    #   colour = ifelse(orientation == "ver", "#5A6B72", "transparent")),
    # panel.grid.major.x = element_line(
    #   size = 0.5,
    #   linetype = 'solid',
    #   colour = ifelse(orientation == "ver", "transparent", "#5A6B72")),
    panel.grid.minor = element_line(
      linetype='solid',
      colour = thm$panel_grid_minor_colour),
    # panel.margin = grid::unit(0.5 * spacing, 'cm'),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    strip.background = element_rect(
      fill = thm$strip_background_fill,
      colour = NA),
    strip.text.x = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(1),
      face = 'plain'),
    strip.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = -90,
      face = 'plain',
      size = rel(1)),
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    plot.background = element_rect(
      colour = thm$plot_background_colour,
      fill = thm$plot_background_fill),
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      debug=FALSE,
      family = thm$plot_title_family,
      colour = thm$plot_title_colour,
      margin=margin(6, 0, 6, 0),
      size = rel(1.2),
      hjust = 0,
      vjust = 1,
      face='plain'),
    plot.subtitle = element_text(
      debug=FALSE,
      family = thm$plot_subtitle_family,
      colour = thm$plot_subtitle_colour,
      margin=margin(3, 0, 9, 0),
      size = rel(1),
      hjust = 0,
      vjust = 1,
      face='plain'),
    plot.caption = element_text(
      debug=FALSE,
      family = thm$plot_subtitle_family,
      colour = thm$plot_caption_colour,
      margin=margin(6, 0, 6, 0),
      size = rel(0.8),
      hjust = 1,
      vjust = 1,
      lineheight = 1.5,
      face='plain'),
    # plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
    complete = TRUE
  )
}
