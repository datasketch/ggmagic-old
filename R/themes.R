#' @export
getPalette <- function(type = "qualitative", rev = FALSE){
  dsGreen <- "#95C11E"
  dsYellow <- "#FFED00"
  dsMagenta <- "#E5007D"
  dsBlue <- "#009EE3"
  dsOrange <- "#F9B233"
  dsPink <- "#EF8998"
  dsLightBlue <- "#16C5E0"
  dsPurple <- "#A839B0"
  dsRed <- "#C92F2F"
  dsGray <- "#A9A9A9"
  dsLila <- "#9B71AF"
  dsPalette <- c(dsBlue,dsMagenta,dsGreen,dsOrange,dsYellow,dsPink,
                 dsLightBlue,dsPurple,dsRed,dsGray, dsLila)
  p <- rep(dsPalette,3)
  if(type == "sequential") {
    p <-  c(dsMagenta,dsBlue)
  }
  if(rev) p <- rev(p)
  p
}

#' @export
theme_ds_clean <- function(){
  theme_ds() + theme(
    axis.line=element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major=element_blank())
}


#' @export
theme_ds <- function(...){
  type <- 'outer'
  inner <- type == 'inner'
  palette <- list(
    background = "#ffffff", # #ffffff #F0EDFF #F8EDFA #FDF8FD
    text = list(inner = "#555555", outer = "#111111"),
    line = list(inner = "#826A50", outer = "#362C21"),
    gridline = "#c9c7d3",
    swatch = c("#111111","#65ADC2","#233B43","#E84646","#C29365","#362C21","#316675","#168E7F","#109B37"),
    gradient = list(low = "#65ADC2", high = "#362C21")
  )
  spacing <- 0.5
  line_colour <- "#1d1d1d"
  text_colour <- "#555555"
  text_size <- 12
  line_weight <- 0.5
  x_title_spacing <- function(spacing)
    max(-1.2, -(spacing / 1.25) + 0.5)
  y_title_spacing <- function(spacing)
    max(0.8, min(2.4, spacing))

  theme(
    #legend.title=element_blank(),
    line = element_line(
      colour = line_colour,
      size = line_weight,
      linetype = 1,
      lineend = "butt"),
    rect = element_rect(
      fill = "white",
      colour = text_colour,
      size = 0.5,
      linetype = 1),
    text = element_text(
      debug=FALSE,
      margin=margin(),
      family = '',
      face = "plain",
      colour = text_colour,
      size = text_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9),
    axis.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8),
      colour = text_colour),
    strip.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    axis.line = element_line(
      colour = line_colour),
    axis.line.x = element_line(colour = line_colour),
    axis.line.y = element_line(colour = line_colour),
    axis.text.x = element_text(
      debug=FALSE,
      margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
      vjust = 1,
      colour = text_colour,
      face='bold'),
    axis.text.y = element_text(
      debug=FALSE,
      margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
      hjust = 1,
      colour = text_colour,
      face='bold'),
    axis.ticks = element_line(colour = line_colour),
    axis.title = element_text(face='bold',colour = text_colour),
    axis.title.x = element_text(
      debug=FALSE,
      margin=margin(),
      vjust=x_title_spacing(spacing)),
    axis.title.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = 90,
      vjust=y_title_spacing(spacing)),
    axis.ticks.length = grid::unit(0.15, "cm"),
    axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
    axis.ticks.length.x.top = grid::unit(0.15, "cm"),
    axis.ticks.length.y.left = grid::unit(0.15, "cm"),
    axis.ticks.length.y.right = grid::unit(0.15, "cm"),
    legend.background = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = ifelse(inner, 'white', palette$background)),
    legend.margin = grid::unit(0.2 * spacing, "cm"),
    legend.key = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = palette$background),
    legend.key.size = grid::unit(
      1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(fill = palette$background,colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(linetype='dashed',colour = palette$gridline),
    panel.grid.minor = element_blank(),
    panel.margin = grid::unit(0.5 * spacing, 'cm'),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(
      fill = ifelse(inner, 'white', palette$background),
      colour = NA),
    strip.text.x = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(1.1),
      face = 'bold'),
    strip.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = -90,
      face = 'bold',
      size = rel(1.1)),
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    plot.background = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = ifelse(inner, 'white', palette$background)),

    plot.title = element_text(
      debug=FALSE,
      margin=margin(0, 0, 6.6, 0),
      size = rel(1.2),
      vjust = spacing,
      face='bold'),
    plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
    complete = TRUE
  )
}

getDefaultTheme <- list(
  background = 'transparent',
  bordercolor = 'transparent',
  colores = NULL,
  fontFamily = 'Ubuntu',
  fontSize = 11,
  color = '#5A6B72',
  marginBottom = 0,
  marginLeft = 0,
  marginRight = 0,
  marginTop = 0,
  plotBackgroundColor = "transparent",
  plotBorderColor = "transparent",
  plotBorderWidth = 1,
  gridColor =  "#cccccc",
  angleTicks = 0,
  axis_x = list(color = "#5A6B72"),
  axis_y = list(color = "#5A6B72")
)

getTheme <- function(theme = NULL){

  userTheme <- theme
  defaultTheme <- getDefaultTheme

  if(!is.null(theme)){
    theme <- modifyList(defaultTheme, userTheme)
  }else{
    theme <- defaultTheme
  }

  theme
}



#' @export
tma <- function(custom = NULL, orientation = "ver", ...) {

  custom <- getTheme(theme = custom)
  theme(
        plot.background = element_rect(fill = custom$background, colour = custom$background),
        panel.background = element_rect(fill = custom$plotBackgroundColor),
        panel.border = element_rect(size = custom$plotBorderWidth, fill = 'transparent', colour = custom$plotBorderColor),
        text = element_text(size = custom$fontSize,  family = custom$fontFamily),
        axis.title.x = element_text(colour = custom$color), #color label eje x
        axis.title.y = element_text(colour = custom$color),
        axis.text.x = element_text(color = custom$color, size = custom$fontSize, angle = custom$angleTicks),
        axis.text.y = element_text(color = custom$color, size = custom$fontSize, angle = custom$angleTicks),
        #  plot.margin = margin(custom$marginTop, custom$marginRight, custom$marginBottom, custom$marginLeft),
        panel.grid.major.y = element_line(size = 0.4, linetype = 'dotted', colour = ifelse(orientation == "ver", "#5A6B72", "transparent")),
        panel.grid.major.x = element_line(size = 0.4, linetype = 'dotted', colour = ifelse(orientation == "ver", "transparent", "#5A6B72")),
        panel.grid.minor = element_line(size = 0.4, linetype = 'solid', colour = "transparent"),
        axis.ticks = element_line(colour = 'transparent'),
        axis.line.x = element_line(colour = ifelse(orientation == "ver", custom$axis_x$color, "transparent")),
        axis.line.y = element_line(colour = ifelse(orientation == "ver", "transparent", custom$axis_y$color))
        #axis.text = element_text(size = (custom$fontSize-2), family = custom$fontFamily)
  )
}

#' @export
theme_leg <- function(custom = NULL, ...) {
  custom <- getTheme(theme = custom)
  theme(
    legend.title= element_blank(),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.key.size = unit(0.5, "cm"),
    legend.text=element_text(color=custom$color,size=11, margin = margin(0, .3, 0, 0, "cm")),
    legend.background = element_rect(colour = NA, fill = 'transparent'),
    plot.caption = element_text(hjust = 1),
    #legend.box.spacing = unit(0.3, "cm"),
    #legend.box.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
    legend.box.background = element_rect(colour = "transparent", fill = "transparent")
  )
}
