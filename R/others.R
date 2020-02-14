#' #############################Boxplot
#'
#' #' Vertical boxplot
#' #' Boxplot
#' #' @name gg_boxplot_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_boxplot_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
#'     geom_boxplot(show.legend = FALSE)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal boxplot
#' #' Boxplot flipped
#' #' @name gg_boxplot_flip_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_boxplot_flip_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                     yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'
#'
#'   graph <- gg_boxplot_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical boxplot + jitter
#' #' Boxplot + dot jitter
#' #' @name gg_boxplot_dot_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_boxplot_dot_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                    yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) + geom_jitter(color = "#D55E00", show.legend = FALSE) +
#'     geom_boxplot(show.legend = FALSE)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal boxplot + jitter
#' #' Boxplot + dot jitter flipped
#' #' @name gg_boxplot_dot_flip_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_boxplot_dot_flip_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                         yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'
#'
#'   graph <- gg_boxplot_dot_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#'
#' #' Vertical boxplot
#' #' Box plot
#' #' @name gg_box_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_box_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
#'                         xLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[1]
#'   xlab <- xLabel %||% "indice"
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   graph <- ggplot(data, aes(x=factor(""), y=a)) + geom_boxplot(aes(fill = ""), show.legend = FALSE)
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#'
#' #' Horizontal boxplot
#' #' Box plot flipped
#' #' @name gg_box_flip_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_box_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
#'                              xLabel = NULL, angle_x = 0, ...){
#'
#'   graph <- gg_box_Num.(data, titleLabel, subtitle, caption, yLabel, xLabel, angle_x = 0, ...)
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Boxplot
#' #' @name gg_box_DatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Dat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_box_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>%
#'     dplyr::filter(!is.na(a), !is.na(b))
#'
#'   graph <- ggplot(data) + geom_boxplot(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
#'     scale_fill_manual(values = getPalette()) +
#'     scale_y_continuous(labels = comma) +
#'     theme_ds() +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' ###################################
#'
#'
#' ################################## Densities
#'
#'
#' #' Density distribution
#' #' Coloured Density Distribution
#' #' @name gg_density_multi_dist_coloured_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_density_multi_dist_coloured_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                                                    xLabel = NULL, yLabel = NULL, fillLabel = NULL, leg_pos = "right",
#'                                                    angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   clab <- fillLabel %||% nms[1]
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(b))
#'   graph <- graph + geom_density(aes(colour = a)) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab) +
#'     theme_ds() + theme(legend.position=leg_pos) + scale_color_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#'
#' #' Vertical distribution facet
#' #' Facet Vertical Dist
#' #' @name gg_dist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                       yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(b))
#'   graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) + facet_wrap(~a) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds()
#'   graph <- graph + scale_color_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal distribution facet
#' #' Facet Horizontal Dist
#' #' @name gg_dist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                       yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_dist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#'
#' #' Vertical density dot + distribution facet
#' #' Facet Vertical Dot Dist
#' #' @name gg_dot_dist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                           yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(b))
#'   graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) +
#'     geom_point(aes(y = 0), color = "#D55E00", alpha = alpha, show.legend = FALSE) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     theme(legend.position=leg_pos) +
#'     scale_color_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'   graph <- graph + facet_wrap(~a)
#'
#'   graph
#' }
#'
#' #' Horizontal density dot + distribution facet
#' #' Facet Horizontal Dot Dist
#' #' @name gg_dot_dist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                           yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   graph <- gg_dot_dist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#'
#' #' Horizontal density 2D bin
#' #' Density in 2D
#' #' @name gg_dens_NumNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dens_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                             yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[2]
#'   xlab <- xLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a), !is.na(b))
#'
#'   graph <- ggplot(data, aes(x = a, y = b)) +
#'     stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
#'     theme_ds() +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position = leg_pos)
#'
#'   if(reverse){
#'     graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
#'                                          high = getPalette(type = "sequential")[1])
#'   }else{
#'     graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
#'                                          high = getPalette(type = "sequential")[2])
#'   }
#'   return(graph)
#' }
#'
#'
#' #' Vertical density 2D bin
#' #' Density 2D flipped
#' #' @name gg_dens_flip_NumNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dens_flip_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                  yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){
#'
#'   graph <- gg_dens_NumNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, reverse, angle_x, leg_pos, ...)
#'   graph <- graph + coord_flip()
#'   graph
#' }
#'
#'
#'
#' #' Cumulative distribution function
#' #' Cumulative distribution function
#' #' @name gg_dist_cum_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_cum_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                              yLabel = NULL, angle_x = 0, ...){
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[1]
#'   xlab <- xLabel %||% "index"
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   graph <- ggplot(data, aes(a)) + geom_step(aes(y=..y.., color = ""), stat="ecdf", show.legend = FALSE) +
#'     scale_color_manual(values = getPalette())
#'
#'   graph <- graph + theme_ds()
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   return(graph)
#'
#' }
#'
#'
#'
#' #########################################################33
#'
#' ####################### Gauge
#'
#'
#'
#' #' Dial gauge
#' #' Gauge
#' #' @name gg_gauge_dial_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_gauge_dial_CatNum. <- function(data,aggregation ='sum', ...){
#'
#'   gg.gauge <- function(pos, breaks=c(0,50,100)) {
#'     require(ggplot2)
#'     get.poly <- function(a,b,r1=0.5,r2=1.0) {
#'       th.start <- pi*(1-a/100)
#'       th.end   <- pi*(1-b/100)
#'       th       <- seq(th.start,th.end,length=100)
#'       x        <- c(r1*cos(th),rev(r2*cos(th)))
#'       y        <- c(r1*sin(th),rev(r2*sin(th)))
#'       return(data.frame(x,y))
#'     }
#'     graph <- ggplot()+
#'       geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill = getPalette()[1]) +
#'       geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill = getPalette()[2]) +
#'       annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
#'       coord_fixed()+
#'       theme_bw()+
#'       theme(axis.text=element_blank(),
#'             axis.title=element_blank(),
#'             axis.ticks=element_blank(),
#'             panel.grid=element_blank(),
#'             panel.border=element_blank())
#'     return(graph)
#'   }
#'
#'   f <- fringe(data)
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(sum = agg(aggregation, b)) %>%
#'     dplyr::arrange(desc(sum)) %>%
#'     dplyr::mutate(order = 1:nrow(.), prop = sum/sum(sum))
#'
#'
#'   newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
#'                     data_graph$a, SIMPLIFY=FALSE)
#'
#'   graphList <- lapply(newList, gg.gauge)
#'   grid.newpage()
#'   grid.draw(arrangeGrob(grobs = graphList,ncol=2))
#' }
#'
#'
#' #' Gauge
#' #' Gauge
#' #' @name gg_gauge_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_gauge_CatNum. <- function(data,aggregation = 'sum', ...){
#'
#'   gg.gauge <- function(pos, breaks=c(0,30,70,100)) {
#'     require(ggplot2)
#'     get.poly <- function(a,b,r1=0.5,r2=1.0) {
#'       th.start <- pi*(1-a/100)
#'       th.end   <- pi*(1-b/100)
#'       th       <- seq(th.start,th.end,length=100)
#'       x        <- c(r1*cos(th),rev(r2*cos(th)))
#'       y        <- c(r1*sin(th),rev(r2*sin(th)))
#'       return(data.frame(x,y))
#'     }
#'     graph <- ggplot()+
#'       geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill = getPalette()[1])+
#'       geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill = getPalette()[2])+
#'       geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill = getPalette()[3])+
#'       geom_polygon(data=get.poly(as.numeric(pos[[1]])-1,as.numeric(pos[[1]])+1,0.2),aes(x,y))+
#'       geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
#'                 aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
#'       annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
#'       coord_fixed()+
#'       theme_bw()+
#'       theme(axis.text=element_blank(),
#'             axis.title=element_blank(),
#'             axis.ticks=element_blank(),
#'             panel.grid=element_blank(),
#'             panel.border=element_blank())
#'     return(graph)
#'   }
#'
#'   f <- fringe(data)
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(sum = agg(aggregation, b)) %>%
#'     dplyr::arrange(desc(sum)) %>%
#'     dplyr::mutate(order = 1:nrow(.), prop = sum/sum(sum))
#'
#'   newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
#'                     data_graph$a, SIMPLIFY=FALSE)
#'
#'   graphList <- lapply(newList, gg.gauge)
#'   grid.newpage()
#'   grid.draw(arrangeGrob(grobs = graphList,ncol=2))
#' }
#'
#' #' Gauge
#' #' Gauge
#' #' @name gg_gauge_Cat.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_gauge_Cat. <- function(data, titleLabel = '', subtitle = '', caption = '', ncol = 2, ...){
#'
#'   gg.gauge <- function(pos, breaks=c(0,30,70,100)) {
#'     require(ggplot2)
#'     get.poly <- function(a,b,r1=0.5,r2=1.0) {
#'       th.start <- pi*(1-a/100)
#'       th.end   <- pi*(1-b/100)
#'       th       <- seq(th.start,th.end,length=100)
#'       x        <- c(r1*cos(th),rev(r2*cos(th)))
#'       y        <- c(r1*sin(th),rev(r2*sin(th)))
#'       return(data.frame(x,y))
#'     }
#'     graph <- ggplot()+
#'       geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y), fill = getPalette()[1])+
#'       geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill = getPalette()[2])+
#'       geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill = getPalette()[3])+
#'       geom_polygon(data=get.poly(as.numeric(pos[[1]])-1,as.numeric(pos[[1]])+1,0.2),aes(x,y))+
#'       geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
#'                 aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
#'       annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
#'       coord_fixed()+
#'       theme_bw()+
#'       theme(axis.text=element_blank(),
#'             axis.title=element_blank(),
#'             axis.ticks=element_blank(),
#'             panel.grid=element_blank(),
#'             panel.border=element_blank())
#'     return(graph)
#'   }
#'
#'   f <- fringe(data)
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(count = n()) %>%
#'     dplyr::arrange(desc(count)) %>%
#'     dplyr::mutate(order = 1:nrow(.), prop = count/(sum(count)))
#'
#'
#'   newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
#'                     data_graph$a, SIMPLIFY=FALSE)
#'
#'   graphList <- lapply(newList, gg.gauge)
#'   grid.newpage()
#'   grid.draw(arrangeGrob(grobs = graphList,ncol= ncol))
#' }
#'
#' #' Dial gauge
#' #' Gauge
#' #' @name gg_gauge_dial_Cat.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_gauge_dial_Cat. <- function(data, ...){
#'
#'   gg.gauge <- function(pos, breaks=c(0,50,100)) {
#'     require(ggplot2)
#'     get.poly <- function(a,b,r1=0.5,r2=1.0) {
#'       th.start <- pi*(1-a/100)
#'       th.end   <- pi*(1-b/100)
#'       th       <- seq(th.start,th.end,length=100)
#'       x        <- c(r1*cos(th),rev(r2*cos(th)))
#'       y        <- c(r1*sin(th),rev(r2*sin(th)))
#'       return(data.frame(x,y))
#'     }
#'     graph <- ggplot()+
#'       geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill = getPalette()[1])+
#'       geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill = getPalette()[2])+
#'       annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
#'       coord_fixed()+
#'       theme_bw()+
#'       theme(axis.text=element_blank(),
#'             axis.title=element_blank(),
#'             axis.ticks=element_blank(),
#'             panel.grid=element_blank(),
#'             panel.border=element_blank())
#'     return(graph)
#'   }
#'
#'   f <- fringe(data)
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(count = n()) %>%
#'     dplyr::arrange(desc(count)) %>%
#'     dplyr::mutate(order = 1:nrow(.), prop = count/(sum(count)))
#'
#'
#'   newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
#'                     data_graph$a, SIMPLIFY=FALSE)
#'
#'   graphList <- lapply(newList, gg.gauge)
#'   grid.newpage()
#'   grid.draw(arrangeGrob(grobs = graphList,ncol=2))
#' }
#'
#' #########################################################
#'
#' #################### RADAR
#'
#'
#' # https://github.com/ricardo-bion/ggradar/blob/master/R/ggradar.R
#' ggradar <- function(plot.data,
#'                     font.radar="",
#'                     values.radar = c("0%", "50%", "100%"),
#'                     axis.labels=colnames(plot.data)[-1],
#'                     grid.min=0,  #10,
#'                     grid.mid=0.5,  #50,
#'                     grid.max=1,  #100,
#'                     centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
#'                     plot.extent.x.sf=1,
#'                     plot.extent.y.sf=1.2,
#'                     x.centre.range=0.02*(grid.max-centre.y),
#'                     label.centre.y=FALSE,
#'                     grid.line.width=0.5,
#'                     gridline.min.linetype="longdash",
#'                     gridline.mid.linetype="longdash",
#'                     gridline.max.linetype="longdash",
#'                     gridline.min.colour="grey",
#'                     gridline.mid.colour="#007A87",
#'                     gridline.max.colour="grey",
#'                     grid.label.size=7,
#'                     gridline.label.offset=-0.1*(grid.max-centre.y),
#'                     label.gridline.min=TRUE,
#'                     axis.label.offset=1.15,
#'                     axis.label.size=8,
#'                     axis.line.colour="grey",
#'                     group.line.width=1.5,
#'                     group.point.size=6,
#'                     background.circle.colour="#D7D6D1",
#'                     background.circle.transparency=0.2,
#'                     plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
#'                     legend.title="",
#'                     legend.text.size=grid.label.size ) {
#'
#'   library(ggplot2)
#'
#'   plot.data <- as.data.frame(plot.data)
#'
#'   plot.data[,1] <- as.factor(as.character(plot.data[,1]))
#'   names(plot.data)[1] <- "group"
#'
#'   var.names <- colnames(plot.data)[-1]  #'Short version of variable names
#'   #axis.labels [if supplied] is designed to hold 'long version' of variable names
#'   #with line-breaks indicated using \n
#'
#'   #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
#'   plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
#'   plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
#'
#'   #Check supplied data makes sense
#'   if (length(axis.labels) != ncol(plot.data)-1)
#'     return("Error: 'axis.labels' contains the wrong number of axis labels")
#'   if(min(plot.data[,-1])<centre.y)
#'     return("Error: plot.data' contains value(s) < centre.y")
#'   if(max(plot.data[,-1])>grid.max)
#'     return("Error: 'plot.data' contains value(s) > grid.max")
#'   #Declare required internal functions
#'
#'   CalculateGroupPath <- function(df) {
#'     #Converts variable values into a set of radial x-y coordinates
#'     #Code adapted from a solution posted by Tony M to
#'     #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
#'     #Args:
#'     #  df: Col 1 -  group ('unique' cluster / group ID of entity)
#'     #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
#'
#'     path <- df[,1]
#'
#'     ##find increment
#'     angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
#'     ##create graph data frame
#'     graphData= data.frame(seg="", x=0,y=0)
#'     graphData=graphData[-1,]
#'
#'     for(i in levels(path)){
#'       pathData = subset(df, df[,1]==i)
#'       for(j in c(2:ncol(df))){
#'         #pathData[,j]= pathData[,j]
#'
#'
#'         graphData=rbind(graphData, data.frame(group=i,
#'                                               x=pathData[,j]*sin(angles[j-1]),
#'                                               y=pathData[,j]*cos(angles[j-1])))
#'       }
#'       ##complete the path by repeating first pair of coords in the path
#'       graphData=rbind(graphData, data.frame(group=i,
#'                                             x=pathData[,2]*sin(angles[1]),
#'                                             y=pathData[,2]*cos(angles[1])))
#'     }
#'     #Make sure that name of first column matches that of input data (in case !="group")
#'     colnames(graphData)[1] <- colnames(df)[1]
#'     graphData #data frame returned by function
#'   }
#'   CaclulateAxisPath = function(var.names,min,max) {
#'     #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
#'     #Args:
#'     #var.names - list of variables to be plotted on radar plot
#'     #min - MININUM value required for the plotted axes (same value will be applied to all axes)
#'     #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
#'     #var.names <- c("v1","v2","v3","v4","v5")
#'     n.vars <- length(var.names) # number of vars (axes) required
#'     #Cacluate required number of angles (in radians)
#'     angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
#'     #calculate vectors of min and max x+y coords
#'     min.x <- min*sin(angles)
#'     min.y <- min*cos(angles)
#'     max.x <- max*sin(angles)
#'     max.y <- max*cos(angles)
#'     #Combine into a set of uniquely numbered paths (one per variable)
#'     axisData <- NULL
#'     for (i in 1:n.vars) {
#'       a <- c(i,min.x[i],min.y[i])
#'       b <- c(i,max.x[i],max.y[i])
#'       axisData <- rbind(axisData,a,b)
#'     }
#'     #Add column names + set row names = row no. to allow conversion into a data frame
#'     colnames(axisData) <- c("axis.no","x","y")
#'     rownames(axisData) <- seq(1:nrow(axisData))
#'     #Return calculated axis paths
#'     as.data.frame(axisData)
#'   }
#'   funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
#'     #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#'     tt <- seq(0,2*pi,length.out = npoints)
#'     xx <- center[1] + r * cos(tt)
#'     yy <- center[2] + r * sin(tt)
#'     return(data.frame(x = xx, y = yy))
#'   }
#'
#'   ### Convert supplied data into plottable format
#'   # (a) add abs(centre.y) to supplied plot data
#'   #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
#'   # in user-supplied data]
#'   plot.data.offset <- plot.data
#'   plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
#'   #print(plot.data.offset)
#'   # (b) convert into radial coords
#'   group <-NULL
#'   group$path <- CalculateGroupPath(plot.data.offset)
#'
#'   #print(group$path)
#'   # (c) Calculate coordinates required to plot radial variable axes
#'   axis <- NULL
#'   axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
#'   #print(axis$path)
#'   # (d) Create file containing axis labels + associated plotting coordinates
#'   #Labels
#'   axis$label <- data.frame(
#'     text=axis.labels,
#'     x=NA,
#'     y=NA )
#'   #print(axis$label)
#'   #axis label coordinates
#'   n.vars <- length(var.names)
#'   angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
#'   axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
#'   axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
#'   #print(axis$label)
#'   # (e) Create Circular grid-lines + labels
#'   #caclulate the cooridinates required to plot circular grid-lines for three user-specified
#'   #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
#'   gridline <- NULL
#'   gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
#'   gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
#'   gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
#'   #print(head(gridline$max$path))
#'   #gridline labels
#'   gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
#'                                    text=as.character(grid.min))
#'   gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
#'                                    text=as.character(grid.max))
#'   gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
#'                                    text=as.character(grid.mid))
#'   #print(gridline$min$label)
#'   #print(gridline$max$label)
#'   #print(gridline$mid$label)
#'   ### Start building up the radar plot
#'
#'   # Delcare 'theme_clear', with or without a plot legend as required by user
#'   #[default = no legend if only 1 group [path] being plotted]
#'   theme_clear <- theme_bw(base_size=20) +
#'     theme(axis.text.y=element_blank(),
#'           axis.text.x=element_blank(),
#'           axis.ticks=element_blank(),
#'           panel.grid.major=element_blank(),
#'           panel.grid.minor=element_blank(),
#'           panel.border=element_blank(),
#'           legend.key=element_rect(linetype="blank"))
#'
#'   if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
#'
#'   #Base-layer = axis labels + plot extent
#'   # [need to declare plot extent as well, since the axis labels don't always
#'   # fit within the plot area automatically calculated by ggplot, even if all
#'   # included in first plot; and in any case the strategy followed here is to first
#'   # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
#'   # then centred labels for axis labels almost immediately above/below x= 0
#'   # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
#'   # This building up the plot in layers doesn't allow ggplot to correctly
#'   # identify plot extent when plotting first (base) layer]
#'
#'   #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
#'   base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
#'     geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
#'               aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, family=font.radar) +
#'     scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) +
#'     scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
#'
#'   # + axis labels for any vertical axes [abs(x)<=x.centre.range]
#'   base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
#'                            aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar)
#'   # + axis labels for any vertical axes [x>x.centre.range]
#'   base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
#'                            aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, family=font.radar)
#'   # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
#'   base <- base + theme_clear
#'   #  + background circle against which to plot radar data
#'   base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
#'                               fill=background.circle.colour,
#'                               alpha=background.circle.transparency)
#'
#'   # + radial axes
#'   base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
#'                            colour=axis.line.colour)
#'
#'
#'   # ... + group (cluster) 'paths'
#'   base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
#'                            size=group.line.width)
#'
#'   # ... + group points (cluster data)
#'   base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
#'
#'
#'   #... + amend Legend title
#'   if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
#'   # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
#'   base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
#'                             lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
#'   base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
#'                             lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
#'   base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
#'                             lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
#'   # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
#'   if (label.gridline.min==TRUE) {
#'
#'     base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1, family=font.radar) }
#'   base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
#'   base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
#'   # ... + centre.y label if required [i.e. value of y at centre of plot circle]
#'   if (label.centre.y==TRUE) {
#'     centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
#'     base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5, family=font.radar) }
#'
#'   base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20,
#'                                                                                     family = font.radar)) +
#'     theme(legend.text = element_text(size = 20), legend.position="left") +
#'     theme(legend.key.height=unit(2,"line")) +
#'     scale_colour_manual(values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051",
#'                                      "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)) +
#'     theme(text=element_text(family=font.radar)) +
#'     theme(legend.title=element_blank())
#'
#'   return(base)
#'
#' }
#'
#'
#' #' Radar
#' #' point
#' #' @name gg_radar_CatNumP.
#' #' @param x A category.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-NumP
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_radar_CatNumP. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                               leg_pos="right",rescale = FALSE,
#'                               aggregation = "mean",...){
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   data <- f$data
#'
#'   data <- data %>%
#'     dplyr::select(group = 1,everything())
#'
#'   data <- data %>% group_by(group) %>%
#'     dplyr::summarise_each(funs(agg(aggregation,.)))
#'
#'   # if(rescale){
#'   #   data <- data %>%
#'   #     mutate_each(funs(rescale), -1)
#'   # }
#'
#'   data <- data %>%
#'     mutate_each(funs(rescale), -1)
#'
#'   graph <- ggradar(data) +
#'     scale_color_manual(values = getPalette()) + theme_ds() + theme_ds_clean() +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption) +
#'     theme(legend.position=leg_pos)
#'   graph
#' }
#'
#'
#' ## HISTOGRAMA
#' #' Vertical stacked histogram
#' #' Stacked Vertical Histogram
#' #' @name gg_hist_stacked_ver_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_stacked_ver_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                         yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   clab <- fillLabel %||% nms[1]
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(b))
#'   graph <- graph + geom_histogram(aes(fill = a), binwidth = 10) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
#'     theme_ds() + theme(legend.position=leg_pos) + scale_fill_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'   graph
#' }
#'
#'
#' #' Horizontal histogram + mean facet
#' #' Facet Horizontal Histogram + Mean
#' #' @name gg_hist_hor_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram facet
#' #' Facet Vertical Histogram
#' #' @name gg_hist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                       yLabel = NULL, leg_pos='right', angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) + facet_wrap(~a) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     scale_fill_manual(values = getPalette()) +
#'     theme(legend.position=leg_pos) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram facet
#' #' Facet Horizontal Histogram
#' #' @name gg_hist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                       yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram + distribution facet
#' #' Facet Vertical Histogram + Dist
#' #' @name gg_dist_hist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(aes(color = ""), show.legend = FALSE) +
#'     theme_ds() + theme(legend.position=leg_pos) +
#'     scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
#'   graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram + distribution facet
#' #' Facet Horizontal Histogram + Dist
#' #' @name gg_dist_hist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_dist_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram + distribution + mean facet
#' #' Facet Vertical Histogram + Dist + Mean
#' #' @name gg_dist_hist_ver_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                 yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(prom = mean(b, na.rm = TRUE))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(aes(color = ""), show.legend = FALSE) +
#'     geom_vline(data = data_graph, aes(xintercept = prom, color = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
#'     theme_ds() + theme(legend.position=leg_pos) +
#'     scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
#'   graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram + distribution + mean facet
#' #' Facet Horizontal Histogram + Dist + Mean
#' #' @name gg_dist_hist_hor_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                 yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_dist_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical density dot + distribution facet
#' #' Facet Vertical Dot Dist
#' #' @name gg_dot_dist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                           yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(b))
#'   graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) +
#'     geom_point(aes(y = 0), color = "#D55E00", alpha = alpha, show.legend = FALSE) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     theme(legend.position=leg_pos) +
#'     scale_color_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'   graph <- graph + facet_wrap(~a)
#'
#'   graph
#' }
#'
#'
#' #' Horizontal histogram + mean facet
#' #' Facet Horizontal Histogram + Mean
#' #' @name gg_hist_hor_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram facet
#' #' Facet Vertical Histogram
#' #' @name gg_hist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                       yLabel = NULL, leg_pos='right', angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) + facet_wrap(~a) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     scale_fill_manual(values = getPalette()) +
#'     theme(legend.position=leg_pos) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram facet
#' #' Facet Horizontal Histogram
#' #' @name gg_hist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                       yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram + distribution facet
#' #' Facet Vertical Histogram + Dist
#' #' @name gg_dist_hist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(aes(color = ""), show.legend = FALSE) +
#'     theme_ds() + theme(legend.position=leg_pos) +
#'     scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
#'   graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram + distribution facet
#' #' Facet Horizontal Histogram + Dist
#' #' @name gg_dist_hist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_dist_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram + distribution + mean facet
#' #' Facet Vertical Histogram + Dist + Mean
#' #' @name gg_dist_hist_ver_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                 yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(prom = mean(b, na.rm = TRUE))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(aes(color = ""), show.legend = FALSE) +
#'     geom_vline(data = data_graph, aes(xintercept = prom, color = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
#'     theme_ds() + theme(legend.position=leg_pos) +
#'     scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
#'   graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram + distribution + mean facet
#' #' Facet Horizontal Histogram + Dist + Mean
#' #' @name gg_dist_hist_hor_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dist_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                 yLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   graph <- gg_dist_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#'
#'
#' #' Vertical density dot + histogram facet
#' #' Facet Vertical Dot Histogram
#' #' @name gg_dot_hist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                           yLabel = NULL, leg_pos='right', alpha = 0.3, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
#'     geom_point(aes(y=0), alpha = alpha, color = "#D55E00") +
#'     facet_wrap(~a) +
#'     scale_fill_manual(values = getPalette()) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal density dot + histogram facet
#' #' Facet Horizontal Histogram + Dot
#' #' @name gg_dot_hist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                           yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   graph <- gg_dot_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical density dot + histogram + mean facet
#' #' Facet Vertical Histogram + Mean + Dot
#' #' @name gg_dot_hist_ver_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                yLabel = NULL, leg_pos='right', alpha = 0.3, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(prom = mean(b, na.rm = TRUE))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
#'     facet_wrap(~a) +
#'     geom_vline(data = data_graph, aes(xintercept = prom, color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
#'     geom_point(aes(y = 0),  alpha = alpha, color = "#D55E00")
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2]) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal density dot + histogram + mean facet
#' #' Facet Horizontal Histogram + Mean + Dot
#' #' @name gg_dot_hist_hor_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   graph <- gg_dot_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical density dot + histogram + distribution facet
#' #' Facet Vertical Histogram + Dist + Dot
#' #' @name gg_dot_dist_hist_ver_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(aes(color=""), show.legend = FALSE) +
#'     geom_point(aes(y = 0), alpha = alpha, color = "#D55E00") +
#'     theme_ds() +
#'     scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
#'   graph <- graph + facet_wrap(~a) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
#'
#'   graph
#' }
#'
#' #' Horizontal density dot + histogram + distribution facet
#' #' Facet Horizontal Histogram + Dist + Dot
#' #' @name gg_dot_dist_hist_hor_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                                yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   graph <- gg_dot_dist_hist_ver_facet_CatNum.(data, titleLabel,subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#'
#' #' Vertical density dot + histogram + distribution + mean facet
#' #' Facet Vertical Histogram + Dist + Mean + Dot
#' #' @name gg_dot_dist_hist_ver_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
#'                                                     leg_pos = "right", alpha = 0.3, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(prom = mean(b, na.rm = TRUE))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(aes(color = ""), show.legend = FALSE) +
#'     geom_vline(data = data_graph, aes(xintercept = prom, colour = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
#'     geom_point(aes(y=0), alpha = alpha, color = "#D55E00") +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme_ds() + scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
#'   graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal density dot + histogram + distribution + mean facet
#' #' Facet Horizontal Histogram + Dist + Mean + Dot
#' #' @name gg_dot_dist_hist_hor_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_dist_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){
#'
#'   graph <- gg_dot_dist_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)
#'
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram + mean facet
#' #' Facet Vertical Histogram + Mean
#' #' @name gg_hist_ver_mean_facet_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                            yLabel = NULL, leg_pos='right', angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% "Conteo"
#'   xlab <- xLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(prom = mean(b, na.rm = TRUE))
#'
#'   graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
#'     geom_vline(data = data_graph, aes(xintercept = prom, color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
#'     facet_wrap(~a)
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
#'     scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2]) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Histogram
#' #' Histogram
#' #' @name gg_hist_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                          yLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% "index"
#'   ylab <- yLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(fill= ""), show.legend = FALSE)
#'
#'   graph <- graph + geom_vline(aes(xintercept=mean(a), color = ""), linetype="dotted",
#'                               show.legend = FALSE) +
#'     scale_color_manual(values = getPalette()[2]) + scale_fill_manual(values = getPalette())
#'
#'
#'   graph <- graph + theme_ds()
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   return(graph)
#'
#' }
#'
#' #' Histogram + density
#' #' Histograms with density
#' #' @name gg_hist_dens_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_dens_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                               yLabel = NULL,  alfa = 0.5, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% "index"
#'   ylab <- yLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
#'     geom_density(alpha=alfa, aes(color = ""), show.legend = FALSE)
#'   graph <- graph + geom_vline(aes(xintercept=mean(a), color = ""),
#'                               linetype = "dotted", show.legend = FALSE) +
#'     scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2])
#'
#'   graph <- graph + theme_ds()
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   return(graph)
#'
#' }
#'
#'
#' #' Histogram density
#' #' density histogram
#' #' @name gg_density_hist_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_density_hist_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                  yLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% "index"
#'   ylab <- yLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   graph <- ggplot(data, aes(x=a)) + geom_density(aes(fill = ""), show.legend = FALSE)
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#' #' Vertical histogram dot bar
#' #' Dot bar
#' #' @name gg_dot_bar_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_bar_Num. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                             xLabel = NULL, yLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[1]
#'   xlab <- xLabel %||% "Index"
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   graph <- ggplot(data, aes(a)) + geom_dotplot(aes(fill = ""), show.legend = FALSE)
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Horizontal histogram dot bar
#' #' Dot bar flipped
#' #' @name gg_dot_bar_flip_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_dot_bar_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                                  xLabel = NULL, yLabel = NULL, angle_x = 0, ...){
#'
#'   graph <- gg_dot_bar_Num.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, ...)
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical histogram
#' #' Histogram 2D
#' #' @name gg_hist_NumNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                             xLabel = NULL, yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[2]
#'   xlab <- xLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a), !is.na(b))
#'
#'   binNummber <- floor(sqrt(nrow(data)))
#'   graph <- ggplot(data, aes(x = a, y = b)) + stat_bin2d(bins = binNummber) +
#'     theme_ds() +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position = leg_pos)
#'
#'   if(reverse){
#'     graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
#'                                          high = getPalette(type = "sequential")[1])
#'   }else{
#'     graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
#'                                          high = getPalette(type = "sequential")[2])
#'   }
#'   graph
#' }
#'
#' #' Horizontal histogram
#' #' Histogram 2d flipped
#' #' @name gg_hist_flip_NumNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_hist_flip_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                                  xLabel = NULL, yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){
#'
#'   graph <- gg_hist_NumNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, reverse, angle_x, leg_pos, ...)
#'   graph <- graph + coord_flip()
#'   graph
#' }
#'
#'
#' #' Histogram
#' #' histogram
#' #' @name gg_histogram_CatDat.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Dat
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_histogram_CatDat. <- function(data, titleLabel = "", subtitle = "", caption = "",xLabel = NULL,
#'                                  yLabel = NULL, fillLabel = NULL, angle_x = 0, leg_pos = "right", ...){
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[2]
#'   ylab <- yLabel %||% "Conteo"
#'   clab <- fillLabel %||% nms[1]
#'   d <- f$d
#'
#'   d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   g <- ggplot(d, aes(x=as.Date(b), fill= a)) +
#'     stat_bin(binwidth=1, position="identity") +
#'     #scale_x_date(breaks=date_breaks(width="1 month")) +
#'     scale_fill_manual(values = getPalette()) +
#'     theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'   g
#' }
#'
#' ####### Marimekko
#'
#' #' Vertical marimekko
#' #' Vertical Marimekko
#' #' @name gg_marimekko_ver_CatCat.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Cat
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_marimekko_ver_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   clab <- fillLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
#'                                  b = ifelse(is.na(b), "NA", b))
#'
#'   # xvar <- deparse(substitute(a))
#'   # yvar <- deparse(substitute(b))
#'   # mytable <- table(data)
#'   # widths <- c(0, cumsum(apply(mytable, 1, sum)))
#'   # heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))})
#'   #
#'   # alldata <- data.frame()
#'   # allnames <- data.frame()
#'   # for(i in 1:nrow(mytable)){
#'   #   for(j in 1:ncol(mytable)){
#'   #     alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]))
#'   #   }
#'   # }
#'   # colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
#'   #
#'   # alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)))
#'   # alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable))
#'   #
#'   # xxmax <- max(alldata$xmin, alldata$xmax)
#'   # alldata <- alldata %>% dplyr::mutate(xmin = xmin/xxmax, xmax = xmax/xxmax)
#'   #text <- alldata %>% dplyr::group_by(a) %>% dplyr::summarise(xtext = (max(xmax) + max(xmin)) / 2) %>% dplyr::mutate(ytext = 1.05)
#'
#'   var1 <- as.factor(data$a)
#'   var2 <- as.factor(data$b)
#'   levVar1 <- length(levels(var1))
#'   levVar2 <- length(levels(var2))
#'
#'   jointTable <- prop.table(table(var1, var2))
#'   plotData <- as.data.frame(jointTable)
#'   plotData$marginVar1 <- prop.table(table(var1))
#'   plotData$var2Height <- plotData$Freq / plotData$marginVar1
#'   plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
#'     plotData$marginVar1 / 2
#'
#'   graph <- ggplot(plotData, aes(var1Center, var2Height)) +
#'     geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
#'     geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) +
#'     labs(x = xLabel, y = yLabel, title = titleLabel, subtitle = subtitle, caption = caption, fill = clab)
#'
#'   # graph <- ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
#'   #   geom_rect(color="black", aes_string(fill=yvar))
#'   graph <- graph + theme_ds() +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'   graph <- graph + scale_fill_manual(values = getPalette()) +
#'     theme(legend.position = leg_pos) +
#'     scale_y_continuous(labels = percent) +
#'     scale_x_continuous(labels = percent)
#'
#'
#'   graph
#' }
#'
#' #' Horizontal marimekko
#' #' Horizontal Marimekko
#' #' @name gg_marimekko_hor_CatCat.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Cat
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_marimekko_hor_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                      yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
#'   graph <- gg_marimekko_ver_CatCat.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
#'   graph <- graph + coord_flip()
#'   graph
#' }
#'
#'
#' ##################### steam
#'
#' #' Steam
#' #' Steam
#' #' @name gg_steam_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_steam_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                              yLabel =  NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[2]
#'   clab <- fillLabel %||% nms[1]
#'   xlab <- xLabel %||% "ndice"
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::mutate(xorder = 1:n()) %>%
#'     tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
#'   data_graph[is.na(data_graph)] <- 0
#'   data_graph$xorder <- as.numeric(data_graph$xorder)
#'
#'   graph <- ggplot(data_graph, aes(x = xorder, y = b, group = a, fill = a)) +
#'     stat_steamgraph() +
#'     labs(tittle = titleLabel, x = xlab, y = ylab, fill = clab) +
#'     scale_fill_manual(values = getPalette()) + theme_ds()
#'   graph <- graph + theme(legend.position=leg_pos) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#'
#' #' Steam
#' #' Steam
#' #' @name gg_steam_CatYeaNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Yea-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_steam_CatYeaNum. <-  function(data, titleLabel = "",  subtitle = "", caption = "", xLabel = NULL,
#'                                  yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   clab <- fillLabel %||% nms[1]
#'   xlab <- xLabel %||% nms[2]
#'   ylab <- yLabel %||% nms[3]
#'   data <- f$d
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a) %>%
#'     tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
#'   data_graph[is.na(data_graph)] <- 0
#'   data_graph$b <- as.numeric(data_graph$b)
#'
#'   graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
#'     stat_steamgraph() +
#'     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'     scale_fill_manual(values = getPalette()) +
#'     theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)  +
#'     theme(legend.position = leg_pos) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Steam
#' #' Steamgraph
#' #' @name gg_steam_CatNumNum.
#' #' @param x A category.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_steam_CatNumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                 yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, aggregation = "sum", ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[2]
#'   ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
#'   clab <- fillLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b), !is.na(c))
#'
#'   data_graph <- data %>%
#'     dplyr::group_by(a, b) %>%
#'     dplyr::summarise(c = agg(aggregation, c)) %>%
#'     tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
#'
#'   data_graph[is.na(data_graph)] <- 0
#'   data_graph$b <- as.numeric(data_graph$b)
#'   data_graph$c <- as.numeric(data_graph$c)
#'
#'   graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
#'     stat_steamgraph() + theme_ds() +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     scale_fill_manual(values = getPalette()) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
#'     theme(legend.position = leg_pos)
#'
#'   graph
#' }
#'
#' #' Steam
#' #' Steam
#' #' @name gg_steam_CatDatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Dat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_steam_CatDatNum. <- function (data, titleLabel = "", subtitle = "", caption = "",
#'                                  xLabel = NULL, yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
#'
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[2]
#'   ylab <- yLabel %||% nms[3]
#'   clab <- fillLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b), !is.na(c))
#'
#'   data_graph <- data %>%
#'     tidyr::drop_na(a,b) %>%
#'     dplyr::group_by(a) %>%
#'     tidyr::spread(b, c) %>%
#'     tidyr::gather(b, c, -a)
#'
#'   data_graph[is.na(data_graph)] <- 0
#'
#'   graph <- ggplot(data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a, fill = a)) +
#'     stat_steamgraph() +
#'     theme_ds() +
#'     theme(axis.text.x = element_text(angle = angle_x,hjust = 1)) +
#'     scale_fill_manual(values = getPalette()) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
#'     theme(legend.position = leg_pos)
#'   graph
#' }
#'
#' ################################################
#'
#' ########################### SUNBURST
#'
#' #' Sunburst
#' #' sunburst
#' #' @name gg_sunburst_CatCatCatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Cat-Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_sunburst_CatCatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", aggregation = "sum",
#'                                       fillLabel = NULL, xLabel = NULL, yLabel = NULL, leg_pos = 'right', ...){
#'
#'
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   flabel <- fillLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
#'                                  b = ifelse(is.na(b), "NA", b),
#'                                  c = ifelse(is.na(c), "NA", c)) %>%
#'     dplyr::filter(!is.na(d))
#'
#'   data$a <- as.factor(data$a)
#'   data$b <- as.factor(data$b)
#'   data$c <- as.factor(data$c)
#'
#'   #angulos
#'
#'   pred_ang <-  function(perc){
#'     angle = -1
#'
#'     if(perc < 0.25) # 1st q [90,0]
#'       angle = 90 - (perc/0.25) * 90
#'     else if(perc < 0.5) # 2nd q [0, -90]
#'       angle = (perc-0.25) / 0.25 * -90
#'     else if(perc < 0.75) # 3rd q [90, 0]
#'       angle = 90 - ((perc-0.5) / 0.25 * 90)
#'     else if(perc < 1.00) # last q [0, -90]
#'       angle = ((perc -0.75)/0.25) * -90
#'
#'     if(perc < 0.5) # 1st half [90, -90]
#'       angle = (180 - (perc/0.5) * 180) - 90
#'     else # 2nd half [90, -90]
#'       angle = (90 - ((perc - 0.5)/0.5) * 180)
#'
#'     return(angle)
#'   }
#'
#'   #primer nivel
#'
#'   part1 <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(total1 = agg(aggregation, d)) %>%
#'     dplyr::mutate(running = cumsum(total1), pos = running - total1/2) %>%
#'     dplyr::group_by(1:n()) %>%
#'     dplyr::mutate(angle = pred_ang((pos)/total1)) %>%
#'     dplyr::arrange(a, -total1)
#'
#'   sunb0 <- ggplot(part1)
#'   sunb1 <- sunb0 +
#'     geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
#'     geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle), check_overlap = TRUE) +
#'     scale_fill_continuous(low = '#009EE3', high = '#E5007D')
#'
#'   #segundo nivel
#'
#'   part2 <- data %>%
#'     dplyr::group_by(a,b) %>%
#'     dplyr::summarise(total1 = agg(aggregation, d)) %>%
#'     ungroup() %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::arrange(a,-total1) %>%
#'     ungroup() %>%
#'     mutate(running = cumsum(total1), pos = running - total1/2) %>%
#'     group_by(1:n()) %>%
#'     mutate(angle = pred_ang((running - total1/2)/total1))
#'
#'
#'   sunb2 <- sunb1 +
#'     geom_bar(data = na.omit(part2), aes(x=2, y = total1,  fill = total1),na.rm = TRUE,stat = 'identity', color = 'white', position = 'stack') +
#'     geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle), check_overlap = TRUE)
#'
#'
#'
#'   #tercer nivel
#'
#'   part3 <- data %>%
#'     #tidyr::drop_na(c) %>%
#'     dplyr::group_by(a,b,c) %>%
#'     dplyr::summarise(total1 = agg(aggregation, d)) %>%
#'     dplyr::arrange(a,c,-total1) %>%
#'     ungroup() %>%
#'     mutate(running = cumsum(total1), pos = running - total1/2) %>%
#'     group_by(1:n()) %>%
#'     mutate(angle = pred_ang((running - total1/2)/total1))
#'
#'   part3$total1[is.na(part3$c)] <- NA
#'
#'
#'   graph <- sunb2 +
#'     geom_bar(data = part3, aes(x=3, y = total1,  fill = total1), stat = 'identity', color = 'white', position = 'stack') +
#'     geom_text(data = part3, aes(label=part3$c, x=3, y=pos, angle=angle), check_overlap = TRUE)
#'
#'   graph +
#'     coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel)
#'
#' }
#'
#'
#' #' Sunburst
#' #' sunburst
#' #' @name gg_sunburst_CatCatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_sunburst_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
#'                                    fillLabel = NULL, aggregation = "sum", ...){
#'
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[2]
#'   ylab <- yLabel %||% nms[3]
#'   flabel <- fillLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
#'                                  b = ifelse(is.na(b), "NA", b)) %>%
#'     dplyr::filter(!is.na(c))
#'
#'   #angulos
#'
#'   pred_ang <-  function(perc){
#'     angle = -1
#'
#'     if(perc < 0.25) # 1st q [90,0]
#'       angle = 90 - (perc/0.25) * 90
#'     else if(perc < 0.5) # 2nd q [0, -90]
#'       angle = (perc-0.25) / 0.25 * -90
#'     else if(perc < 0.75) # 3rd q [90, 0]
#'       angle = 90 - ((perc-0.5) / 0.25 * 90)
#'     else if(perc < 1.00) # last q [0, -90]
#'       angle = ((perc -0.75)/0.25) * -90
#'
#'     if(perc < 0.5) # 1st half [90, -90]
#'       angle = (180 - (perc/0.5) * 180) - 90
#'     else # 2nd half [90, -90]
#'       angle = (90 - ((perc - 0.5)/0.5) * 180)
#'
#'     return(angle)
#'   }
#'
#'   #primer nivel
#'
#'   part1 <- data %>%
#'     dplyr::group_by(a) %>%
#'     dplyr::summarise(total1 = agg(aggregation, c))
#'
#'
#'
#'   part1 <- part1 %>%
#'     mutate(running = cumsum(total1), pos = running - total1/2) %>%
#'     group_by(1:n()) %>%
#'     mutate(angle = pred_ang((pos)/total1))
#'
#'   sunb0 <- ggplot(part1)
#'   sunb1 <- sunb0 +
#'     geom_bar(data = part1, aes(x=1, y = total1, fill = total1 ),stat = 'identity', color = 'white', position = 'stack') +
#'     geom_text(data = part1, aes(label=part1$a, x=1, y=pos, angle=angle), check_overlap = TRUE) +
#'     scale_fill_continuous(low = '#009EE3', high = '#E5007D')
#'
#'   #segundo nivel
#'
#'   cols_col <- data %>%
#'     dplyr::group_by(a,b) %>%
#'     dplyr::summarise(total1 = agg(aggregation, c))
#'
#'
#'   part2 <- cols_col %>%
#'     ungroup(a,b) %>%
#'     mutate(running = cumsum(total1), pos = running - total1/2) %>%
#'     group_by(1:n()) %>%
#'     mutate(angle = pred_ang((running - total1/2)/total1))
#'
#'   sunb2 <- sunb1 +
#'     geom_bar(data = part2, aes(x=2, y = total1,  fill = total1),stat = 'identity', color = 'white', position = 'stack') +
#'     geom_text(data = part2, aes(label=part2$b, x=2, y=pos, angle=angle), check_overlap = TRUE)
#'
#'
#'   graph <- sunb2 + coord_polar('y') +  theme_ds_clean() + guides(fill = FALSE) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
#'
#'   graph
#'
#' }
#'
#'
#' ##########################################
#' ####################################### violin
#'
#' #' Vertical violin
#' #' Violin
#' #' @name gg_violin_mult_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_violin_mult_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                    yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
#'     geom_violin(show.legend = FALSE)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal violin
#' #' Violin multi flipped
#' #' @name gg_violin_mult_flip_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_violin_mult_flip_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                         yLabel = NULL, leg_pos = 'right', angle_x = 0,...){
#'
#'   graph <- gg_violin_mult_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Vertical violin + jitter
#' #' Violin + dot jitter
#' #' @name gg_violin_dot_mult_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_violin_dot_mult_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                        yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot(data, mapping = aes(x = a, y = b, fill = a)) +
#'     geom_jitter(color = "#D55E00", show.legend = FALSE) + geom_violin(show.legend = FALSE)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette())
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     theme(legend.position=leg_pos)
#'
#'   graph
#' }
#'
#' #' Horizontal violin + jitter
#' #' Violin + dot jitter flipped
#' #' @name gg_violin_dot_mult_flip_CatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Cat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_violin_dot_mult_flip_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                             yLabel = NULL, leg_pos = 'right', angle_x = 0, ...){
#'
#'   graph <- gg_violin_dot_mult_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)
#'   graph <- graph + coord_flip()
#'
#'   return(graph)
#'
#' }
#'
#' #' Horizontal violin
#' #' Violin
#' #' @name gg_violin_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#'
#' gg_violin_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
#'                            xLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% "ndice"
#'   ylab <- yLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   data_graph <- data %>%
#'     dplyr::mutate(order = rep(1, nrow(data)))
#'
#'   graph <- ggplot(data_graph, aes(factor(""), a)) + geom_violin(aes(fill = ""), show.legend = FALSE)
#'   graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
#'   graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Vertical violin
#' #' Violin flipped
#' #' @name gg_violin_flip_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_violin_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", yLabel = NULL,
#'                                 xLabel = NULL, angle_x = 0, ...){
#'
#'   graph <- gg_violin_Num.(data, titleLabel, subtitle, caption, yLabel, xLabel, angle_x = 0, ...)
#'   graph <- graph + coord_flip()
#'
#'   graph
#' }
#'
#' #' Violin
#' #' Violin
#' #' @name gg_violin_DatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Dat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_violin_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
#'                               xLabel = NULL, yLabel = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>%
#'     dplyr::filter(!is.na(a), !is.na(b))
#'
#'   graph <- ggplot(data) + geom_violin(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
#'     scale_fill_manual(values = getPalette()) +
#'     scale_y_continuous(labels = comma) +
#'     theme_ds() +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
#'   graph
#' }
#'
#'
#'
#' #########################################################################
#' ########################## waterfall
#'
#' #' Waterfall
#' #' Waterfall
#' #' @name gg_waterfall_Num.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_waterfall_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                               yLabel =  NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% "ndice"
#'   ylab <- yLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a))
#'
#'   data_graph <- data %>% mutate(xorder = 1:nrow(.))
#'   graph <- ggplot_waterfall(data_graph, 'xorder', 'a') +
#'     scale_color_manual(breaks = c("+","-", ""), values = getPalette()) +
#'     theme_ds() + theme(legend.position="none") +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'
#'
#'   graph
#' }
#'
#'
#' #' Waterfall
#' #' Waterfall
#' #' @name gg_waterfall_YeaNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Yea-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_waterfall_YeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                  yLabel =  NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[2]
#'   xlab <- xLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
#'     dplyr::filter(!is.na(b))
#'
#'   graph <- ggplot_waterfall(data,'a','b') + theme_ds() + theme(legend.position="none") +
#'     scale_color_manual(breaks = c("+",  "-", ""), values = getPalette()) +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#'
#' #' Waterfall
#' #' Waterfall
#' #' @name gg_waterfall_NumNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Num-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_waterfall_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                  yLabel =  NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[2]
#'   xlab <- xLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>% dplyr::filter(!is.na(a), !is.na(b))
#'
#'   graph <- ggplot_waterfall(data,'a','b')  +
#'     scale_color_manual(breaks = c("+", "-", ""), values = getPalette()) +
#'     theme_ds() + theme(legend.position="none")+
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'   graph
#' }
#' #' Waterfall
#' #' Waterfall
#' #' @name gg_waterfall_DatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Dat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_waterfall_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                                  yLabel =  NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   ylab <- yLabel %||% nms[2]
#'   xlab <- xLabel %||% nms[1]
#'   data <- f$d
#'
#'   data <- data %>%
#'     dplyr::filter(!is.na(a), !is.na(b))
#'
#'   graph <- ggplot_waterfall(data,'a','b') +
#'     scale_color_manual(values = getPalette()) +
#'     theme_ds() +
#'     theme(legend.position="none") +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   graph
#' }
#'
#' #' Kagi
#' #' Kagi
#' #' @name gg_kagi_DatNum.
#' #' @param x A number.
#' #' @param y A number.
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Dat-Num
#' #' @examples
#' #' add(1, 1)
#' #' add(10, 1)
#' gg_kagi_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
#'                             yLabel = NULL, hline = NULL, angle_x = 0, ...){
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   xlab <- xLabel %||% nms[1]
#'   ylab <- yLabel %||% nms[2]
#'   data <- f$d
#'
#'   data <- data %>%
#'     dplyr::filter(!is.na(a), !is.na(b))
#'
#'   graph <- ggplot(data, aes(x = a, y = b)) +
#'     geom_line(aes(color=ifelse(c(diff(b), NA) > 0, "Gain", "Loss"), group = NA)) +
#'     scale_color_manual(guide="none",values = getPalette()) +
#'     theme_ds() +
#'     labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
#'     theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
#'
#'   if(!is.null(hline)){
#'     graph <- graph + geom_hline(data = data.frame(valores = hline),
#'                                 aes(yintercept = valores), linetype="dotted")
#'   }
#'
#'   return(graph)
#' }
#'
#'
#'
