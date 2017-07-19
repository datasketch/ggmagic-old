#' Vertical marimekko
#' Vertical Marimekko
#' @name gg_marimekko_ver_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_ver_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  # xvar <- deparse(substitute(a))
  # yvar <- deparse(substitute(b))
  # mytable <- table(data)
  # widths <- c(0, cumsum(apply(mytable, 1, sum)))
  # heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))})
  #
  # alldata <- data.frame()
  # allnames <- data.frame()
  # for(i in 1:nrow(mytable)){
  #   for(j in 1:ncol(mytable)){
  #     alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]))
  #   }
  # }
  # colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  #
  # alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)))
  # alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable))
  #
  # xxmax <- max(alldata$xmin, alldata$xmax)
  # alldata <- alldata %>% dplyr::mutate(xmin = xmin/xxmax, xmax = xmax/xxmax)
  #text <- alldata %>% dplyr::group_by(a) %>% dplyr::summarise(xtext = (max(xmax) + max(xmin)) / 2) %>% dplyr::mutate(ytext = 1.05)

  var1 <- as.factor(data$a)
  var2 <- as.factor(data$b)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))

  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  graph <- ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) +
    labs(x = xLabel, y = yLabel, title = titleLabel, subtitle = subtitle, caption = caption, fill = clab)

  # graph <- ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  #   geom_rect(color="black", aes_string(fill=yvar))
  graph <- graph + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + scale_fill_manual(values = getPalette()) +
    theme(legend.position = leg_pos) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = percent)


  graph
}

#' Horizontal marimekko
#' Horizontal Marimekko
#' @name gg_marimekko_hor_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_marimekko_hor_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){
  graph <- gg_marimekko_ver_CatCat.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()
  graph
}
