# aggregation
#'@export
agg <- function(aggregation,...){
  f <- NULL
  if(aggregation == "sum")
    f <- sum(..., na.rm = TRUE)
  if(aggregation == "mean")
    f <- mean(..., na.rm = TRUE)
  if(aggregation == "median")
    f <- median(...,na.rm = TRUE)
  f
}

# defines horLabel and verLabel depending orientation
#'@export
orientationXY <- function(orientation, x, y, hor, ver, line = FALSE) {
  if (orientation == "hor") {
    x0 <- ver %||% x
    y0 <- hor %||% y
  } else {
    x0 <- hor %||% x
    y0 <- ver %||% y
  }
  if (line) {
    x0 <- hor %||% x
    y0 <- ver %||% y
    if (orientation != "hor") {
      x1 <- x0
      x0 <- y0
      y0 <- x1
    }
  }
  c(x0, y0)
}

# order category column
#'@export
orderCategory <- function(data, col, orientation, order, labelWrap) {
  data[[col]] <- stringr::str_wrap(data[[col]], labelWrap)
  if (!is.null(order)) {
    order <- stringr::str_wrap(order, labelWrap)
    order <- union(order, unique(data[[col]])[!is.na(unique(data[[col]]))])
    if (all(!is.na(order)) & any(is.na(data[[col]]))) order <- c(union(order, unique(data[[col]][!is.na(data[[col]])])), NA)
    order[is.na(order)] <- "NA"
    data[[col]] <- factor(data[[col]], levels = unique(data[[col]][order(match(data[[col]], order))]))
    if (orientation == "hor") {
      data[[col]] <- factor(data[[col]], levels = rev(attr(data[[col]], "levels")))
    }
  }
  data
}

# converts a numeric column into the equivalent percentage column
#'@export
percentColumn <- function(data, col, percentage = TRUE, nDigits = 2) {
  if (percentage) {
    #data$percent
    data[[col]] <- round((data[[col]] * 100) / sum(data[[col]], na.rm = TRUE),
                         #esto puede ser variable dep el format...
                         digits = nDigits)
  } else {
    data[[col]] <- round(data[[col]], digits = nDigits)
  }
  data
}

# sort and slice
#'@export
sortSlice <- function(data, col, colOrder, orientation, sort, sliceN) {
  if (sort == "asc") {
    data <- data %>%
      dplyr::arrange_(col)
    data[[colOrder]] <- factor(data[[colOrder]], levels = unique(data[[colOrder]]))
  }
  if (sort == "desc") {
    col <- paste0('desc(', col, ')')
    data <- data %>%
      dplyr::arrange_(.dots = col)
     data[[colOrder]] <- factor(data[[colOrder]], levels = unique(data[[colOrder]]))
  }
  if (orientation == "hor") {
    data[[colOrder]] <- factor(data[[colOrder]], levels = rev(unique(data[[colOrder]])))
  }
  if (!is.null(sliceN)) {
    data <- data %>%
      dplyr::slice(1:sliceN)
  }
  data
}


# labels position
#'@export
labelPosition <- function(data, col, labelRatio) {
  half <- data[[col]] - data[[col]] / 2
  small <- half < max(data[[col]] * labelRatio)
  half[small] <- data[[col]][small] + max(data[[col]]) / 50
  data$labPos <- half
  data
}



# colores
#' @export
fillColors <- function(data, col, colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap) {
  cat <- stringr::str_wrap(unique(data[[col]]), labelWrap)
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  if (diffColorsBar) {
    fillCol <- rep(colors, length(cat))[1:length(cat)]
    names(fillCol) <- cat
    if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
      wh <- which(cat %in% highlightValue)
      hg <- cat[wh]
      rg <- setdiff(cat, cat[wh])
      colHg <- rep(highlightValueColor, length(hg))
      if (is.null(highlightValueColor)) {
        # toca cambiar la opción si colors[2] es null..
        # sumarle al color inicial, no que sea un valor fijo
        colHg <-  rep("#F9B233", length(hg))
      }
      colRg <- rep(colors, length(rg))[1:length(rg)]
      fillCol <- c(colHg, colRg)
      names(fillCol) <- c(hg, rg)
    }
  } else {
    fillCol <- rep(colors[1], length(cat))
    names(fillCol) <- cat
    if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
      wh <- which(cat %in% highlightValue)
      hg <- cat[wh]
      rg <- setdiff(cat, cat[wh])
      colHg <- rep(highlightValueColor, length(hg))
      if (is.null(highlightValueColor)) {
        # toca cambiar la opción si colors[2] es null..
        # sumarle al color inicial, no que sea un valor fijo
        colHg <-  rep("#F9B233", length(hg))
      }
      colRg <- rep(colors[1], length(rg))
      fillCol <- c(colHg, colRg)
      names(fillCol) <- c(hg, rg)
    }
  }
  fillCol
}


# colores
#' @export
fillColors <- function(data, col, colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap) {
  cat <- stringr::str_wrap(unique(data[[col]]), labelWrap)
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  if (diffColorsBar) {
    # este fillCol es dependiendo de la columna categórica
    fillCol <- colorNumeric(colors, 1:length(cat))(1:length(cat))[sample(length(cat))]
    # este fillCol es dependiendo de la columna numérica
    # fillCol <- colorNumeric(colors, 1:max(b, na.rm = TRUE))(b))
    names(fillCol) <- cat
    if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
      wh <- which(cat %in% highlightValue)
      hg <- cat[wh]
      rg <- setdiff(cat, cat[wh])
      colHg <- rep(highlightValueColor, length(hg))
      if (is.null(highlightValueColor)) {
        # toca cambiar la opción si colors[2] es null..
        # sumarle al color inicial, no que sea un valor fijo
        colHg <-  rep("#F9B233", length(hg))
      }
      # colRg <- rep(colors, length(rg))[1:length(rg)]
      colRg <- colorNumeric(colors, 1:length(rg))(1:length(rg))[sample(length(rg))]
      fillCol <- c(colHg, colRg)
      names(fillCol) <- c(hg, rg)
    }
  } else {
    fillCol <- rep(colors, length(cat))[1:length(cat)]
    names(fillCol) <- cat
    if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
      wh <- which(cat %in% highlightValue)
      hg <- cat[wh]
      rg <- setdiff(cat, cat[wh])
      colHg <- rep(highlightValueColor, length(hg))
      if (is.null(highlightValueColor)) {
        # toca cambiar la opción si colors[2] es null..
        # sumarle al color inicial, no que sea un valor fijo
        colHg <-  rep("#F9B233", length(hg))
      }
      colRg <- rep(colors, length(rg))[1:length(rg)]
      fillCol <- c(colHg, colRg)
      names(fillCol) <- c(hg, rg)
    }
  }
  fillCol
}



