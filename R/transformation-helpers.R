

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
  data[[col]] <- factor(stringr::str_wrap(data[[col]], labelWrap), levels = unique(stringr::str_wrap(data[[col]], labelWrap)))
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
labelPosition <- function(data, col, labelRatio, percentage = FALSE, zeroToNa = FALSE) {
  col <- ifelse(percentage, "percent", col)
  half <- data[[col]] - data[[col]] / 2
  small <- half < max(data[[col]] * labelRatio, na.rm = TRUE)
  small <- which(small)
  half[small] <- data[[col]][small] + max(data[[col]], na.rm = TRUE) / 50
  data$labPos <- half
  # do I want zero labels to be shown?
  if (zeroToNa) {
    data$labPos[data[[col]] == 0] <- NA
  }
  data
}

#' select discrete default color
#' @export

discreteColorSelect <- function (colorDefault, d) {
  lengData <- length(unique(d$a))
  lengColor <- length(colorDefault)
  if (lengData == lengColor) {
    colorDefault <- colorDefault
  } else if (lengData > lengColor) {
    colorDefault <- c(colorDefault, sample(colorDefault, lengData-lengColor,replace = TRUE))
  } else {
    colorDefault <- colorDefault[1:lengData]
  }
  colorDefault
}

# colores
#' @export
fillColors <- function(data, col, colors, color_scale, highlightValue, highlightValueColor, labelWrap) {
  # cat <- stringr::str_wrap(unique(data[[col]]), labelWrap)
  # cat <- unique(data[[col]])
  cat <- levels(unique(data[[col]]))
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  ds <- dsColorsHex(TRUE)
  if (!is.null(colors)) {
    cl <- col2rgb(colors)
    colors <- map_chr(1:ncol(cl), function(s) {
      rgb(cl[1, s],
          cl[2, s],
          cl[3, s],
          maxColorValue = 255)
    })
  }
  if (color_scale == "no") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[2]
    }
    fillCol <- rep(colors[1], length(cat))[1:length(cat)]
    names(fillCol) <- cat
  }

  if (color_scale == "discrete") {
    if (is.null(colors)) {
      colors <- dsColorsHex()
    }
    ad <- unlist(map(colors, function(y) {
      l0 <- ds[((grep(substr(y, 2, 2), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      l1 <- paste0(substr(y, 1, 1), l0, substr(y, 3, 7))
      # p0 <- ds[((grep(substr(l1, 4, 4), ignore.case = TRUE, ds) + 7) %% 16) + 1]
      p0 <- ds[((grep(substr(ifelse(length(colors) > 1, y, l1), 4, 4), ignore.case = TRUE, ds) + 6) %% 16) + 1]
      p1 <- paste0(substr(ifelse(length(colors) > 1, y, l1), 1, 3), p0, substr(ifelse(length(colors) > 1, y, l1), 5, 7))
      # colors <<- c(colors, l1, p1)
      c(l1, p1)
    }))
    # [sample(1:length(cat))]
    fillCol <- c(colors, leaflet::colorFactor(c(colors, ad), cat)(cat)[sample(1:length(cat))])
    names(fillCol) <- cat
  }

  if (color_scale == "continuous") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[c(1, 7, 3, 4)]
    }
    if (length(colors) == 1) {
      l0 <- ds[((grep(substr(colors, 2, 2), ignore.case = TRUE, ds) + 7) %% 16) + 1]
      colors <- c(colors, paste0(substr(colors, 1, 1), l0, substr(colors, 3, 7)))
    }
    fillCol <- leaflet::colorNumeric(colors, 1:length(cat))(1:length(cat))
    names(fillCol) <- cat
  }

  if (!is.null(highlightValue) & sum(highlightValue %in% names(fillCol)) > 0) {
    wh <- which(names(fillCol) %in% highlightValue)
    if (is.null(highlightValueColor)) {
      l0 <- ds[((grep(substr(colors[1], 2, 2), ignore.case = TRUE, ds) + 13) %% 16) + 1]
      highlightValueColor <- paste0(substr(colors[1], 1, 1), l0, substr(colors[1], 3, 7))
    }
    fillCol[wh] <- highlightValueColor
  }
  fillCol
}


