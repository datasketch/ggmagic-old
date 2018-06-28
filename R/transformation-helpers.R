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
orderCategory <- function(data, col, order, labelWrap) {
  data[[col]] <- str_wrap(data[[col]], labelWrap)
  if (!is.null(order)) {
    order <- str_wrap(order, labelWrap)
    order <- union(order, unique(data[[col]])[!is.na(unique(data[[col]]))])
    if (all(!is.na(order)) & any(is.na(data[[col]]))) order <- c(union(order, unique(data[[col]][!is.na(data[[col]])])), NA)
    order[is.na(order)] <- "NA"
    data <- data[order(match(data[[col]], order)), ]
  }
  data
}


# converts a numeric column into the equivalent percentage column
#'@export
percentColumn <- function(data, col, percentage = TRUE, nDigits = nDigits) {
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
sortSlice <- function(data, col, sort, sliceN) {
  if (sort == "asc") {
    data <- data %>%
      dplyr::arrange_(col)
  }
  if (sort == "desc") {
    col <- paste0('desc(', col, ')')
  data <- data %>%
    dplyr::arrange_(.dots = col)
  }
  if (!is.null(sliceN)) {
    data <- data %>%
    dplyr::slice(1:sliceN)
  }
  data
}

# highlight value
#'@export
highlightValueData <- function(data, col, highlightValue, color, highlightColor) {
  data$color <- color
  if (!is.null(highlightValue)) {
    w <- which(data[[col]] == highlightValue)
    data$color[w] <- highlightColor
  }
  data
}

# labels position
#'@export
labelPos <- function(data, col, labelRatio) {
  half <- data[[col]] - data[[col]] / 2
  small <- half < max(data[[col]] * labelRatio)
  half[small] <- data[[col]][small] + max(data[[col]]) / 50
  half
}



# fill color =------ highlightvalue... cuando es max o min...
# fillColors <- function(data, col, colors, highlightValue, order) {
#   f <- unique(data[[col]])
#   if (length(f) < length(colors)) {
#
#   } else if (length(f) == length(colors)) {
#
#   } else {
#     if (length(colors) <= 2) {
#
#     } else {
#       if (!is.null(highlightValue)) {
#
#     }
#
#     }
#   }
#
# }
