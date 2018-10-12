# aggregation
#'@export
agg <- function(aggregation, ...) {
  if (!is.null(aggregation) | nchar(aggregation) > 0 | !is.na(aggregation)) {
    do.call(aggregation, list(..., na.rm = TRUE))
  }
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
  small <- half < max(data[[col]] * labelRatio)
  half[small] <- data[[col]][small] + max(data[[col]]) / 50
  data$labPos <- half
  # do I want zero labels to be shown?
  if (zeroToNa) {
    data$labPos[data[[col]] == 0] <- NA
  }
  data
}



# colores VIEJO
# #' @export
# fillColors <- function(data, col, colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap) {
#   cat <- stringr::str_wrap(unique(data[[col]]), labelWrap)
#   highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
#   if (diffColorsBar) {
#     fillCol <- rep(colors, length(cat))[1:length(cat)]
#     names(fillCol) <- cat
#     if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
#       wh <- which(cat %in% highlightValue)
#       hg <- cat[wh]
#       rg <- setdiff(cat, cat[wh])
#       colHg <- rep(highlightValueColor, length(hg))
#       if (is.null(highlightValueColor)) {
#         # toca cambiar la opción si colors[2] es null..
#         # sumarle al color inicial, no que sea un valor fijo
#         colHg <-  rep("#F9B233", length(hg))
#       }
#       colRg <- rep(colors, length(rg))[1:length(rg)]
#       fillCol <- c(colHg, colRg)
#       names(fillCol) <- c(hg, rg)
#     }
#   } else {
#     fillCol <- rep(colors[1], length(cat))
#     names(fillCol) <- cat
#     if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
#       wh <- which(cat %in% highlightValue)
#       hg <- cat[wh]
#       rg <- setdiff(cat, cat[wh])
#       colHg <- rep(highlightValueColor, length(hg))
#       if (is.null(highlightValueColor)) {
#         # toca cambiar la opción si colors[2] es null..
#         # sumarle al color inicial, no que sea un valor fijo
#         colHg <-  rep("#F9B233", length(hg))
#       }
#       colRg <- rep(colors[1], length(rg))
#       fillCol <- c(colHg, colRg)
#       names(fillCol) <- c(hg, rg)
#     }
#   }
#   fillCol
# }


# colores
#' @export
fillColors <- function(data, col, colors, colorScale, highlightValue, highlightValueColor, labelWrap) {
  cat <- stringr::str_wrap(unique(data[[col]]), labelWrap)
  highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
  ds <- dsColorsHex(TRUE)

  if (colorScale == "no") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[2]
    }
    fillCol <- rep(colors, length(cat))[1:length(cat)]
    names(fillCol) <- cat
  }

  if (colorScale == "discrete") {
    if (is.null(colors)) {
      colors <- dsColorsHex()
    }
    map(colors, function(y) {
      l0 <- ds$b[grep(substr(y, 4, 4), ignore.case = TRUE, ds$a)]
      colors <<- c(colors, paste0(substr(y, 1, 3), l0, substr(y, 5, 7)))
    })
    fillCol <- colorNumeric(colors, 1:length(cat))(1:length(cat))
    names(fillCol) <- cat
  }

  if (colorScale == "continuous") {
    if (is.null(colors)) {
      colors <- dsColorsHex()[c(1, 7, 3, 4)]
    }
    if (length(colors) == 1) {
      l0 <- ds[(grep(substr(colors, 2, 2), ignore.case = TRUE, ds) + 7) %% 16]
      colors <- c(colors, paste0(substr(colors, 1, 1), l0, substr(y, 3, 7)))
    }
    fillCol <- colorNumeric(colors, 1:length(cat))(1:length(cat))
    names(fillCol) <- cat
  }

  if (!is.null(highlightValue) & sum(highlightValue %in% names(fillCol)) > 0) {
    wh <- which(names(fillCol) %in% highlightValue)
    # hg <- names(fillCol)[wh]
    if (is.null(highlightValueColor)) {
      l0 <- ds[(grep(substr(colors[1], 2, 2), ignore.case = TRUE, ds) + 16) %% 16]
      highlightValueColor <- paste0(substr(colors[1], 1, 1), l0, substr(colors[1], 3, 7))
    }
    fillCol[wh] <- highlightValueColor
  }

  fillCol
}
# fillColors <- function(data, col, colors, diffColorsBar, highlightValue, highlightValueColor, labelWrap) {
#   cat <- stringr::str_wrap(unique(data[[col]]), labelWrap)
#   highlightValue <- stringr::str_wrap(highlightValue, labelWrap)
#   if (diffColorsBar) {
#     # este fillCol es dependiendo de la columna categórica
#     fillCol <- colorNumeric(colors, 1:length(cat))(1:length(cat))[sample(length(cat))]
#     # este fillCol es dependiendo de la columna numérica
#     # fillCol <- colorNumeric(colors, 1:max(b, na.rm = TRUE))(b))
#     names(fillCol) <- cat
#     if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
#       wh <- which(cat %in% highlightValue)
#       hg <- cat[wh]
#       rg <- setdiff(cat, cat[wh])
#       colHg <- rep(highlightValueColor, length(hg))
#       if (is.null(highlightValueColor)) {
#         # toca cambiar la opción si colors[2] es null..
#         # sumarle al color inicial, no que sea un valor fijo
#         colHg <-  rep("#F9B233", length(hg))
#       }
#       # colRg <- rep(colors, length(rg))[1:length(rg)]
#       colRg <- colorNumeric(colors, 1:length(rg))(1:length(rg))#[sample(length(rg))]
#       fillCol <- c(colHg, colRg)
#       names(fillCol) <- c(hg, rg)
#     }
#   } else {
#     fillCol <- rep(colors, length(cat))[1:length(cat)]
#     names(fillCol) <- cat
#     if (!is.null(highlightValue) & sum(highlightValue %in% cat) > 0) {
#       wh <- which(cat %in% highlightValue)
#       hg <- cat[wh]
#       rg <- setdiff(cat, cat[wh])
#       colHg <- rep(highlightValueColor, length(hg))
#       if (is.null(highlightValueColor)) {
#         # toca cambiar la opción si colors[2] es null..
#         # sumarle al color inicial, no que sea un valor fijo
#         colHg <-  rep("#F9B233", length(hg))
#       }
#       colRg <- rep(colors, length(rg))[1:length(rg)]
#       fillCol <- c(colHg, colRg)
#       names(fillCol) <- c(hg, rg)
#     }
#   }
#   fillCol
# }


# ds palette
#' @export
dsColorsHex <- function(hex = FALSE) {
  if (hex) {
    c <- c(0:9, "A", "B", "C", "D", "E")

  } else {
    c <- c("#2E0F35", "#74D1F7", "#B70F7F", "#C2C4C4", "#8097A4",  "#A6CEDE", "#801549",
           "#FECA84", "#ACD9C2", "#EEF1F2")
  }
}

