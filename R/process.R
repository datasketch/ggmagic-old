

summarizeData <- function(df, agg, to_agg, ...) {
  df %>%
    dplyr::group_by(...) %>%
    dplyr::summarise({{to_agg}} := agg(agg, {{to_agg}}))
}
agg <- function(aggregation, ...) {
  if (!is.null(aggregation) | nchar(aggregation) > 0 | !is.na(aggregation)) {
    do.call(aggregation, list(..., na.rm = TRUE))
  }
}

# labels position
#'@export
labelPosition <- function(data, col, labelRatio = 0.5,  zeroToNa = FALSE) {

  half <- data[[col]] - data[[col]] / 2
  small <- half < max(data[[col]] * labelRatio, na.rm = TRUE)
  small <- which(small)
  half[small] <- data[[col]][small] + max(data[[col]], na.rm = TRUE) / 50
  data$..labpos <- half
  # do I want zero labels to be shown?
  if (zeroToNa) {
    data$..labpos[data[[col]] == 0] <- NA
  }
  data
}
