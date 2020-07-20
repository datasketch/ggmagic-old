

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
