
# aggregation
agg <- function(aggregation, ...) {
  if (!is.null(aggregation) | nchar(aggregation) > 0 | !is.na(aggregation)) {
    do.call(aggregation, list(..., na.rm = TRUE))
  }
}

preprocessData <- function(d,opts){
  if (opts$drop_na){
    d <- d %>% tidyr::drop_na()
  }
  d
}


summarizeData <- function(df,agg, to_agg, ...) {
  group_var <- enquos(...)
  summ_var <- enquo(to_agg)
  df %>%
    group_by(!!! group_var) %>%
    summarise(!! summ_var := agg(agg, !! summ_var))
}


