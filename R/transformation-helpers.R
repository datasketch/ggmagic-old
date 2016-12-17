

agg <- function(aggregation,...){
  f <- NULL
  if(aggregation == "sum")
    f <- sum(..., na.rm = TRUE)
  if(aggregation == "mean")
    f <- mean(...,na.rm = TRUE)
  f
}

