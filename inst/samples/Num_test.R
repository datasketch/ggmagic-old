
load_all()
document()

dataNum <- data.frame(num = rnorm(150))

# Horizon
gg_horizon_Num.(dataNum)

# Waterfall
gg_waterfall_Num.(dataNum)

#Histogram + density + dots (with selection options)
gg_hist_Num.(dataNum)
gg_hist_dens_Num.(dataNum)
gg_dist_cum_Num.(dataNum)

#gg_cumm_dist_flip_Num.(dataNum)

gg_line_point_Num.(dataNum)
gg_line_point_flip_Num.(dataNum)

gg_point_Num.(dataNum, type=2)
gg_point_flip_Num.(dataNum)

gg_density_hist_Num.(dataNum)
gg_box_Num.(dataNum)
gg_box_flip_Num.(dataNum)

gg_violin_Num.(dataNum)
gg_violin_flip_Num.(dataNum)

gg_dot_bar_Num.(dataNum)
gg_dot_bar_flip_Num.(dataNum)





##########################################################################################################

### Pausa
# Binned catégoricas para después

#Conversor de num?rico a categ?rico por rangos


toCatRange <- function(dataNum, nCat){

  calRange <- function(a, range, base) base + (a*range)

  getValue <- function(x, data) { #http://stackoverflow.com/questions/24766104/checking-if-value-in-vector-is-in-range-of-values-in-different-length-vector
    tmp <- data %>%
      filter(minRanges <= x, x <= maxRanges)
    return(tmp$range)
  }


  rangeSize <- (max(dataNum) - min(dataNum))/nCat
  maxRanges <- sapply(X = c(1:nCat), FUN =  calRange, range = rangeSize, base = min(dataNum))
  minRanges <- maxRanges - rangeSize
  matchRanges <- data.frame(minRanges, maxRanges)
  matchRanges$range <- paste("[", round(matchRanges$minRanges, 2), " - ", round(matchRanges$maxRanges, 2), "]", sep = "")

  getValue(55, matchRanges)
  result <- sapply(X = dataNum[, 1], FUN = getValue, data = matchRanges)
  result <- data.frame(result); names(result)[1] <- "a"

  return(result)
}


# cut2() #Hmisc




