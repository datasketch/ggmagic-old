
load_all()

dataNu <- data.frame(num = rnorm(150))

# Horizon
gg_horizon_Nu.(dataNu)

# Waterfall
gg_waterfall_Nu.(dataNu)

#Histogram + density + dots (with selection options)
gg_hist_Nu.(dataNu)
gg_hist_dens_Nu.(dataNu)
gg_cumm_dist_Nu.(dataNu)
gg_flip_cumm_dist_Nu.(dataNu)

gg_line_Nu.(dataNu)
gg_flip_line_Nu.(dataNu)

gg_scatter_Nu.(dataNu, type=2)
gg_flip_scatter_Nu.(dataNu)

gg_density_hist_Nu.(dataNu)
gg_box_Nu.(dataNu)
gg_flip_box_Nu.(dataNu)

gg_violin_Nu.(dataNu)
gg_flip_violin_Nu.(dataNu)

gg_dot_Nu.(dataNu)
gg_flip_dot_Nu.(dataNu)





##########################################################################################################

### Pausa
# Binned catégoricas para después

#Conversor de num?rico a categ?rico por rangos


toCatRange <- function(dataNu, nCat){

  calRange <- function(a, range, base) base + (a*range)

  getValue <- function(x, data) { #http://stackoverflow.com/questions/24766104/checking-if-value-in-vector-is-in-range-of-values-in-different-length-vector
    tmp <- data %>%
      filter(minRanges <= x, x <= maxRanges)
    return(tmp$range)
  }


  rangeSize <- (max(dataNu) - min(dataNu))/nCat
  maxRanges <- sapply(X = c(1:nCat), FUN =  calRange, range = rangeSize, base = min(dataNu))
  minRanges <- maxRanges - rangeSize
  matchRanges <- data.frame(minRanges, maxRanges)
  matchRanges$range <- paste("[", round(matchRanges$minRanges, 2), " - ", round(matchRanges$maxRanges, 2), "]", sep = "")

  getValue(55, matchRanges)
  result <- sapply(X = dataNu[, 1], FUN = getValue, data = matchRanges)
  result <- data.frame(result); names(result)[1] <- "a"

  return(result)
}


# cut2() #Hmisc




