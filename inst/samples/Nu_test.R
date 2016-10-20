
load_all()

dataNu <- data.frame(rnorm(150))
names(dataNu)[1] <- "a"

# Horizon
gg_horizon_Nu.(data.frame(rf = dfData$y))

# Waterfall
gg_waterfall_Nu.(dataNu)

#Histogram + density + dots (with selection options)
hist_graph(dataNu)
hist_dens_graph(dataNu)
cumm_prob_graph(dataNu)
flip_cumm_prob_graph(dataNu)

line_graph(dataNu)
flip_line_graph(dataNu)

scatter_graph(dataNu, type=2)
flip_scatter_graph(dataNu)

density_hist_graph(dataNu)
box_graph(dataNu)
flip_box_graph(dataNu)

violin_graph(dataNu)
flip_violin_graph(dataNu)

dot_graph(dataNu)
flip_dot_graph(dataNu)





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




