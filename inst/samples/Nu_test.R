
dataNum <- data.frame(rnorm(150))
names(dataNum)[1] <- "a"

#Histogram + density + dots (with selection options)
hist_graph(dataNum)
hist_dens_graph(dataNum)
cumm_prob_graph(dataNum)
flip_cumm_prob_graph(dataNum)

line_graph(dataNum)
flip_line_graph(dataNum)

scatter_graph(dataNum, type=2)
flip_scatter_graph(dataNum)

density_hist_graph(dataNum)
box_graph(dataNum)
flip_box_graph(dataNum)

violin_graph(dataNum)
flip_violin_graph(dataNum)

dot_graph(dataNum)
flip_dot_graph(dataNum)





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




