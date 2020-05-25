test_that("multiplication works", {

  library(homodatum)
  data <- sample_data("Dat-Num")
  makeup(fringe_d(fringe(data))$a, sample = "Enero 4")
  v <- as.Date(homodatum:::Dat_show(data[[1]]))
  makeup(v, sample = "Enero 4")
  makeup(v, sample = "Enero 4 2020")
  makeup_dat(v, sample = "Ene 4")

})
