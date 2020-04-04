test_that("Bars", {

  library(homodatum)
  library(paletero)

  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  data

  gg_bar_DatNum(data)

  gg_bar_CatNum(sampleData("Cat-Num", nrow = 10))


})
