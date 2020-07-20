test_that("gg bar DatNum", {


  data <- sample_data("Dat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()

  l <- ggmagic_prep(data, opts)

  expect_equal(lapply(l$d, class), list(a = "Date", b = "numeric", ..colors = "character"))


  gg_area_YeaNum(data)
  gg_area_YeaNum(data, area_alpha = 0.1)
  gg_area_YeaNum(data, title = "New chart", caption ="some caption")


})
