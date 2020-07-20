test_that("gg bar DatNum", {


  data <- sample_data("Yea-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()

  l <- ggmagic_prep(data, opts)

  expect_equal(lapply(l$d, class), list(a = "integer", b = "numeric", ..colors = "character"))


  gg_line_YeaNum(data)
  gg_line_YeaNum(data, title = "New chart", caption ="some caption")


})
