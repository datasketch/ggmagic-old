test_that("gg bar DatNum", {


  data <- sample_data("Dat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()

  l <- ggmagic_prep(data, opts)

  expect_equal(lapply(l$d, class), list(a = "Date", b = "numeric", ..colors = "character"))


  gg_line_DatNum(data)
  gg_line_DatNum(data, logo = "datasketch", caption ="some caption")


})
