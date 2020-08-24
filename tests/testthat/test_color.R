test_that("color", {

  # color Cat-Num
  data <- sample_data("Cat-Num", n = 30, rep = FALSE, addNA = F)
  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- NULL
  l <- ggmagic_prep(data, opts)
  colors <- unique(l$d$..colors)
  expect_equal(length(colors), 1)

  opts$style$color_by <- names(data)[1]
  l <- ggmagic_prep(data, opts)
  colors <- unique(l$d$..colors)
  expect_equal(length(colors), length(unique(data[[1]])))


  # color Cat-Cat-Num
  data <- sample_data("Cat-Cat-Num", n = 30, rep = FALSE, addNA = F)
  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- names(data)[1]
  l <- ggmagic_prep(data, opts)
  colors <- unique(l$d$..colors)
  expect_equal(length(colors), length(unique(data[[1]])))

  opts$style$color_by <- names(data)[2]
  l <- ggmagic_prep(data, opts)
  colors <- unique(l$d$..colors)
  expect_equal(length(colors), length(unique(data[[2]])))
})
