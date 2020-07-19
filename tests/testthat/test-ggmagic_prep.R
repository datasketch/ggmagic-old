test_that("Test extra options", {

  data <- sample_data("Cat-Num")
  opts <- dsviz_default_opts()

  # Pie
  l <- ggmagic_prep(data, opts, extra_pattern = "pie")
  expect_equal(names(l$extra), "pie_dataLabels_pos")
  expect_equal(l$extra$pie_dataLabels_pos, 1.1)

  # Donut
  l <- ggmagic_prep(data, opts, extra_pattern = "donut")
  expect_equal(names(l$extra), c("donut_dataLabels_pos", "donut_width"))
  expect_equal(l$extra$donut_dataLabels_pos, 1.1)
  expect_equal(l$extra$donut_width, 0.2)

})

test_that("Cat -> CatNum aggregation works",{

  data <- sample_data("Cat")
  opts <- dsviz_default_opts()
  l <- ggmagic_prep(data, opts)
  d <- l$d
  randomCat <- sample(data[[1]],1)
  expect_true(sum(data[[1]] == randomCat, na.rm = TRUE) == d[[2]][d[[1]] == randomCat])


})

test_that("CatCatNum",{

  d <- sample_data("Cat-Cat")
  # color_by Always colors by second column
  opts <- dsviz_default_opts()
  ggmagic_prep(d, opts)


})


