test_that("Process data options", {

  d <- tibble(a = c("Casa", "Casa", NA), b = c(1:3))

  opts <- list(drop_na = TRUE)
  d2 <- preprocessData(d, opts = opts)
  expect_equal(nrow(d2), 2)



})
