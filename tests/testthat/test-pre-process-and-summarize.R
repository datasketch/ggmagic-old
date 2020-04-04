test_that("Preprocess and summarize data", {

  dd <- data_frame(x = c("A","A", "B", "C"), y = c("X","Y","Y","Z"), z = c(1,1,3,NA))

  opts <- list(drop_na = TRUE)
  no_na <- processDataOptions(dd, opts)
  expect_false(any(is.na(no_na)))

  #x <- getFringeDataFrame(fringe(sampleData("Cat-Num-Num")))
  d_summ1 <- summarizeData(dd, agg = "sum", to_agg = z, x)
  expect_true(d_summ1$z[d_summ1$x == "A"] == 2)

  d_summ2 <- summarizeData(dd, "sum", to_agg = z, y)
  expect_true(d_summ2$z == c(1,4,0))


})
