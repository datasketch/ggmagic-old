context("ggmagic meta data")

test_that("All viz in meta have an image", {
  imgs <- list.files(system.file("imgs",package="ggmagic", mustWork=TRUE))
  imgs <- file_path_sans_ext(imgs)
  ids <- availableggmagic()
  #allIdsHaveImagess <- function(imgs,ids)Reduce(identical,Map(table,list(imgs,ids)))
  # expect_equal_vectors <- function(a,b){
  #   eval(bquote(expect_equal(setdiff(.(a),.(b)),character(0))))
  # }
  # expect_equal_vectors(ids,imgs)
  expect_equal(setdiff(ids,imgs),character(0))
  expect_equal(setdiff(imgs,ids),character(0))
  #expect_error()
  #expect_true()
  #expect_false()
})


