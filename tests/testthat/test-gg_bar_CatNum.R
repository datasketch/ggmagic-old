test_that("gg bar Cat Num", {

  library(homodatum)
  data <- sample_data("Cat-Num")
  opts <- ggmagic:::ggmagic_defaults()

  gg_bar_CatNum(data)
  gg_bar_CatNum(data, logo = "datasketch", caption ="some caption")

  gg_bar_CatNum(data, logo = "datasketch",
                caption ="some caption",
                color_by = names(data)[1])

  gg_bar_CatNum(data, logo = "datasketch",
                caption ="some caption",
                color_by = names(data)[2],
                ver_title = "Vertical",
                hor_title = "Horizontal")

  gg_bar_CatNum(data, logo = "datasketch",
                caption ="some caption",
                color_by = names(data)[1],
                orientation = "hor",
                ver_title = "Vertical",
                hor_title = "Horizontal")


  gg_bar_CatNum(data = data,
                agg = "mean",
                caption = "This is a caption",
                drop_na = TRUE,
                highlight_value = "CatB",
                highlight_value_color = "#CDA015",
                hor_label = "Horizontal axis",
                title = "This is a title",
                subtitle = "This is a subtitle",
                text_color = "#FEA0D0",
                text_size = 10,
                ver_title = "Vertical axis")


})
