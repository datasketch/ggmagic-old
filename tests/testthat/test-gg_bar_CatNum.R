test_that("gg bar Cat Num", {

  library(homodatum)
  data <- sample_data("Cat-Num")
  opts <- ggmagic:::ggmagic_defaults()

  gg_bar_CatNum(data)
  gg_bar_DatNum(data, logo = "datasketch", caption ="some caption")

  gg_bar_CatNum(data = catnum_data,
                agg = "mean",
                caption = "This is a caption",
                colors = c("#FEAFEA"),
                color_scale = "no",
                drop_na = TRUE,
                highlight_value = "CatB",
                highlight_value_color = "#CDA015",
                hor_label = "Horizontal axis",
                subtitle = "This is a subtitle",
                text_color = "#FEA0D0",
                text_size = 5,
                title = "This is a title",
                ver_label = "Vertical axis")


})
