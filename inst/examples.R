# library(devtools)
# load_all()
# document()
# install()

library(ggmagic)

# Area

cat_data <- sample_data("Dat", rep = T, n = 300)
gg_area_Dat(data = cat_data,
            agg_text = "Count of ",
            caption = "This is a caption",
            colors = c("#FEAFEA"),
            color_opacity = 0.1,
            drop_na = TRUE,
            hor_title = "Horizontal axis",
            hor_line = 5,
            order = c("TypeD", "TypeE"),
            percentage = TRUE,
            shape_size = 5,
            shape_type = 1,
            start_zero = F,
            subtitle = "This is a subtitle",
            text_color = "#c44adf",
            text_size = 15,
            title = "This is a title",
            ver_title = "Vertical axis")
gg_area_Dat(data = cat_data,
            opts = list(
              agg_text = "Count of ",
              caption = "This is a caption",
              drop_na = TRUE,
              hor_title = "Horizontal axis",
              subtitle = "This is a subtitle",
              text_color = "#c44adf",
              text_size = 15,
              title = "This is a title",
              ver_title = "Vertical axis",
              ver_line = 27,
              branding_include = T,
              theme = list(background_color = '#fafafa')
            ))


catnum_data <- sample_data("Cat-Num", 300)
gg_area_CatNum(data = catnum_data)
gg_area_CatNum(data = catnum_data,
               agg = "mean",
               caption = "This is a caption",
               colors = c("#FEAFEA"),
               color_opacity = 1,
               drop_na = TRUE,
               hor_title = "Horizontal axis",
               shape_size = 5,
               shape_type = 1,
               subtitle = "This is a subtitle",
               text_color = "#c44adf",
               text_size = 5,
               title = "This is a title",
               ver_title = "Vertical axis",
               ver_line = 27)

catcat_data <- sample_data("Cat-Cat", 300)
gg_area_CatCat(catcat_data)

catcatnum_data <- sample_data("Cat-Cat-Num", 300)
gg_area_CatCatNum(data = catcatnum_data,
                  percentage = TRUE,
                  legend_show = FALSE)


# Lines
cat_data <- sample_data("Cat")
gg_line_Cat(cat_data,
            opts = list(
              agg_text = "Count of ",
              caption = "This is a caption",
              colors = c("#FEAFEA"),
              color_opacity = 0.1,
              drop_na = TRUE,
              hor_title = "Horizontal axis",
              hor_line = 5,
              order = c("TypeD", "TypeE"),
              percentage = TRUE,
              shape_size = 5,
              shape_type = 1,
              start_zero = F,
              subtitle = "This is a subtitle",
              text_color = "#c44adf",
              text_size = 5,
              title = "This is a title",
              ver_title = "Vertical axis",
              ver_line = 27,
              theme = tma(custom = list(background = '#DADADA'))
            ))


catnum_data <- sample_data("Cat-Num", 300)
gg_line_CatNum(data = catnum_data)


catcat_data <- sample_data("Cat-Cat")
gg_line_CatCat(catcat_data)


catcatnum_data <- sample_data("Cat-Cat-Num")
gg_line_CatCatNum(catcatnum_data)


# Barras


cat_data <- sample_data("Cat")
gg_bar_Cat(data = cat_data)

catnum_data <- sample_data("Cat-Num", 300)
gg_bar_CatNum(data = catnum_data)
gg_bar_CatNum(data = catnum_data,
               agg = "mean",
               caption = "This is a caption",
               colors = c("#FEAFEA"),
               color_scale = "no",
               drop_na = TRUE,
               highlight_value = "CatB",
               highlight_value_color = "#CDA015",
               hor_title = "Horizontal axis",
               subtitle = "This is a subtitle",
               text_color = "#FEA0D0",
               text_size = 5,
               title = "This is a title",
               ver_title = "Vertical axis")

catcat_data <- sample_data("Cat-Cat")
gg_bar_CatCat(catcat_data)


catcatnum_data <- sample_data("Cat-Cat-Num", 500)
gg_bar_CatCatNum(catcatnum_data,
                 agg = "mean",
                 agg_text = "Mean of ",
                 caption =  "This is a caption",
                 colors = c("#FEAFEA", "#CAFADA"),
                 drop_na_v = c(FALSE, TRUE),
                 graph_type = "stacked")


# pie
cat_data <- sample_data("Cat")
gg_pie_Cat(data = cat_data)


catnum_data <- sample_data("Cat-Num", 300)
gg_pie_CatNum(data = catnum_data,
              agg = "mean",
              caption = "This is a caption",
              subtitle = "This is a subtitle",
              text_color = "#FFFFFF",
              text_size = 5,
              title = "This is a title")

# dona
cat_data <- sample_data("Cat")
gg_donut_Cat(data = cat_data)



# Treemap
cat_data <- sample_data("Cat", 300)
gg_treemap_Cat(cat_data)

catnum_data <- sample_data("Cat-Num", 300)
gg_treemap_CatNum(catnum_data)

catcatnum_data <- sample_data("Cat-Cat-Num", 500)
gg_treemap_CatCatNum(catcatnum_data,
                     group_color = "#CCCCCC",
                     drop_na_v = c(TRUE, TRUE))
gg_treemap_CatCatNum(catcatnum_data,
                     group_color = "#CCCCCC",
                     drop_na_v = c(TRUE, TRUE),
                     text_show_v = c(F, F),
                     legend_show = FALSE)



# Labels
gg_bar_Cat(sample_data("Cat"), dataLabels_show = T, graph_type="stacked", percentage = T)
gg_bar_CatCat(sample_data("Cat-Cat"), dataLabels_show = T, graph_type="stacked", percentage = T)
gg_bar_CatCatNum(sample_data("Cat-Cat-Num"), dataLabels_show = T, graph_type="stacked", percentage = T)
gg_bar_CatCatNum(sample_data("Cat-Cat-Num"), dataLabels_show = T)

gg_bar_CatYeaNum(sample_data("Cat-Yea-Num"), dataLabels_show = T)
gg_bar_CatYeaNum(sample_data("Cat-Yea-Num"), dataLabels_show = T, graph_type="stacked")
gg_bar_CatYeaNum(sample_data("Cat-Yea-Num"), dataLabels_show = T, graph_type="stacked", percentage = T)
gg_bar_CatYea(sample_data("Cat-Yea"), dataLabels_show = T, graph_type="stacked", percentage = T)

gg_bar_DatNum(sample_data("Dat-Num", nrow = 100),  dataLabels_show = T)

gg_area_CatDatNum(sample_data("Cat-Dat-Num", nrow = 100),dataLabels_show = T,
                  graph_type="stacked", percentage = T)

gg_area_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 100),dataLabels_show = T,
                  graph_type="stacked", percentage = T)
gg_area_DatNum(sample_data("Dat-Num"), dataLabels_show = T)
gg_line_CatDatNum(sample_data("Cat-Dat-Num"), dataLabels_show = T)

gg_line_CatYeaNum(sample_data("Cat-Yea-Num"), dataLabels_show = T)
gg_line_CatYea(sample_data("Cat-Yea", 100), dataLabels_show = T)
gg_line_DatNum(sample_data("Dat-Num"), dataLabels_show = T)

gg_line_YeaNum(sample_data("Yea-Num", nrow = 10), dataLabels_show = T)
gg_line_Yea(sample_data("Yea", rep = TRUE), dataLabels_show = T)


gg_pie_CatNum(sample_data("Cat-Num", nrow = 100),  dataLabels_show = T)
gg_pie_CatNum(sample_data("Cat-Num", nrow = 100),  dataLabels_show = T, percentage = T)

gg_donut_CatNum(sample_data("Cat-Num", nrow = 100),  dataLabels_show = T, percentage = T)
gg_donut_Cat(sample_data("Cat"), dataLabels_show = T)
