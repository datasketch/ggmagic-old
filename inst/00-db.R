
library(devtools)
load_all()
document()
install()

library(ggmagic)


#####







######

d <- sampleData("Ca-Ye-Nu",nrow = 11)
gg_lines_hor_CaYeNu.(d)

##
d <- sampleData("Ye",nrow = 11)
d$a <- as.factor(d$a)
gg_pie_Ca.(d)

d <- sampleData("Ye-Nu")
gg_lines_hor_YeNu.(d)
gg_lollipop_YeNu.(d)


# Pie

gg_pie_CaNu.(sampleData("Ca-Nu"), title = "hoaa\nfdsafds")


p <- gg_pie_CaNu.(sampleData("Ca-Nu")) + theme_ds()
p
ggsave("~/Desktop/file.png", plot = p)

save_ggmagic(p,"~/Desktop/file.png")


gg_pie_CaNu.(sampleData("Ca-Nu")) +
  labs(title = "RE",
       subtitle="A well-crafted subtitle",
       caption ="Lorem asi pasode dale") +
  theme_ds_clean() +
  scale_fill_manual(values = getPalette())



data("G20")
dataCaCaCa <- G20 %>% select(Region, Country, Economic.classification)
gg_treemap_x_CaCaCa.(dataCaCaCa)+ theme_ds() + scale_fill_manual(values = getPalette())


ggList()
ggList("bar.*_CaNu\\.")

ggList(wrongNames = TRUE)

ggFtype()
ggFtype("gg_waffle.")
data <- data.frame(sample(letters[1:3],10,replace = TRUE))
ggWhich(data)

run_gg(data,"gg_waffle.")
run_gg(data,"gg_bar_ver.")
run_gg(data,"gg_bar_ver_ord.")


