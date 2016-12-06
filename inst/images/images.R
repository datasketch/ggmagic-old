library(ggmagic)
library(stringr)

gg_dat <- data.frame(graph = ggList())
gg_dat$type <- word(gsub('_', ' ', gg_dat$graph), -1)
gg_dat$type <- gsub('\\.', '', gg_dat$type)
gg_dat$type <- gsub("(.{2})", "\\1-", gg_dat$type)
gg_dat$type <- substr(gg_dat$type,1,nchar(gg_dat$type)-1)

pas <- function(x, pat){
  paste0(x,'(sampleData(',"'",pat,"'",'))')
}
path <- getwd()
graphs <- lapply(pas(gg_dat$graph, gg_dat$type), function(x){
  message(x)
  try(eval(parse(text=x))+ ggsave(paste0(getwd(), '/inst/images/', gsub("\\s*\\([^\\)]+\\)|)", '',x),'.jpg')))
  }
)

