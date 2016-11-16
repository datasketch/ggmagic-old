
library(tidyverse)

library(datafringe)
library(ggmagic)

data <- read_csv("data/sample1.csv")






#gg <- sample(bars_CaNu,1)
gg <- "gg_bar_circular_CaNu."
p <- do.call(gg,list(data))


ggsave("")


fringe(data)
fringe()

ftype <- guessFtype(data)
pattern <- paste(ftype,collapse = "")
pattern <- paste0("_",pattern,"\\.")
#pattern <- "bar.*_CaNu\\."
bars_CaNu <- ggList(pattern)

ctypes <- guessCtypes(data)
guessFtype(data)


txt <- "Country	2000
Antigua and Barbuda
Argentina	17.134
The Bahamas	7
Barbados	9.75
Belize	11.4
Bolivia	7.462
Brazil	7.1
Chile	9.708
Colombia	13.325
Costa Rica	5.193"

data <- read_tsv(txt)
guessFtype(data)




#ggWhich(data)

ggList()

#Ca
bars_Ca <- ggList("bar.*_Ca\\.")

#CaCa
bars_CaCa <- ggList("bar.*_CaCa\\.")

#CaNu
bars_CaNu <- ggList("bar.*_CaNu\\.")

#CaCaNu
bars_CaCaNu <- ggList("bar.*_CaCaNu\\.")


gg <- sample(bars_CaNu,1)
gg
do.call(gg,list(data))
do.call(gg,NULL)

