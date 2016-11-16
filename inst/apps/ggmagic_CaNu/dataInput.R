
#data input dev


txt <- 'País	Número de años	Región
Bangladesh	23	Asia
India	21	Asia
Ireland	21	Europa
Iceland	20	Europa
Philippines	16	Asia
Sri Lanka	13	Asia
Norway	13	Europa
Finland	13	Europa
United Kingdom	12	Europa
Liberia	11	África'

d <- read_tsv(txt)

library(rhandsontable)

rhandsontable(d)


d <- d[1:2]

gg_pie_CaNu.(d)




