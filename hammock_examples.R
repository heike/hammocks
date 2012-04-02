# examples 
library(ggplot2)
require(reshape2)

data(mtcars)
gghammock(list("gear", "cyl"), data=mtcars)
gghammock(list("cyl", "gear"), data=mtcars)

#
setwd('Dropbox/biovis_vendettuoli')
load("data/rawdata.rdata")
source("code/hammock.R")

df2$V1 <- row.names(df2)
df2m <- melt(df2, id.var="V1", variable="V2")

gghammock(list("V1", "V2"), data=df2m, weight="value") + scale_fill_brewer(palette=6) + coord_flip()

#
titanic <- as.data.frame(Titanic)
gghammock(list("Class", "Survived"), data=titanic, weight="Freq") + scale_fill_brewer(palette=6)
gghammock(list("Survived", "Class"), data=titanic, weight="Freq") + scale_fill_brewer(palette=6)

gghammock(list("Sex", "Survived"), data=titanic, weight="Freq") + scale_fill_brewer(palette=6)


gghammock(list("Sex", "Class", "Survived", "Age"), data=titanic, weight="Freq")  + coord_flip()

gghammock(list("Sex", "Class", "Survived"), data=titanic, weight="Freq") + scale_fill_brewer(palette=6)  + coord_flip()


