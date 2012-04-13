setwd("/Users/heike/papers/2012 BioVis/hammocks")

# examples 
library(ggplot2)
require(reshape2)

"#FF3333" # red 
"#FF33FF" # purple
"#CCFF33" # yellow
"#33FF99" # green
"#3366FF" # blue

circos_colors <- c( "#3366FF", "#CCFF33", "#FF33FF",  "#33FF99", "#CCFF33", "#FF3333")
data(mtcars)
gghammock(list("cyl", "gear"), data=mtcars, angle=0) + 
	coord_flip() + opts(legend.position="none") + 
	scale_fill_manual(values= circos_colors)
	
qplot(factor(gear), data=mtcars, binwidth=1, fill=factor(cyl)) + 
	scale_fill_manual("cyl", values= circos_colors[-(1:3)]) + scale_x_discrete("gear")


qplot(factor(cyl), data=mtcars, binwidth=1, fill=factor(gear)) + 
	scale_fill_manual("gear", values= circos_colors[-(1:3)])  + scale_x_discrete("cyl")



#
titanic <- as.data.frame(Titanic)
#00CCCC	# purple
#CCCC33	# yellow
#3333FF # blue
#FF33FF	# pink
#FF3333 # red
#00CC00 # green

circos.colors <- c("#FF00FF", "#3333FF", "#FF3333", "#CCCC33", "#00CC00", "#00CCCC")
gghammock(list("Survived", "Class"), data=titanic, weight="Freq", angle=0, order=c(1,0)) +
	 coord_flip() + opts(legend.position="none") + scale_fill_manual(values= circos.colors)
	 
qplot(Class, data=titanic, binwidth=1, fill=Survived, weight=Freq) + 
	scale_fill_manual(values= c("#3333FF", "#FF33FF")) 

qplot(Survived, data=titanic, binwidth=1, fill=Class, weight=Freq) + 
	scale_fill_manual(values= circos.colors[-(1:2)]) 

#
setwd('~/Dropbox/Marie/biovis_vendettuoli')
load("data/rawdata.rdata")
source("code/hammock.R")

df2$V1 <- row.names(df2)
df2m <- melt(df2, id.var="V1", variable="V2")

circos.colors <- c() # need eight colors
gghammock(list("V1", "V2"), data=df2m, weight="value", angle=0) + 
coord_flip() + opts(legend.position="none")
