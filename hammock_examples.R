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
# colors for circos
#6633FF	# purple
#0066CC	# middle blue
#99FF33	# light green
#FF33FF	# pink
#FFCC33	# orange
#00CC33	# grass green
#66FFCC	# turquoise
#FF3333	# red

df2$V1 <- row.names(df2)
df2m <- melt(df2, id.var="V1", variable="V2")

newDataC <- cbind(name = sapply(newDataC$chrom, FUN = function(x){strsplit(as.character(x), "chr")[[1]][2]}), newDataC)
circos.colors <- c("#66FFCC", "#FF33FF" ,"#0066CC" , "#00CC33" ,"#FFCC33" , "#6633FF" , "#99FF33" , "#FF3333" , 
			       "#FF3333" ,"#6633FF" ,"#99FF33" ,"#0066CC" ,"#00CC33" ,"#66FFCC" ,"#FFCC33" ,"#FF33FF")
gghammock(list("V1", "V2"), data=df2m, weight="value", angle=0, text.offset=NULL, color="grey80") + 
coord_flip() + opts(legend.position="none") + scale_fill_manual(values=circos.colors) 

qplot(reorder(V1, value, sum), fill=V2, data=df2m, weight=value) + coord_flip() + xlab("V1") + 
opts(legend.direction = "horizontal", legend.position = "bottom") + scale_fill_manual(values=circos.colors) 

qplot(reorder(V2, value, sum), fill=V1, data=df2m, weight=value) + coord_flip() + xlab("V2") + 
opts(legend.direction = "horizontal", legend.position = "bottom") + scale_fill_manual(values=circos.colors) 


## dataset C - gene data from public dbs
load("data/datasetC.rda")
chroms <- unique(dfC$chrom)[c(1:7,9,11,13,14,17:19,21,25,27,29,32,36,37,39,41,43)]
newDataC <- dfC_sub[dfC_sub$chrom %in% chroms,]
newDataC$chrom <- factor(newDataC$chrom)
newDataC$chromN <- as.numeric(gsub("chr([0-9]*)", "\\1", as.character(newDataC$chrom)))
levels(newDataC$chrom) <- gsub("chr","", levels(newDataC$chrom))

newDataC$chrom <- reorder(newDataC$chrom, newDataC$chromN, FUN=max)

library(RColorBrewer)
gghammock(list("chrom", "path"), order=c(0,1), angle=0, data = newDataC, color = "grey70", text.offset=c(rep(0, 24), 0.2, rep(0, 8))) + scale_fill_manual(values = c(rep("grey40", 24), brewer.pal("YlOrRd", n = 9))) + coord_flip() + opts(legend.position="none")


qplot(chrom, fill=path, data=newDataC, colour=I("grey80")) + scale_fill_brewer(palette="YlOrRd") + opts(legend.direction = "horizontal", legend.position = "bottom") + coord_flip()
qplot(reorder(path, path, FUN=function(x) -length(x)), fill=chrom, data=newDataC, colour=I("grey80")) + opts(legend.direction = "horizontal", legend.position = "bottom") + coord_flip() + xlab("path")
