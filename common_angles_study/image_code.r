library(ggparallel)
library(RColorBrewer)
cols <- c(brewer.pal(5,"Blues")[-1], rev(brewer.pal(3, "Oranges")[-1]), brewer.pal(3, "Greens")[-1])


ggparallel(data = mtcars, vars = list('cyl', 'gear', 'carb', 'am' ), 
           method = "parset", text.angle = 0, order = 0) + theme_bw() + 
          scale_y_continuous(breaks = seq(from =0, to = 30, by = 5)) + 
          scale_colour_manual(values = c(brewer.pal('Dark2', n = 8), 
                            brewer.pal('Set2', n = 8)), guide = 'none') +
          scale_fill_manual(values = c(brewer.pal('Dark2', n = 8), brewer.pal('Set2', n = 8)),
                            guide = 'none')


          

ggsave('mtcars_parset.png')


ggparallel(data = mtcars, vars = list('cyl', 'gear',  'carb', 'am' ), 
           method = "hammock", text.angle = 0, order = 0, ratio = .3) + theme_bw() + 
             scale_y_continuous(breaks = seq(from =0, to = 30, by = 5)) + 
             scale_colour_manual(values = c(brewer.pal('Dark2', n = 8), 
                                            brewer.pal('Set2', n = 8)), guide = 'none') +
                                              scale_fill_manual(values = c(brewer.pal('Dark2', n = 8), brewer.pal('Set2', n = 8)),
                                                                guide = 'none')
ggsave(filename  = 'mtcars_hammock.png')



ggparallel(data = mtcars, vars = list('cyl', 'gear',  'carb', 'am' ), 
           text.angle = 0, order = 0) + theme_bw() + 
             scale_y_continuous(breaks = seq(from =0, to = 30, by = 5)) + 
             scale_colour_manual(values = c(brewer.pal('Dark2', n = 8), 
                                            brewer.pal('Set2', n = 8)), guide = 'none') +
                                              scale_fill_manual(values = c(brewer.pal('Dark2', n = 8), brewer.pal('Set2', n = 8)),
                                                                guide = 'none')
ggsave(filename  = 'mtcars_ca.png') 




ggparallel(data = mtcars, vars = list('cyl', 'gear',  'carb', 'am' ), 
           text.angle = 0, method = 'adj.angle', ratio = 2, order = 0) + theme_bw() + 
             scale_y_continuous(breaks = seq(from =0, to = 30, by = 5)) + 
             scale_colour_manual(values = c(brewer.pal('Dark2', n = 8), 
                                            brewer.pal('Set2', n = 8)), guide = 'none') +
                                              scale_fill_manual(values = c(brewer.pal('Dark2', n = 8), brewer.pal('Set2', n = 8)),
                                                                guide = 'none')
ggsave(filename  = 'mtcars_aa.png') 



## titanic examples
titanic <- data.frame(Titanic)


ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), titanic, weight="Freq", method = 'hammock', ratio = .3) +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + theme_bw()

ggsave(filename = 'titanic_hammock.png')


ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), titanic, weight="Freq") +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + theme_bw()

ggsave(filename = 'titanic_ca.png')


ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), alpha=0.6, method="adj.angle", ratio=.035, titanic, weight="Freq") +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + theme_bw()

ggsave(filename = 'titanic_aa.png')

ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), method="parset", titanic, weight="Freq") +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + theme_bw()

ggsave(filename = 'titanic_parset.png')



1st and female (145)
Crew and survived( 212)
Male and survived(367)

