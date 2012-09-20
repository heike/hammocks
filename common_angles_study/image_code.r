library(ggparallel)
ggparallel(data = mtcars, vars = list('cyl', 'gear', 'carb', 'am' ), 
           method = "parset", text.angle = 0, order = 0)

ggsave('mtcars_parset.png')


ggparallel(data = mtcars, vars = list('cyl', 'gear',  'carb', 'am' ), 
           method = "hammock", text.angle = 0, order = 0) 
ggsave(filename  = 'mtcars_hammock.png')



ggparallel(data = mtcars, vars = list('cyl', 'gear',  'carb', 'am' ), 
           text.angle = 0, order = 0) 
ggsave(filename  = 'mtcars_ca.png') 




ggparallel(data = mtcars, vars = list('cyl', 'gear',  'carb', 'am' ), 
           text.angle = 0, method = 'adj.angle', ratio = 0.4, order = 0) 
ggsave(filename  = 'mtcars_aa.png') 



## titanic examples
titanic <- data.frame(Titanic)
cols <- c(brewer.pal(5,"Blues")[-1], rev(brewer.pal(3, "Oranges")[-1]), brewer.pal(3, "Greens")[-1])


ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), titanic, weight="Freq", method = 'hammock') +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + theme_bw()

ggsave(filename = 'titanic_hammock.png')


ggparallel(names(titanic)[c(1,4,2,1)],  titanic, weight="Freq", text.angle = 0 , 
           order = c(0, 1, 1, 0)) +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired")
ggsave(filename = 'titanic_ca.png')

ggparallel(names(titanic)[c(1,4,2,1)],  titanic, weight="Freq", text.angle = 0 , 
           order = c(0, 1, 1, 0), method = 'adj.angle', ratio = .00005) +
             scale_fill_brewer(palette="Paired") +
             scale_colour_brewer(palette="Paired")
ggsave(filename = 'titanic_aa.png')



ggparallel(names(titanic)[c(1,4,2,1)],  titanic, weight="Freq", text.angle = 0 , 
           order = c(0, 1, 1, 0), method = 'parset') +
             scale_fill_brewer(palette="Paired") +
             scale_colour_brewer(palette="Paired")
ggsave(filename = 'titanic_parset.png')



1st and female (145)
Crew and survived( 212)
Male and survived(367)

