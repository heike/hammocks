### package and code dependencies
require(reshape)
require(ggplot2)
require(plyr)
source('hammock.R')


### some aesthetic choices
fill_colors <- c( "pink", "white", "skyblue1",  "pink", "white", "skyblue1")
bar_thickness <- 0.25
alpha <- 0.5 # set transparancy of connections between 0 and 1 [opaque]
text_angle <- 0 # 0 for horizontal plot orientation, 90 for vertical plot orientation
legend <- 'none'
outline_color <- NA

## dummy data.frame - a frequency table of classifiers
## assumes that initial sample is of 100 pt in each classification of C-reactive protein
##     levels.

## NOTE1: unclear from original plot how many pt start in high category and move to 
##        med vs low classification or start in low category and move to high vs med 
##        classification

## NOTE2: the categorical nature of CRP & HDL measures are due to arbitrary cutoffs of 
##        continuous data and reflect clinical context
French_placeholder1 <- data.frame(baseCRP = rep(1:3, each = 3),
                                  followCRP = rep(1:3, 3),
                                  Freq = c(49, 51, 0, 17, 46, 37, 0, 27, 73))
French_placeholder1$baseCRP <- factor(French_placeholder1$baseCRP, labels = c('high', 'med', 'low'))
French_placeholder1$followCRP <- factor(French_placeholder1$followCRP, labels = c('high', 'med', 'low'))

### data & display variables
variables <- list('baseCRP', 'followCRP') # variables, as named in data.frame
connect_weight <- 'Freq' # variable that stores the connector width
ordering <- 0 # set to -1 or 1 for ordering within bar by freq


#### NOTE: additional ggplot2 commands may be used to modify the display
## horizontal
gghammock(vars = variables, data = French_placeholder1, weight = connect_weight, order = ordering,
          angle = text_angle, color = outline_color) + coord_flip() + 
          opts(legend.position = legend) + scale_fill_manual(values = fill_colors)

## vertical
gghammock(vars = variables, data = French_placeholder1, weight = connect_weight, order = ordering,
          angle = text_angle, color = outline_color) + 
            opts(legend.position = legend) + scale_fill_manual(values = fill_colors)

