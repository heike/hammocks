########## some manipulation of the dataset

# remove the why column for easier reading
newdata <- df5[,-3]

# throw out id R_7ONSkE1nz3KoRFO
# it appears this person closed the browser window before completing the task.
newdata <- newdata[newdata$id != "R_7ONSkE1nz3KoRFO",]

#############################################
## question 0: what type of chart is most preferred
test0 <- ddply(.data = newdata, .variables = "pref", .fun = function(x){length(unique(x$id))})
rownames(test0) <- c("none", "bar", "circle", "hammock", "equal")
test0

#        pref V1
# none          1
# bar        1 16
# circle     2  4
# hammock    3 10
# equal      4  1

#############################################################
# question 1: does order of chart display affect preference
#############################################################
test1 <- ddply(.data = newdata, .variables = c("id", "pref"), .fun = function(x){as.character(unique(x[x$dataset == "A",]$plottype))})

## number of individuals who preferred bar graphs if they were shown bar first
nrow(test1[test1$pref == 1 & test1$V1 == "bar",])
#[1] 11
## percentage of those shown bar first who prefer bar
100 * nrow(test1[test1$pref == 1 & test1$V1 == "bar",]) / nrow(test1[test1$V1 == "bar",])
[1] 100

## number of indiv who preferred hammock plots if they were shown hammocks first
nrow(test1[test1$pref == 3 & test1$V1 == "hammock",])
# [1] 10
## percentage of those shown hammock first who prefer hammock
100 * nrow(test1[test1$pref == 3 & test1$V1 == "hammock",]) / nrow(test1[test1$V1 == "hammock",])
# [1] 76.92308

## number of indiv who preferred circle if they were shown circle first
