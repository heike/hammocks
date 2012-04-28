########## some manipulation of the dataset

# remove the why column for easier reading
newdata <- df5[,-3]

# throw out id R_7ONSkE1nz3KoRFO
# it appears this person closed the browser window before completing the task; 
# this is probably Professor Dixon who informed us of his choice to not participate
newdata <- newdata[newdata$id != "R_7ONSkE1nz3KoRFO",]

# create an answer key
key <- data.frame(dataset = rep(LETTERS[1:3], each = 3), qid = rep(1:3, 9), correct = NA)

## Crew, 1st, 3rd, 2nd
key[key$dataset == "A" & key$qid == 1,]$correct <- "1_2_4_3"
## 1st, 2nd, 3rd, crew 
key[key$dataset == "A" & key$qid == 2,]$correct <- "4_1_2_3"
## Crew, 3rd, 2nd, 1st
key[key$dataset == "A" & key$qid == 3,]$correct <- "1_4_3_2"

## receivers E & B show progression that matches emitter order
key[key$dataset == "B" & key$qid == 1,]$correct <- 1
## 
key[key$dataset == "B" & key$qid == 2,]$correct <- "1_4_3_2"
## 0.15 vs 0.13 true or too close
## 0.20 vs 0.16 true
## 0.20 vs 0.10 false
## 0.20 vs 0.27 true
key[key$dataset == "B" & key$qid == 3,]$correct <- "1_1_2_1" # or "1_4_2_1"


# > ddply(.data = newDataC[newDataC$chrom == "chrX",], .variables = "path", .fun = nrow)
      # path V1
# 1 hsa00230 12
# 2 hsa03010 11
# 3 hsa03410  1
# 4 hsa04012 13
# 5 hsa04110 14
# 6 hsa04380 26
# 7 hsa04620 17
# 8 hsa04972  6
key[key$dataset == "C" & key$qid == 1,]$correct <- "1_2_4_3"
# > nrow(newDataC[newDataC$chrom == "chr4" & newDataC$path == "hsa04620",])
# [1] 36
# > nrow(newDataC[newDataC$chrom == "chr1" & newDataC$path == "hsa04972",])
# [1] 59
# > nrow(newDataC[newDataC$chrom == "chr7" & newDataC$path == "hsa04110",])
# [1] 26
# > nrow(newDataC[newDataC$chrom == "chr21" & newDataC$path == "hsa00230",])
# [1] 28
key[key$dataset == "C" & key$qid == 2,]$correct <- "4_1_2_3"
# > ddply(.data = newDataC[newDataC$chrom == "chr1",], .variables = "path", .fun = function(x){nrow(x)/280})
      # path         V1
# 1 hsa00230 0.20000000
# 2 hsa03010 0.04285714
# 3 hsa03410 0.05000000
# 4 hsa04012 0.10000000
# 5 hsa04110 0.12857143
# 6 hsa04380 0.19285714
# 7 hsa04620 0.07500000
# 8 hsa04972 0.21071429
key[key$dataset == "C" & key$qid == 3,]$correct <- 3
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
nrow(test1[test1$pref == 2 & test1$V1 == "circos",])
#[1] 4
## percentage
100 * nrow(test1[test1$pref == 2 & test1$V1 == "circos",]) / nrow(test1[test1$V1 == "circos",])
#[1] 50

############################################################
# question 2: what chart type has the best accuracy
############################################################


