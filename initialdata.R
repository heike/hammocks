setwd('hammocks/other files')
df1 <- read.csv('Plotting.csv')
#####################################
# Section 1: Clean the data
#####################################
## keep only data that was generated from testing mode
df2 <- df1[df1$V7 == 0,]

## remove columns of data that are not part of study
## user name (anonymous), email, response set id
df2 <- df2[!names(df2) %in% c('V2','V3','V4','V5')]

## descriptive value of column names
df3 <-data.frame( t( df1[1,]))
names(df3) <- "text"
df3 <- data.frame(QID = rownames(df3), text = df3$text)
rownames(df3) <- NULL

## refactoring due to cleaning
df2 <- data.frame(apply(df2, MARGIN = 2, FUN = factor))

#####################################
# Section 2: Summary stats
#####################################

############### factors that affect physical display
## operating system
summary(df2$Q2_3_TEXT)
#                      Intel Mac OS X 10_6_8 Intel Mac OS X 10_7_3   Intel Mac OS X 10.6          Linux x86_64 
#                    2                     7                     4                     1                     3 
#               rv:6.0                Ubuntu                 WOW64 
#                    1                     1                    14 

## browser info
### NOTE: I discovered after the survey went out that IE renders the html code for the training data poorly
summary(df2$Q2_1_TEXT)
#         Chrome Firefox  Safari 
#      2      16      10       5 

## monitor size
summary(df2$Q2_4_TEXT)
#          1280x1024  1280x800  1366x768  1440x900 1600x1200  1600x900 1680x1050 1920x1080 1920x1200 
#        2         2         8         6         5         2         4         2         1         1 

################## final preference
pref <- summary(df2$Q89)
names(pref)<- c("NA","bar", "circle", "hammock", "equal")
pref
#     NA     bar  circle hammock   equal 
#      1      17       4      10       1 

################## testing info
data.frame(n =summary(df2[,290]))
                             n
circos, horizontal, vertical 3
circos, vertical, horizontal 5
horizontal, circos, vertical 8
horizontal, vertical, circos 5
vertical, circos, horizontal 5
vertical, horizontal, circos 7

##########################################
# restructure the data
# variables:
#  ID
#  TypeA: chart type for dataset A
#  RepsonseA1: response for dataset A, question 1
#  ResponseA2: response for dataset A, question 2
#  ResponseA3: response for dataset A, question 3
#  TimeA: time spent on page for dataset A
#  TypeB: chart type for dataset B
#  RepsonseB1: response for dataset B, question 1
#  ResponseB2: response for dataset B, question 2
#  ResponseB3: response for dataset B, question 3
#  TimeB: time spent on page for dataset B
#  TypeC: chart type for dataset C
#  RepsonseC1: response for dataset C, question 1
#  ResponseC2: response for dataset C, question 2
#  ResponseC3: response for dataset C, question 3
#  TimeC: time spent on page for dataset C
#  Preference: which plot did respondent perfer
#  Why: reason for preference
#  Browser: browser type
#  OS: operating system
#  datetaken: date/time taken
#  totaltime: total time in survey
#  monitor: monitor size
##########################################

df4 <- data.frame(ID = unique(df2[,1]))