require(gdata)
require(plumbr)
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
#  TimeA: time spent on page for dataset A (ms)
#  ClicksA: number of clicks on that page
#  TypeB: chart type for dataset B
#  RepsonseB1: response for dataset B, question 1
#  ResponseB2: response for dataset B, question 2
#  ResponseB3: response for dataset B, question 3
#  TimeB: time spent on page for dataset B (ms)
#  ClicksB: number of clicks on that page
#  TypeC: chart type for dataset C
#  RepsonseC1: response for dataset C, question 1
#  ResponseC2: response for dataset C, question 2
#  ResponseC3: response for dataset C, question 3
#  TimeC: time spent on page for dataset C
#  ClicksC: number of clicks on that page
#  Preference: which plot did respondent perfer
#  Why: reason for preference
#  Browser: browser type
#  OS: operating system
#  datestart: date/time started
#  totaltime: total time in survey (minutes)
#  monitorW: monitor width (px)
#  monitorH: monitor height (px)
##########################################

df4 <- data.frame(ID = unique(df2[,1]), TypeA = NA, ResponseA1 = NA, ResponseA2 = NA, 
		ResponseA3 = NA, TimeA = NA, ClicksA = NA, TypeB = NA, ResponseB1 = NA, ResponseB2 = NA,
		ResponseB3 = NA, TimeB = NA, ClicksB = NA, TypeC = NA, ResponseC1 = NA, ResponseC2 = NA,
		ResponseC3 = NA, TimeC = NA, ClicksC = NA, Preference = NA, Why = NA, Browser = NA, OS = NA, 
		datestart = NA,	totaltime = NA, monitorW = NA, monitorH = NA)

# get the structural elements for each respondant		
for(i in df4$ID){
	temp <- as.character(df2[df2[,1] == i, 290])
	df4[df4$ID == i,]$TypeA <- strsplit(temp, ",")[[1]][1]
	df4[df4$ID == i,]$TypeB <- strsplit(temp, ",")[[1]][2]
	df4[df4$ID == i,]$TypeC <- strsplit(temp, ",")[[1]][3]
	df4[df4$ID == i,]$OS <- as.character(df2[df2[,1] == i,]$Q2_3_TEXT)
	df4[df4$ID == i,]$datestart <- as.character(df2[df2[,1] == i,]$V8)
	df4[df4$ID == i,]$totaltime <- round(as.POSIXlt(as.character(df2[df2[,1] == i,]$V9)) - as.POSIXlt(as.character(df2[df2[,1] == i,]$V8)), 2)
	temp <- as.character(df2[df2[,1] == i,]$Q2_4_TEXT)
	df4[df4$ID == i,]$monitorW <- strsplit(temp, "x")[[1]][1]
	df4[df4$ID == i,]$monitorH <- strsplit(temp, "x")[[1]][2]
	df4[df4$ID == i,]$Browser <- as.character(df2[df2[,1] == i,]$Q2_1_TEXT)
}

# turn the block groupings into eye-readable names
df4 <- data.frame(sapply(df4, FUN = function(x){gsub(x = x, pattern = "horizontal", replacement = "hammock")}), stringsAsFactors = FALSE)
df4 <- data.frame(sapply(df4, FUN = function(x){gsub(x = x, pattern = "vertical", replacement = "bar")}), stringsAsFactors = FALSE)

# convert "" strings (NA from qualtrics metadata gathering) to real NAs
df4[df4$OS == "", "OS"] <- NA
df4[df4$Browser == "", "Browser"] <-NA

# remove leading and trailing spaces
df4$TypeA <- trim(df4$TypeA)
df4$TypeB <- trim(df4$TypeB)
df4$TypeC <- trim(df4$TypeC)


for(i in df4$ID){
	temp1 <- df4[df4$ID == i,]
	temp2 <- df2[df2[,1] == i,]
	## block name: horizontal, vertical,circos
	## questions: 99,3, 33, 34,35 in qualtrics 
	## in raw csv: 
	if(temp1$TypeA == "hammock" & temp1$TypeB == "bar" & temp1$TypeC == "circos"){
		df4 <- getAnswers(df4 = df4,i = i, temp1 = temp1, temp2 = temp2, timing = 99, qAs = c(33,34,35),
					qBs = c(51,52,53))
	}
}

getAnswers <- function(df4,i, temp1, temp2,timing, qAs, qBs = NA, qCs = NA){
	naming <- "Response"
	
	df4[df4$ID == i,]$TimeA <- as.numeric(as.character(subset(temp2, select = 
		paste("Q", timing,"_3", sep = ""))[[1]]))
	df4[df4$ID == i,]$ClicksA <- as.numeric(as.character(subset(temp2, select = paste("Q", 
		timing, "_4", sep = ""))[[1]]))
	## case1: respondant did not change order for A
	## case2: all other responses
	
	namesA <- paste(naming, "A", 1:3, sep = "")
	for(j in 1:3){
		if(subset(temp2, select = paste("Q", qAs[j], "_1", sep = ""))[[1]] == 1 & subset(temp2, select = 
			paste("Q", qAs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", qAs[j], "_3", 
			sep = ""))[[1]] == 4 & subset(temp2, select = paste("Q", qAs[j], "_4", sep = ""))[[1]] == 4){
			df4[df4$ID == i, namesA[j]] <- "No Response"
		} else {
			df4[df4$ID == i, namesA[j]] <- paste(subset(temp2, select = 
				paste("Q", qAs[j], "_1", 
				sep = ""))[[1]],subset(temp2, select = paste("Q", qAs[j], "_2", sep = ""))[[1]], 
				subset(temp2, select = 	paste("Q", qAs[j], "_3", sep = ""))[[1]], sep = "_")
		}
	}
	namesB <- paste(naming, "B", 1:3, sep = "")
	for(j in 1:3){
		if(j == 2 & subset(temp2, select = paste("Q", qBs[j], "_1", sep = ""))[[1]] == 1 & 
			subset(temp2, select = 
			paste("Q", qBs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", qBs[j], "_3", 
			sep = ""))[[1]] == 4 & subset(temp2, select = paste("Q", qBs[j], "_4", sep = ""))[[1]] == 4){
			df4[df4$ID == i, namesB[j]] <- "No Response"
		}
		 # else if(j == 2){
			# df4[df4$ID == i, namesB[j]] <- paste(subset(temp2, select = 
				# paste("Q", qBs[j], "_1", 
				# sep = ""))[[1]],subset(temp2, select = paste("Q", qBs[j], "_2", sep = ""))[[1]], 
				# subset(temp2, select = 	paste("Q", qBs[j], "_3", sep = ""))[[1]], sep = "_")
		# } else {
			# if(j == 1){
				# df4[df4$ID == i, namesB[j]] <- subset(temp2, select = paste("Q", qBs[j], sep = ""))[[1]]
			# } else if (j == 3){
				# df4[df4$ID == i, namesB[j]] <- paste(subset(temp2, select = 
				# paste("Q", qBs[j], "_1", 
				# sep = ""))[[1]],subset(temp2, select = paste("Q", qBs[j], "_2", sep = ""))[[1]], 
				# subset(temp2, select = 	paste("Q", qBs[j], "_3", sep = ""))[[1]], sep = "_")

			# }
		# }
	}
	return(df4)
}