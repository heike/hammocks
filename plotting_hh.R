setwd('~/papers/2012 BioVis/hammocks/other files')
frame <- read.csv('Plotting.csv')

# get the data into a reasonable shape for analysis:
# - get all response for a single question into one variable
# - combine responses for the same dataset across different visualizations

head(frame[,6:10])


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
# Section 3 restructure the data
############################################


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




################## helper function
getAnswers <- function(df4,i, temp1, temp2,timing, qAs, qBs = NA, qCs = NA){
	naming <- "Response"
	timenames <- paste("Time", LETTERS[1:length(timing)], sep = "")
	clicknames <- paste("Clicks", LETTERS[1:length(timing)], sep ="")
	for(j in 1:length(timing)){
		df4[df4$ID == i, timenames[j]] <- round(as.numeric(as.character(subset(temp2, select = 
			paste("Q", timing[j],"_3", sep = ""))[[1]])), 2)
		df4[df4$ID == i, clicknames[j]] <- as.numeric(as.character(subset(temp2, select = paste("Q", 
			timing[j], "_4", sep = ""))[[1]]))
	}
	## case1: respondant did not change order for A
	## case2: all other responses
	namesA <- paste(naming, "A", 1:3, sep = "")
	for(j in 1:3){
		if(subset(temp2, select = paste("Q", qAs[j], "_1", sep = ""))[[1]] == 1 & subset(temp2, select = 
			paste("Q", qAs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", qAs[j], "_3", 
			sep = ""))[[1]] == 3 & subset(temp2, select = paste("Q", qAs[j], "_4", sep = ""))[[1]] == 4){
			df4[df4$ID == i, namesA[j]] <- "No Response"
		} else {
			df4[df4$ID == i, namesA[j]] <- paste(subset(temp2, select = 
				paste("Q", qAs[j], "_1", 
				sep = ""))[[1]],subset(temp2, select = paste("Q", qAs[j], "_2", sep = ""))[[1]], 
				subset(temp2, select = 	paste("Q", qAs[j], "_3", sep = ""))[[1]], subset(temp2, 
				select = paste("Q", qAs[j], "_4", sep = ""))[[1]], sep = "_")
		}
	}
	namesB <- paste(naming, "B", 1:3, sep = "")

	for(j in 1:3){
		if(j == 2 && ((subset(temp2, select = paste("Q", qBs[j], "_1", sep = ""))[[1]] == 1 & 
			subset(temp2, select = 
			paste("Q", qBs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", qBs[j], "_3", 
			sep = ""))[[1]] == 3 & subset(temp2, select = paste("Q", qBs[j], "_4", sep = ""))[[1]] == 4) |
			(subset(temp2, select = paste("Q", qBs[j], "_1", sep = ""))[[1]] == "" & 
			subset(temp2, select = 
			paste("Q", qBs[j], "_2", sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qBs[j], "_3", 
			sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qBs[j], "_4", sep = ""))[[1]] == ""))){
		    df4[df4$ID == i, namesB[j]] <- "No Response"
		} else if(j == 2){

			df4[df4$ID == i, namesB[j]] <- paste(subset(temp2, select = 
				paste("Q", qBs[j], "_1", 
				sep = ""))[[1]],subset(temp2, select = paste("Q", qBs[j], "_2", sep = ""))[[1]], 
				subset(temp2, select = 	paste("Q", qBs[j], "_3", sep = ""))[[1]], sep = "_")
		} else {

			if(j == 1){
				
				df4[df4$ID == i, namesB[j]] <- subset(temp2, select = qBs[j])[[1]]
			} else if (j == 3){

				df4[df4$ID == i, namesB[j]] <- paste(subset(temp2, select = 
				paste("Q", qBs[j], "_1", 
				sep = ""))[[1]],subset(temp2, select = paste("Q", qBs[j], "_2", sep = ""))[[1]], 
				subset(temp2, select = 	paste("Q", qBs[j], "_3", sep = ""))[[1]], sep = "_")

			}
		}
	}

	namesC <- paste(naming, "C", 1:3, sep ="")
	for (j in 1:3){
		if(j == 3){
			df4[df4$ID == i, namesC[j]] <- subset(temp2, select = paste("Q", qCs[j], sep = ""))[[1]]
		} else{
			if((subset(temp2, select = paste("Q", qCs[j], "_1", sep = ""))[[1]] == 1 & 
			subset(temp2, select = 
			paste("Q", qCs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", qCs[j], "_3", 
			sep = ""))[[1]] == 3 & subset(temp2, select = paste("Q", qCs[j], "_4", sep = ""))[[1]] == 4) |
			(subset(temp2, select = paste("Q", qCs[j], "_1", sep = ""))[[1]] == "" & 
			subset(temp2, select = 
			paste("Q", qCs[j], "_2", sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qCs[j], "_3", 
			sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qCs[j], "_4", sep = ""))[[1]] == "")){
				df4[df4$ID == i, namesC[j]] <- "No Response"
			} else{
				df4[df4$ID == i, namesC[j]] <- paste(subset(temp2, select = 
				paste("Q", qCs[j], "_1", 
				sep = ""))[[1]],subset(temp2, select = paste("Q", qCs[j], "_2", sep = ""))[[1]], 
				subset(temp2, select = 	paste("Q", qCs[j], "_3", sep = ""))[[1]], sep = "_")

			}
		}
	}

	return(df4)
}


########### end helper function

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
	df4[df4$ID == i,]$Preference <- as.character(df2[df2[,1] == i, ]$Q89)
	df4[df4$ID == i,]$Why <- as.character(df2[df2[,1] == i, ]$Q88)
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
		df4 <- getAnswers(df4 = df4,i = i, temp1 = temp1, temp2 = temp2, timing = c(99,100,101), qAs = c(33,34,35),	qBs = c(51,52,53), qCs = c(71,69,72))
	} else if(temp1$TypeA == "circos" & temp1$TypeB == "hammock" & temp1$TypeC == "bar"){
		df4 <- getAnswers(df4 = df4, i = i, temp1 = temp1, temp2 = temp2, timing = c(90,92,91), qAs = c(16,24,25), qBs = c(42,43,44), qCs = c(60,61,62))
	} else if(temp1$TypeA == "circos" & temp1$TypeB == "bar" & temp1$TypeC == "hammock"){
		df4 <- getAnswers(df4 = df4, i = i, temp1 = temp1, temp2 = temp2, timing = c(93:95), qAs = c(27:29),
				qBs = c(45:47), qCs = c(63:65))
	} else if(temp1$TypeA == "hammock" & temp1$TypeB == "circos" & temp1$TypeC == "bar"){
		df4 <- getAnswers(df4 = df4, i = i, temp1 = temp1, temp2 = temp2, timing = c(96:98), qAs = c(30:32),
		qBs = c(48:50), qCs = c(70,67,66))
	}else if(temp1$TypeA == "bar" & temp1$TypeB == "circos" & temp1$TypeC == "hammock"){
		df4 <- getAnswers(df4 = df4, i = i, temp1 = temp1, temp2 = temp2, timing = c(102:104), qAs = c(36:38),
				qBs = c(54:56), qCs = c(73,68,74))
	}else if(temp1$TypeA == "bar" & temp1$TypeB == "hammock" & temp1$TypeC == "circos"){
		print("start")
		df4 <- getAnswers(df4 = df4, i = i, temp1 = temp1, temp2 = temp2, timing = c(105:107), qAs =c(39:41),
				qBs = c(57:59), qCs = c(75:77))
	}
}

#####################
# reshape 

df5 <- data.frame(id = rep(df4$ID, 9), pref = rep(df4$Preference, 9), why = rep(df4$Why, 9), browser = rep(df4$Browser, 9), os = rep(df4$OS, 9), datestart = rep(df4$datestart, 9), totaltime = rep(df4$totaltime, 9), mheight = rep(df4$monitorH, 9), mwidth = rep(df4$monitorW, 9), plottype = c(rep(df4$TypeA, 3), rep(df4$TypeB, 3), rep(df4$TypeC, 3)), dataset = rep(LETTERS[1:3], each = 99), response = c(df4$ResponseA1, df4$ResponseA2, df4$ResponseA3, df4$ResponseB1, df4$ResponseB2, df4$ResponseB3, df4$ResponseB4, df4$ResponseC1, df4$ResponseC2, df4$ResponseC3), pagetime = c(rep(df4$TimeA, 3), rep(df4$TimeB, 3), rep(df4$TimeC, 3)), clicks = c(rep(df4$ClicksA, 3), rep(df4$ClicksB, 3), rep(df4$ClicksC, 3)), qid = rep(rep(1:3, each = 33), 3), blockid = NA  )

block1 <- df4[df4$TypeA == "hammock" & df4$TypeB == "bar" & df4$TypeC == "circos",]$ID
block2 <- df4[df4$TypeA == "circos" & df4$TypeB == "hammock" & df4$TypeC == "bar",]$ID
block3 <- df4[df4$TypeA == "circos" & df4$TypeB == "bar" & df4$TypeC == "hammock",]$ID
block4 <- df4[df4$TypeA == "hammock" & df4$TypeB == "circos" & df4$TypeC == "bar",]$ID
block5 <- df4[df4$TypeA == "bar" & df4$TypeB == "circos" & df4$TypeC == "hammock",]$ID
block6 <- df4[df4$TypeA == "bar" & df4$TypeB == "hammock" & df4$TypeC == "circos",]$ID

df5[df5$id %in% block1,]$blockid <- 1
df5[df5$id %in% block2,]$blockid <- 2
df5[df5$id %in% block3,]$blockid <- 3
df5[df5$id %in% block4,]$blockid <- 4
df5[df5$id %in% block5,]$blockid <- 5
df5[df5$id %in% block6,]$blockid <- 6


# correct response: 3_4_2_1 (low to high) or 1_2_4_3 (high to low)
da1 <- subset(df5, (qid==1) & (dataset=="A"))
answers <- ldply(strsplit(as.character(da1$response), "_"), function(x) x)
da1$correct <- pmax(rowSums(t(t(answers) == c(1,2,4,3))), rowSums(t(t(answers) == c(3,4,2,1))))/4
plots.stat <- ddply(da1, .(plottype), summarize, 
                    n = length(plottype),
                    time = mean(as.numeric(as.character(pagetime)), na.rm=T),
                    sdt = sd(as.numeric(as.character(pagetime)), na.rm=T),
                    correct=mean(correct, na.rm=T),
                    sdc = sd(correct, na.rm=T))
plots.stat
xtabs(~plottype+factor(response), data=da1)

########
# correct response: 4_1_2_3 (high to low) or 1_4_3_2 (low to high)
da2 <- subset(df5, (qid==2) & (dataset=="A"))
answers <- ldply(strsplit(as.character(da2$response), "_"), function(x) {
  if (length(x) != 4) return(c(0,0,0,0))
  as.numeric(x)})
da2$correct <- pmax(rowSums(t(t(answers) == c(4,1,2,3))), rowSums(t(t(answers) == c(3,2,1,4))))/4
plots.stat <- ddply(da2, .(plottype), summarize, 
                    n = length(plottype),
                    time = mean(as.numeric(as.character(pagetime)), na.rm=T),
                    sdt = sd(as.numeric(as.character(pagetime)), na.rm=T),
                    correct=mean(correct, na.rm=T),
                    sdc = sd(correct, na.rm=T))
plots.stat
xtabs(~plottype+factor(response), data=da2)

########
# correct response: 4_1_2_3 (high to low) or 1_4_3_2 (low to high)
da3 <- subset(df5, (qid==3) & (dataset=="A"))
answers <- ldply(strsplit(as.character(da3$response), "_"), function(x) {
  if (length(x) != 4) return(c(0,0,0,0))
  as.numeric(x)})
da3$correct <- pmax(rowSums(t(t(answers) == c(1,4,3,2))), rowSums(t(t(answers) == rev(c(1,4,3,2)))))/4
plots.stat <- ddply(da3, .(plottype), summarize, 
                    n = length(plottype),
                    time = mean(as.numeric(as.character(pagetime)), na.rm=T),
                    sdt = sd(as.numeric(as.character(pagetime)), na.rm=T),
                    correct=mean(correct, na.rm=T),
                    sdc = sd(correct, na.rm=T))
plots.stat
xtabs(~plottype+factor(response), data=da3)

dA <- rbind(da1, da2, da3)
dA$pagetime <- as.numeric(as.character(dA$pagetime))


########
# correct response: 1
db1 <- subset(df5, (qid==1) & (dataset=="B"))
db1$correct <- as.numeric(db1$response == "1")
plots.stat <- ddply(db1, .(plottype), summarize, 
                    n = length(plottype),
                    time = mean(as.numeric(as.character(pagetime)), na.rm=T),
                    sdt = sd(as.numeric(as.character(pagetime)), na.rm=T),
                    correct=mean(correct, na.rm=T),
                    sdc = sd(correct, na.rm=T))
plots.stat
xtabs(~plottype+factor(response), data=db1)


db2 <- subset(df5, (qid==2) & (dataset=="B"))
db1$correct <- as.numeric(db2$response == "1")
plots.stat <- ddply(db2, .(plottype), summarize, 
                    n = length(plottype),
                    time = mean(as.numeric(as.character(pagetime)), na.rm=T),
                    sdt = sd(as.numeric(as.character(pagetime)), na.rm=T),
                    correct=mean(correct, na.rm=T),
                    sdc = sd(correct, na.rm=T))
plots.stat
xtabs(~plottype+factor(response), data=db2)


library(lme4)
a.out <- lmer(as.numeric(I(correct==1))~plottype+(1|id), family=binomial(), data=dA)
summary(a.out)

ta.out <- lmer(log10(pagetime)~plottype+(1|id), data=dA)
summary(ta.out)



