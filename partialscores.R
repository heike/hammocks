getAnswers2 <- function(df4,i, temp1, temp2, timing, qAs, qBs = NA, qCs = NA){
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
	namesA <- sapply(X = paste(naming, "A", 1:3, sep = ""), FUN = paste, letters[1:4], 
			sep = "")
	for(j in 1:3){
		for(k in 1:4){
			if(subset(temp2, select = paste("Q", qAs[j], "_1", sep = ""))[[1]] == 1 & subset(temp2, select = 
				paste("Q", qAs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", 
					qAs[j], "_3", 
				sep = ""))[[1]] == 3 & subset(temp2, select = paste("Q", qAs[j], "_4", sep = ""))[[1]] == 4){
				df4[df4$ID == i, namesA[k,j]] <- "No Response"
			} else {
				df4[df4$ID == i, namesA[k, j]] <- subset(temp2, select =paste("Q", qAs[j], 	
					"_",k, sep = ""))[[1]]
			}
		}
	}
	namesB <- sapply(X = paste(naming, "B", 1:3, sep = ""), FUN = paste, letters[1:4], 
			sep = "")


	for(j in 1:3){
		if(j == 2 && ((subset(temp2, select = paste("Q", qBs[j], "_1", sep = ""))[[1]] == 1 & 
			subset(temp2, select = 
			paste("Q", qBs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q", qBs[j], "_3", 
			sep = ""))[[1]] == 3 & subset(temp2, select = paste("Q", qBs[j], "_4", sep = ""))[[1]] == 4) |
			(subset(temp2, select = paste("Q", qBs[j], "_1", sep = ""))[[1]] == "" & 
			subset(temp2, select = 
			paste("Q", qBs[j], "_2", sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qBs[j], "_3", 
			sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qBs[j], "_4", sep = ""))[[1]] == ""))){
			for(k in 1:4){
			    df4[df4$ID == i, namesB[k, j]] <- "No Response"
			 }
		} else if(j == 2){
		    for(k in 1:4){
				df4[df4$ID == i, namesB[k,j]] <- subset(temp2, select = 
					paste("Q", qBs[j], "_", k, sep = ""))[[1]]
			}
		} else {

			if(j == 1){
				
				df4[df4$ID == i, namesB[1,j]] <- subset(temp2, select = qBs[j])[[1]]
			} else if (j == 3){
				for(k in 1:4){
					df4[df4$ID == i, namesB[k, j]] <- subset(temp2, select = 
					paste("Q", qBs[j], "_", k,sep = ""))[[1]]
					
				}

			}
		}
	}

	namesC <- sapply(X = paste(naming, "C", 1:3, sep = ""), FUN = paste, letters[1:4], 
			sep = "")	
	for (j in 1:3){
		if(j == 3){
			print(df4[df4$ID == i, namesC[1, j]])
			print(subset(temp2, select = paste("Q", qCs[j], sep = ""))[[1]])
			df4[df4$ID == i, namesC[1, j]] <- subset(temp2, select = paste("Q", qCs[j], sep = ""))[[1]]
			print("ok2")
		} else{
			for(k in 1:4){
				if((subset(temp2, select = paste("Q", qCs[j], "_1", sep = ""))[[1]] == 1 & 
					subset(temp2, select = 
					paste("Q", qCs[j], "_2", sep = ""))[[1]] == 2 & subset(temp2, select = paste("Q",
					qCs[j], "_3", 
					sep = ""))[[1]] == 3 & subset(temp2, select = paste("Q", qCs[j], "_4", 
					sep = ""))[[1]] == 4) |
					(subset(temp2, select = paste("Q", qCs[j], "_1", sep = ""))[[1]] == "" & 
					subset(temp2, select = 
					paste("Q", qCs[j], "_2", sep = ""))[[1]] == "" & subset(temp2, select = 
					paste("Q", qCs[j], "_3", 
					sep = ""))[[1]] == "" & subset(temp2, select = paste("Q", qCs[j], "_4", 
					sep = ""))[[1]] == "")){
		
					df4[df4$ID == i, namesC[k, j]] <- "No Response"
				} else{
					df4[df4$ID == i, namesC[k, j]] <- subset(temp2, select = 
					paste("Q", qCs[j], "_", k, sep = ""))[[1]]
				}
			}
		}
	}

	return(df4)
}


df7 <- data.frame(ID = unique(df2[,1]), TypeA = NA, ResponseA1a = NA, ResponseA1b = NA, ResponseA1c = NA, 			ResponseA1d = NA, ResponseA2a = NA, ResponseA2b = NA, ResponseA2c = NA, ResponseA2d = NA, 
		ResponseA3a = NA, ResponseA3b = NA, ResponseA3c = NA, ResponseA3d = NA, TimeA = NA, ClicksA = NA, 			TypeB = NA, ResponseB1a = NA, ResponseB2a = NA, ResponseB2b = NA, ResponseB2c = NA, ResponseB2d = NA,
		ResponseB3a = NA, ResponseB3b = NA, ResponseB3c = NA, ResponseB3d = NA,TimeB = NA, ClicksB = NA, 
		TypeC = NA, ResponseC1a = NA, ResponseC1b = NA, ResponseC1c = NA, ResponseC1d = NA, ResponseC2a = NA,
		ResponseC2b = NA, ResponseC2c = NA, ResponseC2d = NA, ResponseC3a = NA, TimeC = NA, ClicksC = NA, 
		Preference = NA, Why = NA, Browser = NA, OS = NA, 
		datestart = NA,	totaltime = NA, monitorW = NA, monitorH = NA)


# get the structural elements for each respondant		
for(i in df7$ID){
	temp <- as.character(df2[df2[,1] == i, 294])
	print(temp)
	df7[df7$ID == i,]$TypeA <- strsplit(temp, ",")[[1]][1]
	df7[df7$ID == i,]$TypeB <- strsplit(temp, ",")[[1]][2]
	df7[df7$ID == i,]$TypeC <- strsplit(temp, ",")[[1]][3]
	df7[df7$ID == i,]$OS <- as.character(df2[df2[,1] == i,]$Q2_3_TEXT)
	df7[df7$ID == i,]$datestart <- as.character(df2[df2[,1] == i,]$V8)
	df7[df7$ID == i,]$totaltime <- round(as.POSIXlt(as.character(df2[df2[,1] == i,]$V9)) - as.POSIXlt(as.character(df2[df2[,1] == i,]$V8)), 2)
	temp <- as.character(df2[df2[,1] == i,]$Q2_4_TEXT)
	df7[df7$ID == i,]$monitorW <- strsplit(temp, "x")[[1]][1]
	df7[df7$ID == i,]$monitorH <- strsplit(temp, "x")[[1]][2]
	df7[df7$ID == i,]$Browser <- as.character(df2[df2[,1] == i,]$Q2_1_TEXT)
	df7[df7$ID == i,]$Preference <- as.character(df2[df2[,1] == i, ]$Q89)
	df7[df7$ID == i,]$Why <- as.character(df2[df2[,1] == i, ]$Q88)
}

# turn the block groupings into eye-readable names
df7 <- data.frame(sapply(df7, FUN = function(x){gsub(x = x, pattern = "horizontal", replacement = "hammock")}), stringsAsFactors = FALSE)
df7 <- data.frame(sapply(df7, FUN = function(x){gsub(x = x, pattern = "vertical", replacement = "bar")}), stringsAsFactors = FALSE)

# convert "" strings (NA from qualtrics metadata gathering) to real NAs
df7[df7$OS == "", "OS"] <- NA
df7[df7$Browser == "", "Browser"] <-NA

# remove leading and trailing spaces
df7$TypeA <- trim(df7$TypeA)
df7$TypeB <- trim(df7$TypeB)
df7$TypeC <- trim(df7$TypeC)



for(i in df7$ID){
	temp1 <- df7[df7$ID == i,]
	temp2 <- df2[df2[,1] == i,]
	## block name: horizontal, vertical,circos
	## questions: 99,3, 33, 34,35 in qualtrics 
	## in raw csv: 
	if(temp1$TypeA == "hammock" & temp1$TypeB == "bar" & temp1$TypeC == "circos"){
		df7 <- getAnswers2(df4 = df7,i = i, temp1 = temp1, temp2 = temp2, timing = c(99,100,101), qAs = c(33,34,35),	qBs = c(51,52,53), qCs = c(71,69,72))
	} else if(temp1$TypeA == "circos" & temp1$TypeB == "hammock" & temp1$TypeC == "bar"){
		df7 <- getAnswers2(df4 = df7, i = i, temp1 = temp1, temp2 = temp2, timing = c(90,92,91), qAs = c(16,24,25), qBs = c(42,43,44), qCs = c(60,61,62))
	} else if(temp1$TypeA == "circos" & temp1$TypeB == "bar" & temp1$TypeC == "hammock"){
		df7 <- getAnswers2(df4 = df7, i = i, temp1 = temp1, temp2 = temp2, timing = c(93:95), qAs = c(27:29),
				qBs = c(45:47), qCs = c(63:65))
	} else if(temp1$TypeA == "hammock" & temp1$TypeB == "circos" & temp1$TypeC == "bar"){
		df7 <- getAnswers2(df4 = df7, i = i, temp1 = temp1, temp2 = temp2, timing = c(96:98), qAs = c(30:32),
		qBs = c(48:50), qCs = c(70,67,66))
	}else if(temp1$TypeA == "bar" & temp1$TypeB == "circos" & temp1$TypeC == "hammock"){
		df7 <- getAnswers2(df4 = df7, i = i, temp1 = temp1, temp2 = temp2, timing = c(102:104), qAs = c(36:38),
				qBs = c(54:56), qCs = c(73,68,74))
	}else if(temp1$TypeA == "bar" & temp1$TypeB == "hammock" & temp1$TypeC == "circos"){
		
		df7 <- getAnswers2(df4 = df7, i = i, temp1 = temp1, temp2 = temp2, timing = c(105:107), qAs =c(39:41),
				qBs = c(57:59), qCs = c(75:77))
	}
}


#####################
# reshape 

df8 <- data.frame(	id = rep(df7$ID, 30), 
					pref = rep(df7$Preference, 30), 
					why = rep(df7$Why, 30), 
					browser = rep(df7$Browser, 30), 
					os = rep(df7$OS, 30), 
					datestart = rep(df7$datestart, 30), 
					totaltime = rep(df7$totaltime, 30), 
					mheight = rep(df7$monitorH, 30), 
					mwidth = rep(df7$monitorW, 30), 
					plottype = c(rep(df7$TypeA, 12), rep(df7$TypeB, 9), rep(df7$TypeC, 9)), 
					dataset = rep(c(rep("A", 12), rep("B", 9), rep("C", 9)), each = 33), 
					response = c(df7$ResponseA1a, df7$ResponseA1b, df7$ResponseA1c, df7$ResponseA1d, df7$ResponseA2a, df7$ResponseA2b, df7$ResponseA2c, df7$ResponseA2d, df7$ResponseA3a, df7$ResponseA3b, df7$ResponseA3c, df7$ResponseA3d, df7$ResponseB1a, df7$ResponseB2a, df7$ResponseB2b, df7$ResponseB2c, df7$ResponseB2d, df7$ResponseB3a, df7$ResponseB3b, df7$ResponseB3c, df7$ResponseB3d, df7$ResponseC1a,  df7$ResponseC1b,  df7$ResponseC1c,  df7$ResponseC1d, df7$ResponseC2a, df7$ResponseC2b, df7$ResponseC2c, df7$ResponseC2d, df7$ResponseC3a), 
					pagetime = c(rep(df7$TimeA, 12), rep(df7$TimeB, 9), rep(df7$TimeC, 9)), 
					clicks = c(rep(df7$ClicksA, 12), rep(df7$ClicksB, 9), rep(df7$ClicksC, 9)), 
					qid = rep(c(rep(1:3, each = 4), c(1, rep(2:3, each = 4), c(rep(1:2, each = 4), 3))), each = 33) ,
					parid = rep(c(rep(letters[1:4], 3), "a", rep(letters[1:4],2 ), rep(letters[1:4], 2), "a"), each = 33), blockid = NA  )

block1 <- df7[df7$TypeA == "hammock" & df7$TypeB == "bar" & df7$TypeC == "circos",]$ID
block2 <- df7[df7$TypeA == "circos" & df7$TypeB == "hammock" & df7$TypeC == "bar",]$ID
block3 <- df7[df7$TypeA == "circos" & df7$TypeB == "bar" & df7$TypeC == "hammock",]$ID
block4 <- df7[df7$TypeA == "hammock" & df7$TypeB == "circos" & df7$TypeC == "bar",]$ID
block5 <- df7[df7$TypeA == "bar" & df7$TypeB == "circos" & df7$TypeC == "hammock",]$ID
block6 <- df7[df7$TypeA == "bar" & df7$TypeB == "hammock" & df7$TypeC == "circos",]$ID

df8[df8$id %in% block1,]$blockid <- 1
df8[df8$id %in% block2,]$blockid <- 2
df8[df8$id %in% block3,]$blockid <- 3
df8[df8$id %in% block4,]$blockid <- 4
df8[df8$id %in% block5,]$blockid <- 5
df8[df8$id %in% block6,]$blockid <- 6

######
# reshape again for heike

key2 <- data.frame(dataset = c(rep("A", 12), rep("B", 9), rep("C", 9)), parid = c(rep(letters[1:4], 3), "a", rep(letters[1:4], 2), rep(letters[1:4], 2), "a"), qid = c(rep(1:3, each = 4), 1, rep(2:3, each = 4), rep(1:2, each = 4), 3), correct = c(1,2,4,3,4,1,2,3,1,4,3,2,1,1,4,3,2,1,1,2,1,1,2,4,3,4,1,2,3,3))

df9 <- cbind(df8[,c("id", "qid", "parid", "dataset", "plottype", "response")], cresponse = NA,correct = NA)
for(i in 1:nrow(df9)){
	# print(df9[i,])
	df9[i,"cresponse"] <- key2[key2$dataset == df9[i, "dataset"] & key2$qid == df9[i, "qid"] & key2$parid == df9[i, "parid"],]$correct  
	df9[i,"correct"] <- (key2[key2$dataset == df9[i, "dataset"] & key2$qid == df9[i, "qid"]& key2$parid == df9[i, "parid"],]$correct == df9[i, "response"])
}

write.csv(df9, file = "gradedresponses2.csv")

## reproduce the plots
frame <- read.csv("gradedresponses2.csv")
frame$newid <- paste(frame$qid, frame$parid, sep = "")
frame$newid <- factor(frame$newid)

#frame$qid <- factor(frame$qid)
correct.mod <- glm(as.numeric(correct)~dataset/newid/plottype-1, data=frame, family=binomial())
summary(correct.mod)  


res <- ddply(frame, .(dataset, newid, plottype), summarize,
      n=length(newid),
      mean=mean(correct),
      sd=sd(correct))

qplot(as.numeric(newid)+(as.numeric(plottype)-2)/15, mean*100, 
      data=res, facets=.~dataset, colour=plottype) + scale_x_continuous(breaks=c(1:12)) + xlab("Question") + ylab("Percentage correct") + 
        geom_errorbar(aes(x=as.numeric(newid)+(as.numeric(plottype)-2)/15, 
                           ymin=pmax(100*(mean-1.96*sd/sqrt(n)), 0), 
                           ymax=pmin(100*(mean+1.96*sd/sqrt(n)), 100)))
ggsave(filename= "partials.png")

