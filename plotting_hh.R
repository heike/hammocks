setwd('~/papers/2012 BioVis/hammocks/other files')
frame <- read.csv("gradedresponses.csv")

frame$qid <- factor(frame$qid)
correct.mod <- glm(as.numeric(correct)~dataset/qid/plottype-1, data=frame, family=binomial())
summary(correct.mod)  


res <- ddply(frame, .(dataset, qid, plottype), summarize,
      n=length(qid),
      mean=mean(correct),
      sd=sd(correct))

qplot(as.numeric(qid)+(as.numeric(plottype)-2)/15, mean*100, 
      data=res, facets=.~dataset, colour=plottype) + scale_x_continuous(breaks=c(1:3)) + xlab("Question") + ylab("Percentage correct") + 
        geom_errorbar(aes(x=as.numeric(qid)+(as.numeric(plottype)-2)/15, 
                           ymin=pmax(100*(mean-1.96*sd/sqrt(n)), 0), 
                           ymax=pmin(100*(mean+1.96*sd/sqrt(n)), 100)))

######################
frame2 <- read.csv("gradedresponses2.csv")

frame <- ddply(frame2, .(id, qid, dataset, plottype), summarize,
               n=length(response),
               correct=sum(correct))
frame$qid <- factor(frame$qid)
correct.mod <- glm(cbind(correct, n-correct)~dataset/qid/plottype-1, data=frame, family=binomial())
summary(correct.mod)  


res <- ddply(frame, .(dataset, qid, plottype), summarize,
             n=length(qid),
             mean=mean(correct/n),
             sd=sd(correct/n))

qplot(as.numeric(qid)+(as.numeric(plottype)-2)/15, mean*100, 
      data=res, facets=.~dataset, colour=plottype) + scale_x_continuous(breaks=c(1:3)) + xlab("Question") + ylab("Percentage correct") + 
        geom_errorbar(aes(x=as.numeric(qid)+(as.numeric(plottype)-2)/15, 
                          ymin=pmax(100*(mean-1.96*sd/sqrt(n)), 0), 
                          ymax=pmin(100*(mean+1.96*sd/sqrt(n)), 100)))

