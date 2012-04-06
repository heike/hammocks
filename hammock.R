
gghammock <- function(vars=list(), data, weight=NULL, alpha=0.5, width = 0.25, order = 1, color = NA, label = TRUE, ...) {
  # order will arrange 
  #  1  levels in x in decreasing order (bottom up) and 
  #	 -1 levels in y in decreasing order
  #  0  leave levels unchanged
  ### weight determines thickness of lines
  ### width determines width of bars
  
  ### error checking
  k = length(vars)
  if (k < 2) message("Error: gghammock needs at least two variables. Use vars=list('X', 'Y')")
  
  ### if user doesn't specify the weight, assign value of 1. 
  data$weight <- data[,weight]
  if (is.null(weight)) data$weight <- 1
  
  ## if ordering is selected, organize x and y axis by weight
  # make order a vector of length length(vars)
  order <- rep(order, length=length(vars))
  for (i in 1:length(vars)){
	if (! is.factor(data[,vars[[i]]])) 
  		data[,vars[[i]]] <- factor(data[,vars[[i]]])

    if (order[i] != 0)
      data[,vars[[i]]] <- reorder(data[,vars[[i]]], data$weight, 
                             function(x) if (order[i] > 0) sum(x)
                             			 else -sum(x)
                             )
  }
  
#browser()  
  for (i in 1:length(vars)) {
  	
  	levels(data[,vars[[i]]]) <- paste(vars[[i]], levels(data[,vars[[i]]]), sep=":")
  }
  
  ## helper function
  getRibbons <- function(xid,yid) {    
    ## get the names of the x and y variables
    x <- vars[[xid]]
    y <- vars[[yid]]
    
    xname <- x
    yname <- y
    
    ## create the data table, x, y, and weight
    ## correction: order the table by x, y according to gghammocks ordering
    ##	dfxy <- as.data.frame(xtabs(data$weight~data[,y] + data[,x]))
    dfxy <- as.data.frame(xtabs(data$weight~data[,x] + data[,y]))
    dfxy <- subset(dfxy, Freq > 0)
     
    #names(dfxy)[1:2] <- c(yname, xname)
    ## correction: maintain x, y order as for variable entry
    names(dfxy)[1:2] <- c(xname, yname)
    
    ## get the ordering for data according to x-axis categories
    ## correction: order in the same direction as x, y axis
    ## idx <- order(dfxy[,y], dfxy[,x], decreasing = TRUE)
    idx <- order(dfxy[,x], dfxy[,y], decreasing = FALSE)
    
    ## find the position of X-axis connector
    dfxy$X[idx] <- cumsum(dfxy$Freq[idx])    
    
    ## get the ordering for data according to y-axis categories
    ## correction: order in the same direction as x, y axis
    ##   idx <- order(dfxy[,x], dfxy[,y])
    idx <- order(dfxy[,y], dfxy[,x], decreasing = FALSE)
    ## find the position of the Y-axis connector
    dfxy$Y[idx] <- cumsum(dfxy$Freq[idx])
    
    ## assign row number as id
    dfxy$id <- 1:nrow(dfxy)    
    
    dfm <- melt(dfxy, measure.var=c("X", "Y"))
    levels(dfm$variable) <- c(x,y)    
    
    dfxy$XX <- dfxy[,xname]
    dfxy$YY <- dfxy[,yname]
    
    dfm$Nodeset <- dfm[,yname]    
    
    dfm$offset <- c(width/2,-width/2)[as.numeric(dfm$variable)]
    dfm$xid <- xid - 1
    dfm$yid <- yid
    
    geom_ribbon(aes(x=as.numeric(variable)+offset+xid,ymin=value -Freq, ymax= value, group=id, 
                    fill=Nodeset),	alpha=alpha, data=dfm)
  }
  ## end helper function
  
  
  gr <- list()
  for (i in 1:(length(vars)-1))
    gr[[i]] <- getRibbons(i,i+1)

  	
  dfm <- melt(data[,c("weight", unlist(vars))], id.var="weight")
  names(dfm)[3] <- "Nodeset"

  llabels <- NULL
  if (label) {
	  label.stats <- ddply(dfm, .(variable, Nodeset), summarize,
	                       n = length(weight),
	                       weight=sum(weight)
	                       )
	  maxWeight <- sum(label.stats$weight)/length(unique(label.stats$variable))
	  label.stats$ypos <- cumsum(label.stats$weight)-(as.numeric(label.stats$variable)-1)*maxWeight
	  label.stats$ypos <- label.stats$ypos-label.stats$weight/2
#browser()
	  varnames <- paste(unlist(vars), sep="|", collapse="|")
	  label.stats$Nodeset <- gsub(sprintf("(%s):(.*)",varnames),"\\2", as.character(label.stats$Nodeset))
	llabels <- geom_text(aes(x=variable, y=ypos, label=Nodeset),
	                      colour = "grey30", data=label.stats, angle=90, size=4)  
  }
  ggplot() + geom_bar(aes(weight=weight, x=variable, fill=Nodeset), 
                      colour = color, width=width,data=dfm) + 
             llabels + xlab("")  + gr 
}

