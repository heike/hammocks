require(cranvas)
require(qtpaint)
require(qtbase)
require(plyr)
require(objectProperties)

## todo convert values for connecting lines into a qdata object: form = x0,x1,y0,y1



Hammocks.meta <- setRefClass("Hammocks_meta", fields  = properties(c(Common.meta,
                    list(x1 = "numeric", y1 = "numeric", barxleft = "numeric", 
                         barxright = "numeric", barytop = "numeric", barybottom = "numeric"))))


qhammock <- function(x, variables, freq = NULL, xat = NULL, yat = NULL, width, pal = rainbow(n = 10), main = ""){
  variables <- var_names(vars = variables, data = x)

################# error handling
  if(class(x) != "data.frame"){
    stop("qhammock only handles data.frame input")
  }
	if(length(variables) != 2){
		stop("qhammock can only handle 2 variables at this time! Please enter variables in form c(X, Y)")
	}
	
	if(is.null(freq)){
		print("Using number of records to determine frequency")
		x <- getFreq(x, variables)
		freq <- "V1"
	}

##################
################## helper functions
##################
	getxat <- function(){
		if(is.null(xat)){
			return(1:length(variables))
		} else {
			return(xat)
		}
	}
	
	getyat <- function(){
		if(is.null(yat)){
			return(pretty(sum(x[freq][[1]])))
		} else {
			return(yat)
		}
	}
	
	getFreq <- function(x, variables){
		return(ddply(.data = x, .variables = variables, .fun = nrows))
	}
			
################ end helper functions

	
############### transform input variables for cranvas
	x <- x[order(x[variables[1]][[1]], x[variables[2]][[1]]),]
  x <- check_data(x)
  b <- brush(x)

	meta <- Hammocks.meta$new(xat = getxat(), yat = getyat(), minor = "", main = main, active = TRUE)
  ## Common.meta elements
  meta$limits <- matrix(c( c(-1, 1) * diff(meta$xat) * 2 * width + meta$xat, 
                            c(0, 1.1 * sum(x[freq][[1]]))), 2)
  meta$brush.size <- c(1, -1) * apply(meta$limits, 2, diff) / 15
  
  ## for drawing the bars
  widths <- c(-1, 1) * width
  rectx <- sapply(1:length(variables), FUN = function(x){x + widths})
  h <- list(V1 = ddply(.data = x, .variables = variables[1], .fun = function(x){sum(x[freq][[1]])})$V1,
            V2 = ddply(.data = x, .variables = variables[2], .fun = function(x){sum(x[freq][[1]])})$V1)
  meta$barxleft <- c(rep(rectx[1,1], length(h[[1]]))  ,rep(rectx[1,2], length(h[[2]])) )
  meta$barxright <- c(rep(rectx[2,1], length(h[[1]])),  rep(rectx[2,2], length(h[[2]])))
  meta$barytop <-  c(cumsum(h$V1), cumsum(h$V2))
  meta$barybottom <- c(0,cumsum(h$V1)[-length(h$V1)], 0, cumsum(h$V2)[-length(h$V2)])
  
  ## for drawing the connecting lines
  temp <- ddply(.data = x, .variables = variables, .fun = function(x){sum(x[freq][[1]])})
  ylines_V1 <- cbind(temp, y = mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)}))
  ylines_V1 <- ylines_V1[order(ylines_V1[variables[2]]),]$y
  temp <- temp[order(temp[variables[2]]),]
  ylines_V2 <- mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)})
  
  meta$x1 <- rep(c(rectx[2,1], rectx[1,2], NA), length(h$V1) * length(h$V2))
  meta$y1 <-  as.vector(t(data.frame(v1 = ylines_V1, v2 = ylines_V2, NA)))

############## draw the cranvas elements
	scene <- qscene()
  layer.root <- qlayer(scene)
	

	layer.main <- qlayer(paintFun = function(layer, painter){
						 qdrawLine(painter,
								   x = meta$x1,
								   y = meta$y1, 
								   stroke = "grey60")
						 qdrawRect(painter, 
								   xleft = meta$barxleft ,
								   xright = meta$barxright,
								   ybottom = meta$barybottom,
								   ytop = meta$barytop,
								   fill = pal)
 						 } , limits = qrect(meta$limits))
	
  
    layer.root[1, 1] <- qgrid(meta = meta, xlim = meta$limits[,1], ylim = meta$limits[,2])	
    layer.root[1, 1] <- layer.main
    view <- qplotView(scene = scene)
    print(view)

}

#Survived   V1
#1       No 1490
#2      Yes  711

#Class  V1
#1   1st 325
#2   2nd 285
#3   3rd 706
#4  Crew 885
titanic <- data.frame(Titanic)
qhammock(x = titanic, variables = c("Survived", "Class"), freq = "Freq", width = .1, pal = rainbow(n = 6))
