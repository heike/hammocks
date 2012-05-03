require(cranvas)
require(qtpaint)
require(qtbase)
require(ddply)

## todo correct the lines for connections.

qhammock <- function(x, variables, freq, xat = NULL, yat = NULL, width, pal = rainbow(n = 10)){
	
################# error handling
	if(length(variables) != 2){
		stop("qhammock can only handle 2 variables at this time!")
	}

##################
################## helper functions
##################
	getxlim <- function(vals){
		return(c(-1, 1) * diff(vals) * 2 * width + vals)
	}
	
	getylim <- function(){
		return(c(0, 1.1 * sum(x[freq])))
	}
	
	getxat <- function(){
		if(is.null(xat)){
			return(1:length(variables))
		} else {
			return(xat)
		}
	}
	
	getyat <- function(){
		if(is.null(yat)){
			return(sum(x[freq]))
		} else {
			return(yat)
		}
	}
	
	getlimits <- function(varx, vary){
		return(qrect(matrix(c(varx, vary), 2)))
	}
	
	getx <- function(){
		return(1:length(variables))
	}
	
	getrectwidths <- function(){
		return(width)
	}
	
	getrectheights <- function(){
		return(list(V1 = ddply(.data = x, .variables = variables[1], .fun = function(x){sum(x[freq])})$V1,
						  V2 = ddply(.data = x, .variables = variables[2], .fun = function(x){sum(x[freq])})$V1))
	}
################ end helper functions

	
############### transform input variables for cranvas
	
	h <- getrectheights()
	xticks <- getxat()
	xlim <- getxlim(xticks)
	yticks <- getyat()
	ylim <- getylim()
	l <- getlimits(xlim, ylim)
	
	## for the y position of bars
	y_bottom <- c(0,cumsum(h$V1)[-length(h$V1)], 0, cumsum(h$V2)[-length(h$V2)])
	y_top <- c(cumsum(h$V1), cumsum(h$V2))

	
	## for the x position of bars
	widths <- c(-1, 1) * getrectwidths()
	rectx <- sapply(getx(), FUN = function(x){
		   x + widths})
	x_left <- c(rep(rectx[1,1], length(h[[1]]))  ,rep(rectx[1,2], length(h[[2]])) )
	x_right <- c(rep(rectx[2,1], length(h[[1]])),	rep(rectx[2,2], length(h[[2]])))
	
	
	## for the x position of lines
	xlines <- rep(c(rectx[2,1], rectx[1,2], NA), length(h$V1) * length(h$V2))
	
	## for the y position of lines
	ylines_V1 <- rep(mapply(y = 1:length(h$V1), FUN = function(y){max(0, cumsum(h$V1)[y - 1]) + (h$V1[y] / 2)}),  length(h$V2))
	temp <- ddply(.data = x, .variables = variables, .fun = function(x){sum(x[freq])})
	temp <- temp[order(temp[variables[2]]),]
	ylines_V2 <- mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)})
	ylines <- as.vector(t(data.frame(v1 = ylines_V1, v2 = ylines_V2, NA)))
	
print(ylines)
############## draw the cranvas elements
	scene <- qscene()
    layer.root <- qlayer(scene)
	

	layer.main <- qlayer(paintFun = function(layer, painter){
						 qdrawLine(painter,
x = xlines,
								   y = ylines, 
#								   x = c(1.1,1.9,NA),
#								   y = c(745, 61, NA),
								   stroke = "grey60")
						 qdrawRect(painter, 
								   xleft = x_left ,
								   xright = x_right,
								   ybottom = y_bottom,
								   ytop = y_top,
								   fill = pal)
						 } ,  
								   limits = l)
	
	layer.brush <- qlayer(paintFun = function(layer, painter){
						  }, limits = l)
	layer.root[1, 1] <- qgrid(xat = xticks, yat = getyat(), xlim = xlim, ylim = getylim(), limits = l, minor = "")	
    layer.root[1, 1] <- layer.main
#layer.root[1, 1] <- layer.brush
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
