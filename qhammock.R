require(cranvas)
require(qtpaint)
require(qtbase)

## todo specific args internally. 
qhammock <- function(xlim, ylim, xat, yat, limits,x,  width, heights){

##################
################## helper functions
##################
	getxlim <- function(){
		return(xlim)
	}
	
	getylim <- function(){
		return(ylim)
	}
	
	getxat <- function(){
		return(xat)
	}
	
	getyat <- function(){
		return(yat)
	}
	
	getlimits <- function(){
		return(qrect(matrix(limits, 2)))
	}
	
	getx <- function(){
		return(x)
	}
	
	getrectwidths <- function(){
		return(width)
	}
	
	getrectheights <- function(){
		return(heights)
	}
################ end helper functions
	
	
############### transform input variables for cranvas
	l <- getlimits()
	h <- getrectheights()
	y_bottom <- c(0, cumsum(h[[1]])[-length(h[[1]])])
	y_top <- cumsum(h[[1]])
	widths <- c(-1, 1) * getrectwidths()
	rectx <- sapply(getx(), FUN = function(x){
		   x + widths})
	
	x_left <- c(rep(rectx[1,1], length(h[[1]]))  ,rep(rectx[1,2], length(h[[2]])) )
	x_right <- c(rep(rectx[2,1], length(h[[1]])),	rep(rectx[2,2], length(h[[2]])))
	

############## draw the cranvas elements
	scene <- qscene()
    layer.root <- qlayer(scene)
	

	layer.main <- qlayer(paintFun = function(layer, painter){
						 qdrawLine(painter,
								   x = c(1.1,1.9, NA),
								   y = c(745, 61, NA),
								   stroke = "grey60")
						 qdrawRect(painter, 
								   xleft = x_left ,
								   xright = x_right,
								   ybottom = c(0,1490, 0, 325, 610, 1316),
								   ytop = c(1490,2201,325, 610, 1316, 2201  ),
								   fill = rainbow(n = 6))
						 } ,  
								   limits = l)
	
	layer.brush <- qlayer(paintFun = function(layer, painter){
						  }, limits = l)
	layer.root[1, 1] <- qgrid(xat = getxat(), yat = getyat(), xlim = getxlim(), ylim = getylim(), limits = l, minor = "")	
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
qhammock(xat = c(1,2), yat = seq(0, 2000, 500), xlim = c(0, 3), ylim = c(0, 2225), limits = c(0,3,-5,2225),
	x = 1:2, width = .1, heights = list(V1 = c(1490, 711), V2 = c(325, 285, 706, 885)))
