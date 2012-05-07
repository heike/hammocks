require(cranvas)
require(qtpaint)
require(qtbase)
require(plyr)
require(objectProperties)

## todo add correction factor if freq is not specified



Hammocks.meta <- setRefClass("Hammocks_meta", fields  = properties(c(Common.meta)))


qhammock <- function(x, variables, freq = NULL, xat = NULL, yat = NULL, width, pal = rainbow(n = 10), main = ""){
	x <- check_data(x)
	print(attributes(x)$Brush)
	variables <- var_names(vars = variables, data = x)
	x <- x[order(x[variables[1]][[1]], x[variables[2]][[1]]),]

################# error handling
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
	getxlim <- function(vals){
		return(c(-1, 1) * diff(vals) * 2 * width + vals)
	}
	
	getylim <- function(){
		return(c(0, 1.1 * sum(x[freq][[1]])))
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
			return(pretty(sum(x[freq][[1]])))
		} else {
			return(yat)
		}
	}
	
	getlimits <- function(varx, vary){
		return(matrix(c(varx, vary), 2))
	}
	
	getx <- function(){
		return(1:length(variables))
	}
	
	getrectwidths <- function(){
		return(width)
	}
	
	getrectheights <- function(){
		return(list(V1 = ddply(.data = x, .variables = variables[1], .fun = function(x){sum(x[freq][[1]])})$V1,
						  V2 = ddply(.data = x, .variables = variables[2], .fun = function(x){sum(x[freq][[1]])})$V1))
	}
	
	getFreq <- function(x, variables){
		return(ddply(.data = x, .variables = variables, .fun = nrows))
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
	temp <- ddply(.data = x, .variables = variables, .fun = function(x){sum(x[freq][[1]])})
	

	ylines_V1 <- mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)})
	ylines_V1 <- cbind(temp, y = ylines_V1)
	ylines_V1 <- ylines_V1[order(ylines_V1$Class),]$y
	
	
	temp <- temp[order(temp[variables[2]]),]
	ylines_V2 <- mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)})
	ylines <- as.vector(t(data.frame(v1 = ylines_V1, v2 = ylines_V2, NA)))
	
	print(class(x))
	print(head(x))
	b <- brush(x)
	meta <- Hammocks.meta$new(xat = xticks, yat = getyat(), limits = l, minor = "", main = main)
	## brush range: horizontal and vertical
    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15

############## draw the cranvas elements
	scene <- qscene()
    layer.root <- qlayer(scene)
	
	## record the coordinates of the mouse on click
    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, x, meta)
    }
	
## identify segments being brushed when the mouse is moving
    brush_mouse_move = function(layer, event) {

		r <- qrect(update_brush_size(meta, event))

        hits <- layer$locate(r) + 1

		print(hits)
		selected(x) <- mode_selection(selected(x), hits, mode = b$mode)
		common_mouse_move(layer, event, x, meta)
    }
	
    brush_mouse_release = function(layer, event) {
        brush_mouse_move(layer, event)
        common_mouse_release(layer, event, x, meta)
		print(x)
    }

	layer.main <- qlayer(paintFun = function(layer, painter){
						 qdrawLine(painter,
								   x = xlines,
								   y = ylines, 
								   stroke = "grey60")
						 qdrawRect(painter, 
								   xleft = x_left ,
								   xright = x_right,
								   ybottom = y_bottom,
								   ytop = y_top,
								   fill = pal)
						 } , mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
						 mouseMoveFun = brush_mouse_move,
								   limits = qrect(meta$limits))
	
	
	layer.brush <- qlayer(paintFun = function(layer, painter){
						  }, limits = qrect(meta$limits))
	layer.root[1, 1] <- qgrid(meta = meta, xlim = xlim, ylim = ylim)	
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
titanic <- qdata(data.frame(Titanic))
qhammock(x = titanic, variables = c("Survived", "Class"), freq = "Freq", width = .1, pal = rainbow(n = 6))
