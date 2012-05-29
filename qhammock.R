require(cranvas)
require(qtpaint)
require(qtbase)
require(plyr)
require(objectProperties)
require(plumbr)

## todo convert values for connecting lines into a qdata object: form = x0,x1,y0,y1



Hammocks.meta <- setRefClass("Hammocks_meta", fields  = properties(c(Common.meta,
                    list(x1 = "numeric", y1 = "numeric", barxleft = "numeric", 
                         barxright = "numeric", barytop = "numeric", barybottom = "numeric",
                         nlines  = "numeric", values = "list", cat = 'data.frame', var1 = "factor",
                         var2 = "factor", variables = "character", alpha = "numeric"))))

.findhitsdata <- function(x1, hits, idx){
  h <- logical(length(x1$var1))

   for(i in hits){
     #are you selecting a line?
     if(i <= x1$nlines){
         h <- (x1$var1 == x1$cat[i,][x1$variables[1]][[1]] & x1$var2 == x1$cat[i,][x1$variables[2]][[1]])
     # are you selecting v1?
     }
     else if (i > x1$nlines & i <= (x1$nlines + length(x1$values[[1]]) + 1)){
        h <- x1$var1 == levels(x1$var1)[i - x1$nlines - 1]
        # are you selecting v2?
     } else {
        h <- x1$var2 == levels(x1$var2)[i - x1$nlines - length(levels(x1$var1)) - 1]
     }
   }
   return(h)
}

# .findhitsbars <- function(x1, hits, idx){
#   h <- logical(length(x1$var1))
#   for(i in hits){
#     if (i > x1$nlines & i <= (x1$nlines + length(x1$values[[1]]) + 1)){
#       h <- x1$var1 == levels(x1$var1)[i - x1$nlines - 1]
#       # are you selecting v2?
#     } else {
#       h <- x1$var2 == levels(x1$var2)[i - x1$nlines - length(factor(x1$var1)) - 1]
#     }
#   }
#   return(h)
# }
#    

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

	
############### transform input variables for cranvas [create meta object]
	x <- x[order(x[variables[1]][[1]], x[variables[2]][[1]]),]
  x <- check_data(x)
  b <- brush(x)

	meta <- Hammocks.meta$new(xat = getxat(), yat = getyat(), variables = variables, minor = "", main = main, 
                            active = TRUE, alpha = 0.5)
  ## Common.meta elements
  meta$limits <- matrix(c( c(-1, 1) * diff(meta$xat) * 2 * width + meta$xat, 
                            c(0, 1.1 * sum(x[freq][[1]]))), 2)
  meta$brush.size <- c(1, -1) * apply(meta$limits, 2, diff) / 15
  

  ## for drawing the bars
  rectx <- sapply(1:length(variables), FUN = function(x){x + c(-1, 1) * width})
  meta$values <- list(V1 = ddply(.data = x, .variables = variables[1], .fun = function(x){sum(x[freq][[1]])})$V1,
            V2 = ddply(.data = x, .variables = variables[2], .fun = function(x){sum(x[freq][[1]])})$V1)

  meta$barxleft <- c(rep(rectx[1,1], length(meta$values[[1]]))  ,rep(rectx[1,2], length(meta$values[[2]])) )
  meta$barxright <- c(rep(rectx[2,1], length(meta$values[[1]])),  rep(rectx[2,2], length(meta$values[[2]])))
  meta$barytop <-  c(cumsum(meta$values$V1), cumsum(meta$values$V2))
  meta$barybottom <- c(0,cumsum(meta$values$V1)[-length(meta$values$V1)], 0, cumsum(meta$values$V2)[-length(meta$values$V2)])
  
  ## for drawing the connecting lines
  temp <- ddply(.data = x, .variables = variables, .fun = function(x){sum(x[freq][[1]])})
  ylines_V1 <- cbind(temp, y = mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)}))
  ylines_V1 <- ylines_V1[order(ylines_V1[variables[2]]),]$y
  temp <- temp[order(temp[variables[2]]),]
  ylines_V2 <- mapply(y = 1:length(temp$V1), FUN = function(y){max(0, cumsum(temp$V1)[y - 1]) + (temp$V1[y] / 2)})
  
  meta$x1 <- rep(c(rectx[2,1], rectx[1,2], NA), length(meta$values$V1) * length(meta$values$V2))
  meta$y1 <-  as.vector(t(data.frame(v1 = ylines_V1, v2 = ylines_V2, NA)))

  ## describe the chart

  meta$nlines <- length(ylines_V1)
  meta$cat <- temp
  meta$var1 <- factor(x[meta$variables[1]][[1]])
  meta$var2 <- factor(x[meta$variables[2]][[1]])
############## cranvas action functions
  key_press <- function(layer,  event) {

    common_key_press(layer, event, x, meta)
  }
  
  key_release <- function(layer, event) {
    common_key_release(layer, event, x, meta)
  }
  
  brush_mouse_press <- function(layer, event) {
    common_mouse_press(layer, event, x, meta)
  }
  
  brush_mouse_move <- function(layer, event) {
    rect <- qrect(update_brush_size(meta, event))
    hits <- layer$locate(rect) + 1
    if (length(hits)) {
       hits <- .findhitsdata(meta, hits)
    }
    selected(x) <- mode_selection(selected(x), hits, mode = b$mode)
    common_mouse_move(layer, event, x, meta)
  }
  
  brush_mouse_release <- function(layer, event){
    brush_mouse_move(layer, event)
    common_mouse_release(layer, event, x, meta)
  }

  
  brush_draw <- function(layer, painter){
    sub <- x[selected(x),][meta$variables]

    if(nrow(sub) > 0){
      if(length(unique(sub[meta$variables[1]][[1]])) == 1 & length(unique(sub[meta$variables[2]][[1]])) == 1){
        lineid <- which((meta$cat[meta$variables[1]][[1]]%in% unique(sub[meta$variables[1]][[1]])) &
                    (meta$cat[meta$variables[2]][[1]]%in% unique(sub[meta$variables[2]][[1]])))
        newxleft <- meta$x1[3 * lineid - 2]
        newxright <- meta$x1[3 * lineid - 1]
        newyleft <- meta$y1[3 * lineid - 2]
        newyright <- meta$y1[3 * lineid - 1]
        newyleft_poly <- newyleft  + c(-1, 1) * (0.5 * meta$cat[lineid,]["V1"][[1]])
        newyright_poly <- newyright + c(1, -1) * (0.5 * meta$cat[lineid,]["V1"][[1]])
  #  print(c(rep(newxleft, 2), rep(newxright, 2)))
  #  print(c(newyleft_poly, newyright_poly))
        qdrawPolygon(painter,
                  x = c(rep(newxleft, 2), rep(newxright, 2)),
                  y = c(newyleft_poly, newyright_poly),
               fill = alpha("yellow", meta$alpha),
               stroke = NA
               )
        qdrawRect(painter,
              xleft = min(meta$barxleft),
              xright = min(meta$barxright),
              ytop = max(newyleft_poly),
              ybottom = min(newyleft_poly),
              fill = alpha("black", meta$alpha))
        qdrawRect(painter,
              xleft = max(meta$barxleft),
              xright = max(meta$barxright),
              ytop = max(newyright_poly),
              ybottom = min(newyright_poly),
              fill = alpha("black", meta$alpha))
        qdrawLine(painter,
              x = c(newxleft, newxright),
              y = c(newyleft, newyright),
              stroke = "grey60")
        draw_brush(layer, painter, x, meta)
      } else if(length(unique(sub[meta$variables[1]][[1]])) == 1) {
     
        qdrawRect(painter,
                  xleft = min(meta$barxleft),
                  xright = min(meta$barxright),
                  ytop = meta$barytop[which(levels(sub[meta$variables[1]][[1]]) == unique(sub[meta$variables[1]][[1]]))],
                  ybottom = meta$barybottom[which(levels(sub[meta$variables[1]][[1]]) == unique(sub[meta$variables[1]][[1]]))],
                  fill = alpha("black", meta$alpha))
        draw_brush(layer, painter, x, meta)
      } else if(length(unique(sub[meta$variables[2]][[1]])) == 1) {

        qdrawRect(painter,
                  xleft = max(meta$barxleft),
                  xright = max(meta$barxright),
                  ytop = meta$barytop[which(levels(sub[meta$variables[2]][[1]]) == unique(sub[meta$variables[2]][[1]]))  + length(unique(meta$var1))],
                  ybottom = meta$barybottom[which(levels(sub[meta$variables[2]][[1]]) == unique(sub[meta$variables[2]][[1]])) + length(unique(meta$var1))],
                  fill = alpha("black", meta$alpha))
        draw_brush(layer, painter, x, meta)
      }
    }
  }
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
 						 } , 
             keyPressFun = key_press, 
             keyReleaseFun = key_release,
             mousePressFun = brush_mouse_press, 
             mouseMoveFun = brush_mouse_move,
             mouseReleaseFun = brush_mouse_release,

             limits = qrect(meta$limits))
	
    layer.brush <- qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.root[1, 1] <- qgrid(meta = meta, xlim = meta$limits[,1], ylim = meta$limits[,2])	
    layer.root[1, 1] <- layer.main
    layer.root[1, 1] <- layer.brush
  
    view <- qplotView(scene = scene)
  
    lis <- add_listener(x, function(i, j){
        switch(j,
                .brushed = qupdate(layer.brush))})
    
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
