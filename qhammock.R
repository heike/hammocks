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
#case1: lines
  part <- x1$cat[hits[which(hits <= x1$nlines)],]
  if(nrow(part) > 0){
    for(i in 1:nrow(part)){

      h <- h | (x1$var1 == part[i,][x1$variables[1]][[1]] & x1$var2 == part[i,][x1$variables[2]][[1]])

    }
  }
#case2: left bar
  part <- hits[which(hits > x1$nlines & hits <=(x1$nlines + length(x1$values[[1]]) + 1))]
  if(length(part) > 0){
    h <- x1$var1 %in% levels(x1$var1)[part - x1$nlines - 1]
  }
  
#case3: right bar
  part <- hits[which(hits > (x1$nlines + length(x1$values[[1]]) + 1))]
  if(length(part) > 0){

    h <- x1$var2 %in% levels(x1$var2)[part - x1$nlines - 1 - length(levels(x1$var1))]
  }
#    for(i in hits){
#      #are you selecting a line?
#      if(i <= x1$nlines){
#          h <- (x1$var1 == x1$cat[i,][x1$variables[1]][[1]] & x1$var2 == x1$cat[i,][x1$variables[2]][[1]]) | h
#      # are you selecting v1?
#      }
#      else if (i > x1$nlines & i <= (x1$nlines + length(x1$values[[1]]) + 1)){
#         h <- x1$var1 == levels(x1$var1)[i - x1$nlines - 1] |h
#         # are you selecting v2?
#      } else {
#         h <- x1$var2 == levels(x1$var2)[i - x1$nlines - length(levels(x1$var1)) - 1] | h
#      }
#    }

   return(h)
}

.getindex <- function(yleft, yright, meta, lineid){

  newy_poly <- NULL
  for(i in 1:length(yright)){
    
    newy_poly <- c(newy_poly, 
                   yleft[i]  + c(-1, 1) * (0.5 * meta$cat[lineid[i],]["V1"][[1]]),
                   yright[i] + c(1, -1) * (0.5 * meta$cat[lineid[i],]["V1"][[1]]),
                   NA)
    
  }
  return(newy_poly)
}
qhammock <- function(x, variables, freq = NULL, xat = NULL, yat = NULL, width, pal = rainbow(n = 10), main = ""){
  variables <- var_names(vars = variables, data = x)

################# error handling
  if(class(x)[1] == "data.frame"){
    x <- x[order(x[variables[1]][[1]], x[variables[2]][[1]]),]
    
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
      lineid <- NULL
      for(i in 1:nrow(sub)){
        lineid <- c(lineid, which((meta$cat[meta$variables[1]][[1]] == unique(sub[meta$variables[1]][[1]][i])) &
          (meta$cat[meta$variables[2]][[1]] ==  unique(sub[meta$variables[2]][[1]][i]))))
      }
      lineid <- unique(lineid)
      newyleft <- meta$y1[3 * lineid - 2]
      newyright <- meta$y1[3 * lineid - 1]
      newxleft <- meta$x1[3 * lineid - 2]
      newxright <- meta$x1[3 * lineid - 1]
      lineindex <- (rep(lineid, each = 3) * 3 - 2 + (0:2))
      newy_poly <- .getindex(yleft = newyleft, yright = newyright, meta = meta, lineid = lineid)
        qdrawPolygon(painter,
                     y = newy_poly,
                     x = rep(c(rep(min(meta$barxright), 2), rep(max(meta$barxleft), 2), NA), length(lineid)),
                     fill = alpha("black",meta$alpha),
                     stroke = NA)
        
        qdrawLine(painter,
                  x = meta$x1[lineindex],
                  y = meta$y1[lineindex],
                  stroke = 'grey60')

        qdrawRect(painter,
                  xleft = min(meta$barxleft),
                  xright = min(meta$barxright),
                  ytop = newy_poly[c(1:length(lineid) * 5 - 3)],
                  ybottom = newy_poly[c(1:length(lineid) * 5 - 4)],
                  fill = alpha("black", meta$alpha),
                  stroke = NA)
        qdrawRect(painter,
                  xleft = max(meta$barxleft),
                  xright = max(meta$barxright),
                  ytop = newy_poly[c(1:length(lineid) * 5 - 2)],
                  ybottom = newy_poly[c(1:length(lineid) * 5 - 1)],
                  fill = alpha("black", meta$alpha),
                  stroke = NA)

         draw_brush(layer, painter, x, meta)

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
titanic <- qdata(data.frame(Titanic))
qbar(data = titanic, x = 'Class')
qhammock(x = titanic, variables = c("Survived", "Class"), freq = "Freq", width = .1, pal = rainbow(n = 6))
