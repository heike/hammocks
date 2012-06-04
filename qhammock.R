require(cranvas)
require(qtpaint)
require(qtbase)
require(plyr)
require(objectProperties)
require(plumbr)

## todo use b$ .brushed colors for highlighting



Hammocks.meta <- setRefClass("Hammocks_meta", fields  = properties(c(Common.meta,
                    list(x1 = "numeric", y1 = "numeric", barxleft = "numeric", 
                         barxright = "numeric", barytop = "numeric", barybottom = "numeric",
                         nlines  = "numeric", values = "list", cat = 'data.frame', var1 = "factor",
                         var2 = "factor", variables = "character", alpha = "numeric"))))

.findhitsdata <- function(x1, hits, horizontal){

  h <- logical(length(x1$var1))
#case1: lines
  part <- x1$cat[hits[which(hits <= x1$nlines)],]
  if(nrow(part) > 0){

    for(i in 1:nrow(part)){

        h <- h | (x1$var1 == part[i,][x1$variables[1]][[1]] & x1$var2 == part[i,][x1$variables[2]][[1]])

    }
  }
#case2: left or top bar
  part <- hits[which(hits > x1$nlines & hits <= (x1$nlines + length(x1$values[[1]]) + 1))]
  if(length(part) > 0){


    h <- x1$var1 %in% levels(x1$var1)[part - x1$nlines - 1]
  }
  
#case3: right bar
  part <- hits[which(hits > (x1$nlines + length(x1$values[[1]]) + 1))]
  if(length(part) > 0){

    h <- x1$var2 %in% unique(x1$cat[x1$variables[2]])[[1]][part - x1$nlines - 1 - length(unique(x1$cat[x1$variables[1]])[[1]])]
  }

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

.get_identify <- function(meta, hits){
  h <- numeric(0)
  part <- meta$cat[hits[which(hits <= meta$nlines)],]
  if(nrow(part) > 0){
    for(i in 1:nrow(part)){
      h <- c(h, which(meta$var1 == part[i,][meta$variables[1]][[1]] & meta$var2 == part[i,][meta$variables[2]][[1]]))
    }  
  }
  
  part <- hits[which(hits > meta$nlines & hits <= (meta$nlines + length(meta$values[[1]]) + 1))]
  if(length(part) > 0){
    
    
    h <- which(meta$var1 %in% levels(meta$var1)[part - meta$nlines - 1])
  }
  part <- hits[which(hits > (meta$nlines + length(meta$values[[1]]) + 1))]
  if(length(part) > 0){
    
    h <- which(meta$var2 %in% unique(meta$cat[meta$variables[2]])[[1]][part - meta$nlines - 1 - length(unique(meta$cat[meta$variables[1]])[[1]])])
  }
  return(unique(h))
}
qhammock <- function(x, variables, freq = NULL, xat = NULL, yat = NULL, width = 0.2, pal = rainbow(n = 10), 
                     main = "", identify = FALSE, 
                    labels = TRUE, horizontal = FALSE){
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
  b$identify <- identify
  b$label.gen <- function(x){
    sumfreq <- ddply(.data = x, .variables = names(x)[1:2], .fun = function(x){sum(x[names(x)[3]][[1]])})
    names(sumfreq)[3] <- 'nrecords'
#     sumfreq <- t(as.data.frame(sumfreq))
    return(paste(capture.output(print(sumfreq, quote = FALSE, row.names = FALSE)), collapse = "\n"))
  }

	meta <- Hammocks.meta$new(xat = getxat(), yat = getyat(), variables = variables, minor = "", main = main, 
                            active = TRUE, alpha = 0.5, color = x$.color)
  ## Common.meta elements
  meta$limits <- matrix(c( c(-1, 1) * diff(meta$xat) * 2 * width + meta$xat, 
                            c(-.1, 1.1) * sum(x[freq][[1]])), 2)
  meta$brush.size <- c(1, -1) * apply(meta$limits, 2, diff) / 15
  meta$main = main
  

  ## for drawing the bars
  rectx <- sapply(1:length(variables), FUN = function(x){x + c(-1, 1) * width * 0.5 })

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
  meta$ylabels <- format(meta$yat)
  
  if(horizontal){
    print('horizontal')

    meta$limits <- meta$limits[,2:1]
#     meta$limits[,2] <- rev(meta$limits[,2])
    switch_value('x1', 'y1', meta)
    switch_value('barxleft', 'barybottom', meta)
    switch_value('barxright', 'barytop', meta)
    names(meta$values) <- c('V2', 'V1')
    meta$cat <- meta$cat[, c(2,1,3)]
#     meta$variables <- meta$variables[2:1]
    switch_value('xat', 'yat', meta)
    meta$xat <- pretty(c(0, max(meta$xat)))
    meta$brush.size <- rev(meta$brush.size)
#      switch_value('var1', 'var2', meta)
    
    meta$ylabels <- meta$variables
    meta$xlabels <- format(meta$xat)
    
  } else {
    meta$yat <- pretty(c(0, max(meta$yat)))
    meta$ylabels <- format(meta$yat)
    meta$xlabels <- meta$variables
  }
  

############## cranvas action functions
  key_press <- function(layer,  event) {
    if(match_key('B', event)){
      b$identify <- !b$identify
      qupdate(layer.identify)
      qupdate(layer.brush)
    }
    common_key_press(layer, event, x, meta)
  }
  
  key_release <- function(layer, event) {
    if(match_key('B', event)){

      qupdate(layer.identify)
      qupdate(layer.brush)
    }
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
      if(!horizontal){
        newy_poly <- .getindex(yleft = newyleft, yright = newyright, meta = meta, lineid = lineid)
        newx_poly <- rep(c(rep(min(meta$barxright), 2), rep(max(meta$barxleft), 2), NA), length(lineid))
        rect1_ytop <- newy_poly[c(1:length(lineid) * 5 - 3)]
        rect1_ybottom <- newy_poly[c(1:length(lineid) * 5 - 4)]
        rect1_xleft <- min(meta$barxleft)
        rect1_xright <- min(meta$barxright)
        rect2_ytop <- newy_poly[c(1:length(lineid) * 5 - 1)]
        rect2_ybottom <- newy_poly[c(1:length(lineid) * 5 - 2)]
        rect2_xleft <- max(meta$barxleft)
        rect2_xright <- max(meta$barxright)

      } else {
        newy_poly <- rep(c(rep(min(meta$barytop), 2), rep(max(meta$barybottom), 2), NA), length(lineid))
        newx_poly <- .getindex(yleft = newxleft, yright = newxright, meta = meta, lineid = lineid)
        rect1_ytop <- min(meta$barytop)
        rect1_ybottom <- min(meta$barybottom)
        rect1_xleft <- newx_poly[c(1:length(lineid) * 5 - 3)]
        rect1_xright <- newx_poly[c(1:length(lineid) * 5 - 4)]
        rect2_ytop <- max(meta$barytop)
        rect2_ybottom <- max(meta$barybottom)
        rect2_xleft <- newx_poly[c(1:length(lineid) * 5 - 1)]
        rect2_xright <- newx_poly[c(1:length(lineid) * 5 - 2)]
        
      }
      
      qdrawPolygon(painter,
                   y = newy_poly,
                   x = newx_poly,
                   fill = (attr(x, 'Brush')$color),
                   stroke = NA)
      
      qdrawLine(painter,
                x = meta$x1[lineindex],
                y = meta$y1[lineindex],
                stroke = 'grey60')

      qdrawRect(painter,
                xleft = rect1_xleft,
                xright = rect1_xright,
                ytop = rect1_ytop,
                ybottom = rect1_ybottom,
                fill = (attr(x, 'Brush')$color),
                stroke = NA)
     qdrawRect(painter,
                  xleft = rect2_xleft,
                  xright = rect2_xright,
                  ytop = rect2_ytop,
                  ybottom = rect2_ybottom,
                  fill = (attr(x, 'Brush')$color),
                  stroke = NA)
      if(labels){

        qdrawText(painter,
                  text = c(levels(meta$var1), levels(meta$var2)),
                  x = 0.5 * (meta$barxright - meta$barxleft) + meta$barxleft ,
                  y = 0.5 * (meta$barytop - meta$barybottom) + meta$barybottom,
                  color = "grey60",
                  cex = 1)
      }
         

    }
    draw_brush(layer, painter, x, meta)
  }
############## draw the cranvas elements
	scene <- qscene()
  layer.root <- qlayer(scene)
  fix_dimension(layer.root,
                row = list(id = c(0,2), value = c(prefer_height(meta$main),
                                                  prefer_height(meta$xlabels))),
                column = list(id = c(0,2), value = c(prefer_width(meta$ylabels), 10)))
  identify_hover <- function(layer, event){
    if(!b$identify){
      return()
    }
    b$cursor <- 2L
    meta$pos <- as.numeric(event$pos())
    hits <- layer$locate(identify_rect(meta)) + 1
    meta$identified <- .get_identify(meta, hits)
    qupdate(layer.identify)
  }
  
  identify_draw <- function(layer, painter){
    if(b$identify && length(meta$identified)){
      meta$identify.labels <- b$label.gen(x[meta$identified, c(meta$variables, freq)])
      draw_identify(layer, painter, x, meta)
    }
    
  }
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
					      	 fill = c(pal[1:length(unique(meta$cat[meta$variables[1]][[1]]))], 
                            meta$color[1:length(unique(meta$cat[meta$variables[2]][[1]]))]))
    
             if(labels){
                 qdrawText(painter,
                           text = c(as.character(unique(meta$cat[meta$variables[1]][[1]])), 
                                    as.character(unique(meta$cat[meta$variables[2]][[1]]))),
                           x = 0.5 * (meta$barxright - meta$barxleft) + meta$barxleft ,
                           y = 0.5 * (meta$barytop - meta$barybottom) + meta$barybottom,    color = "black",
                           valign = 'center',
                           cex = 1)
                 
            }
            
 						 } , 
             keyPressFun = key_press, 
             keyReleaseFun = key_release,
             mousePressFun = brush_mouse_press, 
             mouseMoveFun = brush_mouse_move,
             mouseReleaseFun = brush_mouse_release,
             hoverMoveFun = identify_hover,  
             limits = qrect(meta$limits))
  

	
    layer.brush <- qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.identify <- qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
    layer.title <- qmtext(meta = meta, side = 3)
    layer.root[0, 1] <- layer.title

    layer.root[2, 1] <- qaxis(meta = meta, side = 1)
    layer.root[1, 0] <- qaxis(meta = meta, side = 2)
    layer.root[1, 1] <- qgrid(meta = meta, xlim = meta$limits[,1], ylim = meta$limits[,2])	
    layer.root[1, 1] <- layer.main
    layer.root[1, 1] <- layer.brush
    layer.root[1, 1] <- layer.identify
  
    view <- qplotView(scene = scene)
    view$setWindowTitle(paste('Hammock plot: ', paste(meta$variables, collapse = ', ')))
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
titanic <- qdata(data.frame(Titanic), color = Class )
new_pal <- function(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1){
  function(n){

    if ((diff(h)%%360) < 1) {
      
      h[2] <- h[2] - 360/6
    }
    rotate <- function(x) (x + h.start)%%360 * direction
    hues <- rotate(seq(h[1], h[2], length = 6))
    hcl(hues, c, l)
  }
}
color_pal(titanic) <- new_pal()

attr(titanic, 'Brush')$color <- alpha('black', .5)


# qbar(data = titanic, x = 'Class')

qhammock(x = titanic, variables = c("Survived", "Class"), freq = "Freq", 
         pal = new_pal()(6)[5:6], horizontal = TRUE)
