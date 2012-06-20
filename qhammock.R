require(cranvas)
require(qtpaint)
require(qtbase)
require(plyr)
require(objectProperties)
require(plumbr)
require(stringr)

## definition for hammocks class @param y frequency table of data

Hammocks.meta <- 
  setRefClass("Hammocks_meta", 
              fields = properties(c(Common.meta, 
                                    list(y = "data.frame", 
                                         freq = "character", 
                                         horizontal = "logical", 
                                         variables = "character", 
                                         width = "numeric", 
                                         pal = "character", 
                                         nlines = "numeric"))))

## --------------------------------------------------------
## --------------------------------------------------------
## 
## helpers
## 
## --------------------------------------------------------
## --------------------------------------------------------
draw_identify<- function (layer, painter, data, meta) 
{
  if (!length(meta$identify.labels)) 
    return()
  b = brush(data)
  qfont(painter) = Qt$QFont("Monospace")
  bgwidth = qstrWidth(painter, meta$identify.labels)
  bgheight = qstrHeight(painter, meta$identify.labels)
  hflag = meta$limits[2] - meta$pos[1] > bgwidth
  vflag = meta$pos[2] - meta$limits[3] > bgheight
  qdrawRect(painter, 
            meta$pos[1], 
            meta$pos[2], 
            meta$pos[1] + ifelse(hflag, 1.25, -1.25) * bgwidth, 
            meta$pos[2] + ifelse(vflag,-1.25, 1.25) * bgheight, 
            stroke = rgb(1, 1, 1, 0.8), 
            fill = rgb(1, 1, 1, 0.8))
  qstrokeColor(painter) = b$label.color
  qdrawText(painter, meta$identify.labels, meta$pos[1] + ifelse(hflag, .1, -.1) * bgwidth, meta$pos[2] + ifelse(vflag,-.1, .1) * bgheight, 
            halign = ifelse(hflag, "left", "right"), 
            valign = ifelse(vflag, "top", "bottom"))
}

identify_rect <- function (meta) 
{
  r = apply(meta$limits, 2, diff)/100
  
  p = meta$pos
  qrect(rbind(p - r, p + r))
}
.new_pal <- function(h = c(0, 360) + 15, 
                     c = 100, 
                     l = 65, 
                     h.start = 0, 
                     direction = 1) {
  function(n) {
    
    if ((diff(h)%%360) < 1) {
      h[2] <- h[2] - 360/(n)
    }
    rotate <- function(x) (x + h.start)%%360 * direction
    hues <- rotate(seq(h[1], h[2], length = n))
    hcl(hues, c, l)
  }
}

.instantiateHammocks <- function(param) {
  ## get the plotting data
  if (is.null(param$freq)) {
    print("Calculating frequencies using nrow")
    y <- ddply(.data = param$x[param$variables], 
               .variables = param$variables, 
               .fun = nrow)
    param$freq <- "V1"
  } else {
    y <- data.frame(param$x[, c(param$variables, param$freq)])
    
  }
  
  if (is.null(param$xat)) {
    xat <- 1:length(param$variables)
  } else {
    xat <- param$xat
  }
  
  if (is.null(param$yat)) {
    yat <- pretty(1:sum(y[, param$freq, ]))
  } else {
    yat <- param$yat
  }
  limits <- matrix(c(c(-1, 1) * diff(xat) * 2 * param$width + xat, 
                     c(-0.1, 1.1) * sum(y[, param$freq])), 2)
  if (param$horizontal) {
    temp <- yat
    yat <- xat
    xat <- temp
    xlabels <- format(xat)
    ylabels <- format(param$variables)
    limits <- limits[, 2:1]
  } else {
    ylabels <- format(yat)
    xlabels <- format(param$variables)
    
  }
  if (is.null(param$minor)) {
    param$minor <- ""
  }
  if (is.null(param$main)) {
    param$main <- ""
  }
  
  if (is.null(param$pal)) {
    param$pal <- 
      .new_pal()(length(unique(unlist(data.frame(param$x[, 
                                                         param$variables])))))
  }
  return(Hammocks.meta$new(y = y, 
                           freq = param$freq, xat = xat, yat = yat, 
                           horizontal = param$horizontal, limits = limits, 
                           xlabels = xlabels, 
                           ylabels = ylabels, minor = param$minor, main = param$main, 
                           variables = param$variables, width = param$width, 
                           pal = param$pal, 
                           brush.size = c(1, -1) * apply(limits, 2, diff)/15, 
                           nlines = length(unique(param$x[, 
                                                          param$variables[1]])) * length(unique(param$x[, 
                                                                                                        param$variables[2]])), 
                           active = TRUE))
}

.getmainplotting <- function(meta) {
  if (meta$horizontal) {
    right <- vector(mode = 'list', length = length(meta$variables))
    left <- vector(mode = 'list', length = length(meta$variables))
    rectbottom <- vector(mode = 'list', length = length(meta$variables))
    recttop <- vector(mode = 'list', length = length(meta$variables))
    linex <- vector(mode = 'list', length = length(meta$variables))
    for(i in 1:length(meta$variables)){
      right[[i]] <- cumsum(ddply(meta$y,
                                 .variables = names(meta$ylabels)[i],
                                 .fun = function(x){
                                   sum(x[meta$freq])})$V1)
      left[[i]] <- c(0, right[[i]][-length(right[[i]])])
      rectbottom[[i]] <- rep(meta$yat[i] - 0.5 * meta$width, 
                             length(unique(meta$y[,meta$variables[i]])))
      recttop[[i]] <- rep(meta$yat[i] + 0.5 * meta$width, 
                          length(unique(meta$y[, meta$variables[i]])))
      linex[[i]] <- cumsum(meta$y[order(meta$y[names(meta$ylabels)[i]]), 
                                  meta$freq])
      linex[[i]] <- (linex[[i]] - c(0, linex[[i]][-length(linex[[i]])])) * 0.5 + 
        c(0, linex[[i]][-length(linex[[i]])])
    }
    
    
    
    liney <- vector(mode = 'list', length = length(meta$variables) - 1)
    
    for(i in 1:(length(meta$variables) - 1)){
      
      liney[[i]] <- rep(c(unique(recttop[[i]]), unique(rectbottom[[i + 1]]), NA),
                        nrow(meta$y[,meta$variables[c(i, i + 1)]]))
      
    }
    liney <- unlist(liney)
    rectleft <- unlist(left)
    rectright <- unlist(right)
    rectbottom <- unlist(rectbottom)
    recttop <- unlist(recttop)
    
    linex1 <- cumsum(meta$y[order(meta$y[names(meta$ylabels)[2]]), 
                            meta$freq])
    linex1 <- (linex1 - c(0, linex1[-length(linex1)])) * 0.5 + 
      c(0, linex1[-length(linex1)])
    
    linex2 <- cumsum(meta$y[order(meta$y[names(meta$ylabels)[1]]), 
                            meta$freq])
    linex2 <- (linex2 - c(0, linex2[-length(linex2)])) * 0.5 + 
      c(0, linex2[-length(linex2)])
    
    linex <- c(rbind(linex2[order(meta$y[names(meta$ylabels)[2]])], 
                     linex1, NA))
    
    
  } else {
    recttop1 <- cumsum(ddply(meta$y, .variables = names(meta$xlabels)[1], 
                             .fun = function(x) {
                               sum(x[meta$freq])
                             })$V1)
    rectbottom1 <- c(0, recttop1[-length(recttop1)])
    
    recttop2 <- cumsum(ddply(meta$y, .variables = names(meta$xlabels)[2], 
                             .fun = function(x) {
                               sum(x[meta$freq])
                             })$V1)
    rectbottom2 <- c(0, recttop2[-length(recttop2)])
    
    recttop <- c(recttop1, recttop2)
    rectbottom <- c(rectbottom1, rectbottom2)
    
    rectright <- c(rep(meta$xat[1] + 0.5 * meta$width, length(unique(meta$y[, 
                                                                            meta$variables[1]]))), rep(meta$xat[2] + 0.5 * meta$width, 
                                                                                                       length(unique(meta$y[, meta$variables[2]]))))
    rectleft <- c(rep(meta$xat[1] - 0.5 * meta$width, length(unique(meta$y[, 
                                                                           meta$variables[1]]))), rep(meta$xat[2] - 0.5 * meta$width, 
                                                                                                      length(unique(meta$y[, meta$variables[2]]))))
    
    linex <- rep(c(min(rectright), max(rectleft), NA), nrow(meta$y))
    liney1 <- cumsum(meta$y[order(meta$y[names(meta$xlabels)[2]]), 
                            meta$freq])
    liney1 <- (liney1 - c(0, liney1[-length(liney1)])) * 0.5 + 
      c(0, liney1[-length(liney1)])
    liney2 <- cumsum(meta$y[order(meta$y[names(meta$xlabels)[1]]), 
                            meta$freq])
    liney2 <- (liney2 - c(0, liney2[-length(liney2)])) * 0.5 + 
      c(0, liney2[-length(liney2)])
    liney <- c(rbind(liney2[order(meta$y[names(meta$xlabels)[2]])], 
                     liney1, NA))
    
  }
  
  
  return(list(rectleft = rectleft, rectright = rectright, rectbottom = rectbottom, 
              recttop = recttop, liney = liney, linex = linex))
}
.getparalines <- function(meta, main_plotvalues){
  if(meta$horizontal){
    
    
  }
}
## my version of identify_rect
.identify_rect <- function (meta, n = 0.01) 
{
  r = n * apply(meta$limits, 2, diff)/2
  p = meta$pos
  qrect(rbind(p - r, p + r))
}

#' @param x the data in qdata or data.frame format @param
#' variables
qhammock <- function(variables, x = last_data(), freq = NULL, 
                     xat = NULL, yat = NULL, width = 0.2, pal = NULL, main = "", identify = FALSE, 
                     reorder = FALSE, labels = TRUE, horizontal = FALSE) {
  
  #############################################################
  ############################################################# 
  ###  data preprocessing
  
  variables <- var_names(vars = sapply(variables, as.character), 
                         data = x)
  
  ################# error handling
  #   if (length(variables) != 2) {
  #     stop("qhammock can only handle 2 variables at this time! 
  #              Please enter variables in form c(X, Y)")
  #     }
  x <- check_data(x)
  
  ###############################################################
  ############################################################### 
  ##  instantiate the hammock object and customize data attributes
  ###############################################################
  ###############################################################
  #    color_pal(titanic) <- .new_pal()
  attr(x, "Brush")$color <- alpha("black", 0.5)
  b <- brush(x)
  b$identify <- FALSE
  b$label.color <- "black"
  b$label.gen <- function(x){
    sumfreq <- ddply(.data = x, .variables = names(x)[1:2], 
                     .fun = function(x){sum(x[names(x)[3]][[1]])})
    names(sumfreq)[3] <- 'nrecords'
    
    return(paste(sapply(X = capture.output(print(sumfreq, quote = FALSE, row.names = FALSE)), FUN = str_trim), collapse = "\n"))
  }
  
  meta <- .instantiateHammocks(environment())
  
  .findhitsdata <- function(meta, hits, horizontal) {
    h <- logical(nrow(x))
    
    # case1: lines
    part <- hits[which(hits <= meta$nlines)]
    part <- meta$y[order(meta$y[, meta$variables[2]]), ][part,]
    if (nrow(part) > 0) {            
      h <- sapply(1:nrow(part), FUN = function(y){ x[, 
                                                     variables[1]] == part[y, ][variables[1]][[1]] & 
                                                       x[, variables[2]] == part[y, ][variables[2]][[1]]})
    }
    # #case2: left or top bar
    part <- hits[which(hits - meta$nlines - length(unique(meta$y[, 
                                                                 variables[1]])) > 1)]
    if (length(part) > 0) {
      h <- x[, variables[2]] %in% levels(meta$y[, 
                                                variables[2]])[part - meta$nlines - 
                                                  length(unique(meta$y[, variables[1]])) - 1]
    }
    # #case3: right bar
    part <- hits[which(hits > meta$nlines & hits < (meta$nlines + 
      length(levels(unlist(meta$y[, variables])))))]
    
    if (length(part) > 0) {
      h <- x[, variables[1]] %in% levels(meta$y[, variables[1]])[part - 
        meta$nlines - 1]
    }
    
    return(h)
  }
  
  ###############################################################
  ############################################################### 
  ## create layer and drawing elements
  ###############################################################
  ###############################################################
  
  key_press <- function(layer, event) {
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
  
  brush_mouse_release <- function(layer, event) {
    brush_mouse_move(layer, event)
    common_mouse_release(layer, event, x, meta)
  }
  
  identify_hover <- function(layer, event) {
    #         meta$pos <- as.numeric(event$pos())
    if (!b$identify) {
      return()
    }
    b$cursor <- 2L
    meta$pos <- as.numeric(event$pos())
    hits <- layer$locate(.identify_rect(meta, n = 0.02 )) + 1
    meta$identified <- which(.findhitsdata(meta, hits))
    #         meta$identified <- .findhitsdata(meta, hits)
    qupdate(layer.identify)
  }
  
  brush_draw <- function(layer, painter) {
    
    main_plotvalues <- .getmainplotting(meta)
    sub <- x[selected(x), ][meta$variables]
    sub <- unique(as.data.frame(sub))
    
    ## draw selected lines when brushing
    lineid <- NULL
    if (nrow(sub) > 0) {
      
      
      lineid <- sapply(1:nrow(sub), FUN = function(x){
        which(meta$y[order(meta$y[,meta$variables[2]]), 
                     meta$variables[1]] == sub[x, meta$variables[1]] &
                       meta$y[order(meta$y[,meta$variables[2]]), 
                              meta$variables[2]] == sub[x, meta$variables[2]])
        
      })
    }
    lineid <- unique(lineid)
    if (length(lineid) > 1) {
      thickness <- meta$y[order(meta$y[, meta$variables[2]]), 
                          meta$freq][lineid]
      newy <- c(rbind(sapply(1:length(lineid), FUN = function(a) {
        rep(c(main_plotvalues$liney[3 * lineid[a] - 2], 
              main_plotvalues$liney[3 * lineid[a] - 1]), each = 2) + 
                rep(c(-0.5, 0.5) * ifelse(!meta$horizontal, thickness[a], 
                                          0), 2)})[c(1, 2, 4, 3), ], NA))
      newx <- c(rbind(sapply(1:length(lineid), FUN = function(a) {
        rep(c(main_plotvalues$linex[3 * lineid[a] - 2], 
              main_plotvalues$linex[3 * lineid[a] - 1]), each = 2) + 
                rep(c(-0.5, 0.5) * ifelse(meta$horizontal, thickness[a], 
                                          0), 2)})[c(1, 2, 4, 3), ], NA))
      
      qdrawPolygon(painter, y = newy, x = newx, fill = (attr(x, 
                                                             "Brush")$color), stroke = NA)
      qdrawLine(painter, y = c(rbind(main_plotvalues$liney[3 * 
        lineid - 2], main_plotvalues$liney[3 * lineid - 1], 
                                     NA)), x = c(rbind(main_plotvalues$linex[3 * lineid - 
                                       2], main_plotvalues$linex[3 * lineid - 1], NA)), stroke = "grey60")
      if (meta$horizontal) {
        qdrawRect(painter, ytop = min(newy, na.rm = T), ybottom = min(newy, 
                                                                      na.rm = T) - meta$width, xleft = newx[5 * (1:length(lineid)) - 
                                                                        4], xright = newx[5 * (1:length(lineid)) - 3], fill = attr(x, 
                                                                                                                                   "Brush")$color, stroke = NA)
        qdrawRect(painter, ytop = max(newy, na.rm = T), ybottom = max(newy, 
                                                                      na.rm = T) + meta$width, xleft = newx[5 * (1:length(lineid)) - 
                                                                        1], xright = newx[5 * (1:length(lineid)) - 2], fill = attr(x, 
                                                                                                                                   "Brush")$color, stroke = NA)
      } else {
        
        qdrawRect(painter, ytop = newy[5 * (1:length(lineid)) - 
          4], ybottom = newy[5 * (1:length(lineid)) - 3], xright = min(newx, 
                                                                       na.rm = T), xleft = min(newx, na.rm = T) - meta$width, 
                  fill = attr(x, "Brush")$color, stroke = NA)
        qdrawRect(painter, ytop = newy[5 * (1:length(lineid)) - 
          1], ybottom = newy[5 * (1:length(lineid)) - 2], xleft = max(newx, 
                                                                      na.rm = T), xright = max(newx, na.rm = T) + meta$width, 
                  fill = attr(x, "Brush")$color, stroke = NA)
      }
    } else if (length(lineid) == 1) {
      thickness <- meta$y[order(meta$y[, meta$variables[2]]), 
                          meta$freq][lineid]
      newy <- c(rep(c(main_plotvalues$liney[3 * lineid - 2], 
                      main_plotvalues$liney[3 * lineid - 1]), each = 2) + 
                        rep(c(-0.5, 0.5) * ifelse(!meta$horizontal, thickness, 
                                                  0), 2))[c(1, 2, 4, 3)]
      newx <- c(rep(c(main_plotvalues$linex[3 * lineid - 2], 
                      main_plotvalues$linex[3 * lineid - 1]), each = 2) + 
                        rep(c(-0.5, 0.5) * ifelse(meta$horizontal, thickness, 
                                                  0), 2))[c(1, 2, 4, 3)]
      
      qdrawPolygon(painter, y = newy, x = newx, fill = (attr(x, 
                                                             "Brush")$color), stroke = NA)
      
      qdrawLine(painter, y = c(main_plotvalues$liney[3 * lineid - 
        2], main_plotvalues$liney[3 * lineid - 1]), x = 
        c(main_plotvalues$linex[3 * lineid - 2], 
          main_plotvalues$linex[3 * lineid - 1]), stroke = "grey60")
      if (meta$horizontal) {
        qdrawRect(painter, ytop = min(newy), ybottom = min(newy) - 
          meta$width, xleft = newx[1], xright = newx[2], fill = attr(x, 
                                                                     "Brush")$color, stroke = NA)
        qdrawRect(painter, ybottom = max(newy), ytop = max(newy) + 
          meta$width, xleft = newx[4], xright = newx[3], fill = attr(x, 
                                                                     "Brush")$color, stroke = NA)
        
      } else {
        qdrawRect(painter, ytop = newy[1], ybottom = newy[2], 
                  xleft = min(newx) - meta$width, xright = min(newx), 
                  fill = attr(x, "Brush")$color, stroke = NA)
        qdrawRect(painter, ytop = newy[4], ybottom = newy[3], 
                  xleft = max(newx), xright = max(newx) + meta$width, 
                  fill = attr(x, "Brush")$color, stroke = NA)
      }
    }
    
    
    draw_brush(layer, painter, x, meta)
  }
  
  identify_draw <- function(layer, painter){
    
    if(b$identify && length(meta$identified)){
      
      sub <- x[meta$identified, meta$variables]
      sub <- ddply(sub, .fun = nrow, .variables = meta$variables)
      names(sub)[3] <- 'nrecords'
      meta$identify.labels <- b$label.gen(sub)
      draw_identify(layer, painter, x, meta)
    }
    
  }
  
  layer.main <- qlayer(paintFun = function(layer, painter) {
    main_plotvalues <- .getmainplotting(meta)
    paralines <- .getparalines(meta, main_plotvalues)
    ## draw the horizontal line segs
    
    #     qdrawLine(painter, 
    #               x = main_plotvalues$linex, 
    #               y = main_plotvalues$liney, 
    #               stroke = "grey60")
    
    qdrawRect(painter, 
              xleft = main_plotvalues$rectleft, 
              xright = main_plotvalues$rectright, 
              ybottom = main_plotvalues$rectbottom, 
              ytop = main_plotvalues$recttop, 
              stroke = "grey60", 
              fill = meta$pal[1:6])
    
    if (labels) {
      
      qdrawText(painter, 
                text = c(as.character(unique(meta$y[, 
                                                    names(meta$variables)[1]])), as.character(unique(meta$y[, 
                                                                                                            names(meta$variables)[2]]))), 
                x = 0.5 * (main_plotvalues$rectright - main_plotvalues$rectleft) + 
                  main_plotvalues$rectleft, 
                y = 0.5 * (main_plotvalues$recttop - main_plotvalues$rectbottom) + 
                  main_plotvalues$rectbottom, color = "black", valign = "center", 
                cex = 1)
      
    }
    
    
  }, 
                       keyPressFun = key_press, 
                       keyReleaseFun = key_release, 
                       mousePressFun = brush_mouse_press, 
                       mouseMoveFun = brush_mouse_move, 
                       mouseReleaseFun = brush_mouse_release, 
                       hoverMoveFun = identify_hover, 
                       limits = qrect(meta$limits))
  
  layer.brush <- qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
  layer.identify <- qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
  ###############################################################
  ## draw the cranvas display
  scene <- qscene()
  layer.root <- qlayer(scene)
  fix_dimension(layer.root, row = list(id = c(0, 2), value = 
    c(prefer_height(meta$main), prefer_height(meta$xlabels))), 
                column = list(id = c(0, 2), value = c(prefer_width(meta$ylabels), 10)))
  
  layer.root[1, 1] <- qgrid(meta = meta, xlim = meta$limits[, 1], 
                            ylim = meta$limits[, 2])
  layer.root[0, 1] <- qmtext(meta = meta, side = 3)
  layer.root[2, 1] <- qaxis(meta = meta, side = 1)
  layer.root[1, 0] <- qaxis(meta = meta, side = 2)
  layer.root[1, 1] <- layer.main
  layer.root[1, 1] <- layer.brush
  layer.root[1, 1] <- layer.identify
  view <- qplotView(scene = scene)
  lis <- add_listener(x, function(i, j) {
    switch(j, .brushed = qupdate(layer.brush))
  })
  view$setWindowTitle(paste("Hammock plot: ", paste(meta$variables, 
                                                    collapse = ", ")))
  print(view)
  
}


## --------------------------------------------------------
## --------------------------------------------------------
## 
## examples
## 
## --------------------------------------------------------
## --------------------------------------------------------

qtitanic <- qdata(titanic, color = Class)
color_pal(qtitanic)<-.new_pal()(6)
## qhammock(x = qtitanic, variables = c('Class', 'Survived'))
qhammock(x = qtitanic, variables = c("Class", "Survived"), 
         horizontal = TRUE)
# temp <- ddply(data.frame(Titanic), c('Class', 'Survived'), .fun
# = function(x){sum(x$Freq)}) names(temp)[3] <- 'Freq' qhammock(x
# = qdata(temp), variables = c('Class', 'Survived'), freq =
# 'Freq') qhammock(x = qdata(temp), variables = c('Class',
# 'Survived'), freq = 'Freq', horizontal = TRUE) 
