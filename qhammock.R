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
              list(y = "data.frame",  ## frequency table
                   freq = "character", ## name of  column holding frequency values in y
                   horizontal = "logical", ## orientation of plot
                   variables = "character", ## name of variables
                   width = "numeric", ## size of bars, 0 to 1
                   pal = "character", ## palatte
                   nlines = "numeric", ## number of connecting lines between bars
                   main_plotvalues = 'list', ## values used to draw the bars, in plot coordinates
                   paralines = 'list', ## subset plot description by axis set
                   r = 'list', ## values to draw the ribbons in plot coordinates
                   hits = 'numeric', ## highlighted values
                   labels = 'logical' ## whether to display labels
                   ))))

## --------------------------------------------------------
## --------------------------------------------------------
## 
## helpers
## 
## --------------------------------------------------------
## --------------------------------------------------------


## local override for cranvas::draw_identify
draw_identify<- function (layer, painter, data, meta) {
  
  if (!length(meta$identify.labels))     return()
  b <- brush(data)
  qfont(painter) <- Qt$QFont("Monospace")
  bgwidth <- qstrWidth(painter, meta$identify.labels)
  bgheight <- qstrHeight(painter, meta$identify.labels)
  hflag <- meta$limits[2] - meta$pos[1] > bgwidth
  vflag <- meta$pos[2] - meta$limits[3] > bgheight
  qdrawRect(painter, 
            meta$pos[1], 
            meta$pos[2], 
            meta$pos[1] + ifelse(hflag, 1.25, -1.25) * bgwidth, 
            meta$pos[2] + ifelse(vflag,-1.25, 1.25) * bgheight, 
            stroke = rgb(1, 1, 1, 0.8), 
            fill = rgb(1, 1, 1, 0.8))
  qstrokeColor(painter) = b$label.color
  qdrawText(painter, 
            meta$identify.labels, 
            meta$pos[1] + ifelse(hflag, .1, -.1) * bgwidth, 
            meta$pos[2] + ifelse(vflag,-.1, .1) * bgheight, 
            halign = ifelse(hflag, "left", "right"), 
            valign = ifelse(vflag, "top", "bottom"))

}
# 
# ## local override for cranvas::identify_rect
# identify_rect <- function (meta, pct = 0.01){
#   r = pct * apply(meta$limits, 2, diff)
#   p = meta$pos
#   qrect(rbind(p - r, p + r))
# }

## instantiate the coloring scheme
## set the palette and variables to map data to aesthetics
## similar to 'color_pal<-'
.new_pal <- function(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1) {
  function(n) {
    
    if ((diff(h)%%360) < 1) {
      h[2] <- h[2] - 360/(n)
    }
    rotate <- function(x) (x + h.start)%%360 * direction
    hues <- rotate(seq(h[1], h[2], length = n))
    hcl(hues, c, l)
  }
}

## instantiate a hammocks meta object
.instantiateHammocks <- function(param) {
  
  ## get the frequency table
  if (is.null(param$freq)) {
    print("Calculating frequencies using nrow")
    y <- ddply(.data = param$x[param$variables], 
               .variables = param$variables, 
               .fun = nrow)
    param$freq <- "V1"
  } else {
    y <- data.frame(param$x[, c(param$variables, param$freq)])
    
  }
  
  ## factoring variables for ordering
  for(i in param$variables){
    if(!is.factor(y[, i])){
      y[,i] <- factor(y[,i])
    }
  }

  ## determine location of xticks
  if (is.null(param$xat)) {
    xat <- 1:length(param$variables)
  } else {
    xat <- param$xat
  }

  ## determine location of yticks
  if (is.null(param$yat)) {
    yat <- pretty(1:sum(y[, param$freq, ]))
  } else {
    yat <- param$yat
  }
  
  ## determine layer limits
  ## used to ensure that all layers are displayed on same scale
  limits <- matrix(c(c(-1, 1) * diff(range(xat)) * 2 * param$width + range(xat), 
                     c(-0.1, 1.1) * sum(y[, param$freq])), 2)
  
  ## some switching of variables due to orientation
  if (param$horizontal) {
    temp <- yat
    yat <- xat
    xat <- temp
    xlabels <- format(xat, trim = TRUE)
    ylabels <- format(param$variables, trim = TRUE)
    limits <- limits[, 2:1]
  } else {
    ylabels <- format(yat, trim = TRUE)
    xlabels <- format(param$variables, trim = TRUE)
    
  }
  
  ## Common.meta$minor variable
  ## whether to draw minor grid lines on axis(es)
  ## default '' == none
  if (is.null(param$minor)) {
    param$minor <- ""
  }
  
  ## Common.meta$main variable
  ## plot title
  ## default '' == none
  if (is.null(param$main)) {
    param$main <- ""
  }
  
  ## palatte function
  if (is.null(param$pal)) {
    param$pal <- 
      .new_pal()(length(unique(unlist(data.frame(param$x[, 
                                                         param$variables])))))
  }
 
  return(Hammocks.meta$new(y = y, labels = param$labels,
                           freq = param$freq, xat = xat, yat = yat, 
                           horizontal = param$horizontal, limits = limits, 
                           xlabels = xlabels, 
                           ylabels = ylabels, minor = param$minor, main = param$main, 
                           variables = param$variables, width = param$width, 
                           pal = param$pal, 
                           brush.size = c(1, -1) * apply(limits, 2, diff)/15, 
                           nlines = cumsum(unlist(sapply(1:length(param$variables), FUN = function(x){if(x > 1){
                             length(unique(y[,param$variables[x]])) * length(unique(y[,param$variables[x - 1]]))
                           }}))), 
                           active = TRUE))

}

## from the meta elements, get values in plot coordinates to draw the bars
.getmainplotting <- function(meta) {

 
  ## identify which axis are for bars
  if (meta$horizontal) {
    selectlabels <- meta$ylabels
    selectaxis <- meta$yat
  } else {
    selectlabels <- meta$xlabels
    selectaxis <- meta$xat
  }
  
  ## populate the variables
  right <- sapply(meta$variables,
               FUN = function(x){
                 cumsum(ddply(meta$y,
                              .variables = x,
                              .fun = function(a){sum(a[meta$freq])})$V1)})
  left <- llply(right,
              .fun = function(x){c(0, x[-length(x)])})
  rectbottom <- sapply( 1:length(selectaxis), 
                        FUN = function(x){rep(selectaxis[x] - 0.5 * meta$width,
                                 length(levels(meta$y[, meta$variables[x]])))})
  recttop <- sapply( 1:length(selectaxis), 
                      FUN = function(x){rep(selectaxis[x] + 0.5 * meta$width,
                                            length(levels(meta$y[, meta$variables[x]])))})
  linex <- sapply(2:length(meta$variables),
               FUN = function(x){
                 tmp1 <- cumsum(ddply(meta$y[order(meta$y[names(meta$variables)[x - 1]]),],
                       .variables = meta$variables[c(x - 1, x)],
                       .fun = function(a){sum(a[meta$freq])})$V1)
                 tmp <- ddply(meta$y[order(meta$y[names(meta$variables)[x]]), ],
                             .variables = meta$variables[c(x, x - 1)],
                             .fun = function(a){sum(a[meta$freq])})
                 neworder <- order(tmp[meta$variables[x - 1]])
                 tmp <- cumsum(tmp$V1)[neworder]
                 return(c(rbind(tmp1, tmp1, NA, tmp, tmp, NA)))
                 
               })
  liney <- sapply(2:length(meta$variables),
               FUN = function(x){
                 rep(c(c(0, 0.1) + unique(recttop[[x - 1]]), NA, c(-0.1, 0) + unique(rectbottom[[x]]), NA),
                 length(levels(meta$y[, meta$variables[x - 1]])) *  length(levels(meta$y[, meta$variables[x]])))})

  if(!meta$horizontal){
    tmp <- recttop
    recttop <- right
    right <- tmp

    tmp <- rectbottom
    rectbottom <- left
    left <- tmp
    
    tmp <- linex
    linex <- liney
    liney <- tmp

  }
  return(list(rectleft = unlist(left), rectright = unlist(right), rectbottom = unlist(rectbottom), 
              recttop = unlist(recttop), liney = unlist(liney), linex = unlist(linex), 
              ncat = sum(sapply(meta$variables, FUN = function(y){length(unique(meta$y[,y]))})) ))
}

## describe data and number of ribbons and categories for each subset of the plot
.getparalines <- function(meta){

  subtables <- plyr:::loop_apply(length(meta$variables) - 1,
               f = function(x){
                 data.frame(ddply(meta$y, 
                       .variables = meta$variables[c(x, x + 1)], 
                       .fun = function(a){sum(a[meta$freq])}))})
  nlines <- unlist(lapply(subtables, FUN = nrow))
  ncats <- sapply(1:length(meta$variables), FUN = function(a){length(unique(meta$y[, a]))})
  return(list(subtables = subtables, nlines = nlines, ncats = ncats))

}

## local override for cranvas::identify_rect
.identify_rect <- function (meta, n = 0.01) {
  r = n * apply(meta$limits, 2, diff)/2
  p = meta$pos
  qrect(rbind(p - r, p + r))
}

## determine the plotting values for horizontal ribbons
.getHribbonVals <- function(meta){
  ncolors <- (meta$nlines - c(0, meta$nlines[1:(length(meta$nlines) - 1)])) / 2
  
  lineyends <- plyr:::loop_apply(length(meta$variables) - 1,
                                f = function(x){
                                  sub_linex <- meta$main_plotvalues$linex[(6 * (max(meta$nlines[x - 1], 0) + 1) - 
                                    5):(6 * meta$nlines[x])]
                                  sub_liney <- meta$main_plotvalues$liney[(6 * (max(meta$nlines[x - 1], 0) + 1) - 
                                    5):(6 * meta$nlines[x])]
                                  temp <- cbind(data.frame(matrix(sub_linex,
                                                                 nrow = length(sub_linex)/6,
                                                                 ncol = 6, byrow = TRUE)[, c(2,4)]),
                                               data.frame(matrix(sub_liney,
                                                                 nrow = length(sub_liney)/6,
                                                                 ncol = 6, byrow = TRUE)[, c(2,4)]))
                                  names(temp) <- c('start', 'end', 'startaxis', 'endaxis')
                                  temp <- cbind(temp, thick = temp$start - 
                                    c(0, temp$start[-nrow(temp)]), unique(meta$y[,c(x, x + 1)]))
                                  temp <- cbind(temp, color =  meta$pal[max(0, cumsum(ncolors)[x - 1]) + 
                                    as.numeric(temp[6][[1]])])
                                  return(temp)
                                })
  uplines <- llply(lineyends, function(x){x[x$end > x$start,]})
  downlines <- llply(lineyends, function(x){x[x$end < x$start, ]})
  straightlines <- llply(lineyends, function(x){x[x$end == x$start,]})
  sorteddownlines <- llply(downlines, function(x){cbind(x, slope = x$start - x$end)})
  sorteddownlines <- llply(sorteddownlines, function(x){x[order(x$slope, decreasing = TRUE),]})
  down_mult <- llply(sorteddownlines, function(x){diff(as.numeric(x[1, c('startaxis', 'endaxis')])) / x[1, 'slope']})
  sorteduplines <- llply(uplines, function(x){cbind(x, slope = x$end - x$start)})
  sorteduplines <- llply(sorteduplines, function(x){x[order(x$slope, decreasing = TRUE),]})
  up_mult <- llply(sorteduplines, function(x){diff(as.numeric(x[1, c('startaxis', 'endaxis')])) / x[1, 'slope']})
  sortedlines <- plyr:::loop_apply(length(sorteduplines), 
                                   f = function(x){list(sorteddownlines[[x]], sorteduplines[[x]])})
  mults <- plyr:::loop_apply(length(down_mult),
                             f = function(x){list(down_mult[[x]], up_mult[[x]])})
  return(list(lineyends = lineyends,  straightlines = straightlines, 
                sortedlines = sortedlines, mults = mults ))
}

## draw ribbons for horizontal orientation
.drawhorizontalribbons <- function( meta, painter){
  for(k in 1:(length(meta$variables) - 1)){
    for(i in 1:nrow(meta$r$straightlines[[k]])){
      sli <- meta$r$straightlines[[k]][i, ]
      qdrawPolygon(painter,
                   y = c(sli['startaxis'] - meta$width/2, 
                         sli['endaxis'] + meta$width/2, 
                         sli['endaxis'] + meta$width/2, 
                         sli['startaxis'] - meta$width/2),
                   x = sli[c('start', 'start', 'end', 'end')] - 
                         c(0, 0, rep(sli['thick'], 2)),
                   fill = alpha(as.character(sli['color'][[1]]), 0.5),
                   stroke = alpha(as.character(sli['color'][[1]]), 0.5))
    }
  
    for(j in 1:2){
      
      for(i in 1:nrow(meta$r$sortedlines[[k]][[j]])){
        srtlji <- meta$r$sortedlines[[k]][[j]][i,]
        newwidth <- srtlji['slope'] * meta$r$mults[[k]][[j]]
        qdrawPolygon(painter,
                     y = c(srtlji['startaxis'] - meta$width/2,
                           srtlji['startaxis'],
                           srtlji['endaxis'] - 
                             ifelse(newwidth < (srtlji['endaxis'] - srtlji['startaxis']),
                                    srtlji['endaxis'] - srtlji['startaxis'] - newwidth,
                                    0),
                           srtlji['endaxis'],
                           srtlji['endaxis'] + meta$width/2,
                           srtlji['endaxis'] + meta$width/2,
                           srtlji['endaxis'],
                           srtlji['endaxis'] - 
                             ifelse(newwidth < (srtlji['endaxis'] - srtlji['startaxis']),
                                    srtlji[ 'endaxis'] - srtlji['startaxis'] - newwidth,
                                    0),
                           srtlji['startaxis'],
                           srtlji['startaxis'] - meta$width/2),
                     x = c(srtlji[ c('start', 'start', 'end', 'end', 'end')],
                           srtlji[ c('end', 'end', 'end', 'start', 'start')][1,] - srtlji['thick'][1,]),
                     stroke = alpha(as.character(srtlji['color'][[1]]), 0.5),
                     fill = alpha(as.character(srtlji['color'][[1]]), 0.5))
      }
    }
  }
}

## determine the plotting values for vertical ribbons
.getVribbonVals <- function(meta){
  lineyends <- plyr:::loop_apply(length(meta$variables) - 1,
                        f = function(x){
                          sub_liney <- meta$main_plotvalues$liney[(6 * (max(meta$nlines[x - 1], 0) + 1) - 
                            5):(6 * meta$nlines[x])]
                          temp <- data.frame(matrix(sub_liney, 
                                                    nrow = length(sub_liney) / 6, 
                                                    ncol = 6, 
                                                    byrow = TRUE)[,c(1, 4)])
                          names(temp) <- c('start', 'end')
                          tmp <- unique(meta$y[, c(x, x + 1)])
                          tmp <- tmp[order(tmp[, meta$variables[x]]),]
                          temp <- cbind(temp,
                                        thick = temp$start - c(0, temp$start[-nrow(temp)]),
                                        tmp)
                          
                          sub_linex <- meta$main_plotvalues$linex[(6 * (max(meta$nlines[x - 1], 0) + 1) - 
                            5):(6 * meta$nlines[x])]
                          ncolors <- (meta$nlines - c(0, meta$nlines[1:(length(meta$nlines) - 1)])) / 2
                          temp <- cbind(temp, data.frame(matrix(sub_linex, 
                                                                nrow = length(sub_linex) / 6,
                                                                ncol = 6, byrow = TRUE)[, c(2, 4)]), 
                                    color = meta$pal[max(0, cumsum(ncolors)[x - 1]) + 
                                      as.numeric(temp[names(temp)[4]][[1]])])
                          names(temp)[6:7] <- c('startaxis', 'endaxis')
                          return(temp)
                        })
  uplines <- llply(lineyends,.fun = function(x){x[x$end > x$start,]})
  downlines <- llply(lineyends, .fun = function(x){x[x$end < x$start,]})
  straightlines <- llply(lineyends, .fun = function(x){x[x$end == x$start,]})
  sorteddownlines <- llply(downlines, .fun = function(x){cbind(x, slope = x$start - x$end)})
  sorteddownlines <- llply(sorteddownlines, .fun = function(x){x[order(x$slope, decreasing = TRUE),]})
  down_mult <- llply(sorteddownlines, .fun = function(x){diff(as.numeric(x[1, c('startaxis', 'endaxis')])) / x[1, 'slope']})
  sorteduplines <- llply(uplines, .fun = function(x){cbind(x, slope = x$end - x$start)})
  sorteduplines <- llply(sorteduplines, .fun = function(x){x[order(x$slope, decreasing = TRUE),]})
  up_mult <- llply(sorteduplines, .fun = function(x){diff(as.numeric(x[1, c('startaxis', 'endaxis')]))/x[1, 'slope']})
  mults <- plyr:::loop_apply(length(down_mult), f = function(x){list(down_mult[[x]], up_mult[[x]])})
  sortedlines <- plyr:::loop_apply(length(sorteduplines), f = function(x){list(sorteddownlines[[x]], sorteduplines[[x]])})
  return(list(lineyends = lineyends,  straightlines = straightlines, 
                sortedlines = sortedlines, mults = mults ))
}

## draw the labels
.drawlabels <- function(painter, meta){
  
  qdrawText(painter, 
            text = unlist(sapply(names(meta$variables), FUN = function(y){unique(meta$y[,y])})), 
            x = 0.5 * (meta$main_plotvalues$rectright - meta$main_plotvalues$rectleft) + 
              meta$main_plotvalues$rectleft, 
            y = 0.5 * (meta$main_plotvalues$recttop - meta$main_plotvalues$rectbottom) + 
              meta$main_plotvalues$rectbottom, color = "black", valign = "center", 
            cex = 1)
}

## draw the highlighted ribbons, vertical orientation
.drawVbrushribs <-function(selribs, meta, painter, mults, raxis){
  print(selribs)
  if(nrow(selribs) > 0){
    for(i in 1:nrow(selribs)){
      if(selribs[i, 'slope'] == 0){
        qdrawPolygon(painter,
                     x = c(selribs[i, 'startaxis'] - meta$width,
                           selribs[i, 'endaxis'] + meta$width,
                           selribs[i, 'endaxis'] + meta$width,
                           selribs[i, 'startaxis'] - meta$width),
                     y = selribs[i, c('start', 'start', 'end', 'end')] - 
                       c(0, 0, rep(selribs[i, 'thick'], 2)),
                     fill = alpha('yellow', .6),
                     stroke = 'yellow')
      } else {
        if(selribs[i, 'end'] > selribs[i, 'start']){
          newwidth <- selribs[i, 'slope'] * meta$r$mults[[raxis]][[2]]
        } else {
          newwidth <- selribs[i, 'slope'] * meta$r$mults[[raxis]][[1]]
        }
        
  
        qdrawPolygon(painter,
                     x = c(selribs[i, 'startaxis'] -  meta$width,
                           selribs[i, 'startaxis'],
                           selribs[i, 'endaxis'] - 
                             ifelse(newwidth < (selribs[i, 'endaxis'] - selribs[i, 'startaxis']),
                                    selribs[i, 'endaxis'] - selribs[i, 'startaxis'] - newwidth,
                                    0),
                           selribs[i, 'endaxis'],
                           selribs[i, 'endaxis'] + meta$width,
                           selribs[i, 'endaxis'] + meta$width,
                           selribs[i, 'endaxis'],
                           selribs[i, 'endaxis'] - 
                             ifelse(newwidth < (selribs[i, 'endaxis'] - selribs[i, 'startaxis']),
                                    selribs[i, 'endaxis'] - selribs[i, 'startaxis'] - newwidth,
                                    0),
                           selribs[i, 'startaxis'],
                           selribs[i, 'startaxis'] - meta$width),
                     y = c(selribs[i, c('start','start', 'end', 'end', 'end')],
                           selribs[i, c('end', 'end', 'end', 'start', 'start')] - 
                             selribs[i, 'thick']),
#                      fill = alpha('yellow', .25),
                     fill = alpha('yellow', .6),
                     stroke = 'yellow'
                     )
      }
    }
    ribvars <- (names(meta$paralines$subtables[[raxis]])[1:2])
    
    for(j in 1:length(ribvars)){
      axissum <- (ddply(meta$y, .variables = ribvars[j], .fun = function(a){
        sum(a[,meta$freq])}))
  
      selribsum <- ddply(selribs[,c('thick', ribvars[j])], ribvars[j], function(a){
        sum(a$thick)})
    
      for(i in 1:nrow(selribsum)){
       parwidth <- selribsum[i, meta$freq] / (axissum[axissum[, ribvars[j]] == 
         (selribsum[i, ribvars[j]]), meta$freq])

       qdrawRect(painter,
                 ytop = cumsum(axissum$V1)[which(axissum[, ribvars[j]] == 
                        selribsum[i, ribvars[j]])],
                 ybottom = c(0, cumsum(axissum$V1))[which(axissum[, ribvars[j]] == 
                           selribsum[i, ribvars[j]])],
                 xleft = (raxis + j - 1) - 0.5 * meta$width * parwidth,
                 xright = (raxis + j - 1) + 0.5 * meta$width *parwidth,
                 stroke = 'yellow',
                 fill = 'yellow')
      }
    }
    .drawextendedbrushribbons(painter, selribs, meta, raxis)
    if(meta$labels){
      .drawlabels(painter, meta)
    }
  }
}
.drawHbrushribs <- function(selribs, meta, painter, mults, raxis){
  if(nrow(selribs) > 0){
    for(i in 1:nrow(selribs)){
      if(selribs[i, 'slope'] == 0){
        qdrawPolygon(painter,
                     x = c(selribs[i, 'startaxis'] - meta$width,
                           selribs[i, 'endaxis'] + meta$width,
                           selribs[i, 'endaxis'] + meta$width,
                           selribs[i, 'startaxis'] - meta$width),
                     y = selribs[i, c('start', 'start', 'end', 'end')] - 
                       c(0, 0, rep(selribs[i, 'thick'], 2)),
                     fill = alpha('yellow', .6),
                     stroke = 'yellow')
      }
    }
    if(meta$labels){
      .drawlabels(painter, meta)
    }
  }
}
## extend the brushed ribbon to left and right axis
.drawextendedbrushribbons <- function(painter, selribs, meta, raxis){

  ## common drawing call to extend highlighted ribbons
  .common_extendaxis <- function(new_raxis, origdft, ribs){
    new_vars <- names(meta$paralines$subtables[[new_raxis]])[1:2]
    new_selribs <- ddply(origdft, new_vars, function(a){sum(a[, 
                                                              meta$freq])})
    
    
    temp <- meta$r$straightlines[[new_raxis]]
    temp1 <- meta$r$sortedlines[[new_raxis]][[1]]
    temp2 <- meta$r$sortedlines[[new_raxis]][[2]]
    for(i in 1:nrow(new_selribs)){
      
      newcoords <- temp[temp[, names(new_selribs)[1]] == new_selribs[i, 1] &
        temp[, names(new_selribs)[2]] == new_selribs[i, 2], 
                        c('start', 'end', 'startaxis', 'endaxis', 'color')]
      
      if(nrow(newcoords) > 0){
        
        qdrawPolygon(painter,
                     x = c(newcoords[,'startaxis'] -
                       meta$width, newcoords[, 'endaxis'] +
                       meta$width, newcoords[,'endaxis'] + meta$width,
                           newcoords[, 'startaxis'] - meta$width),
                     y = newcoords[, c('start', 'start', 'end', 'end')] - c(0, 0,
                                                                            rep(new_selribs$V1[i], 2)),
                     fill = alpha('yellow', .6),
                     stroke = 'yellow')}
      
      newcoords1 <- temp1[temp1[, names(new_selribs)[1]] == new_selribs[i, 1] &
        temp1[, names(new_selribs)[2]] == new_selribs[i, 2], 
                          c('start', 'end', 'startaxis', 'endaxis', 'color', 'slope')]
      newwidth1 <- newcoords1[, 'slope'] * meta$r$mults[[new_raxis]][[1]]
      
      if(nrow(newcoords1) > 0){
        for(j in 1:nrow(newcoords1)){
          qdrawPolygon(painter,
                       x = c(newcoords1[j, 'startaxis'] - meta$width,
                             newcoords1[j, 'startaxis'],
                             newcoords1[j, 'endaxis'] - 
                               ifelse(newwidth1[j] < (newcoords1[j, 'endaxis'] - newcoords1[j, 'startaxis']),
                                      newcoords1[j, 'endaxis'] - newcoords1[j, 'startaxis'] - newwidth1[j],
                                      0),
                             newcoords1[j, 'endaxis'],
                             newcoords1[j, 'endaxis'] + meta$width,
                             newcoords1[j, 'endaxis'] + meta$width,
                             newcoords1[j, 'endaxis'],
                             newcoords1[j, 'endaxis'] - 
                               ifelse(newwidth1[j] < (newcoords1[j, 'endaxis'] - newcoords1[j, 'startaxis']),
                                      newcoords1[j, 'endaxis'] - newcoords1[j, 'startaxis'] - newwidth1[j],
                                      0),
                             newcoords1[j, 'startaxis'],
                             newcoords1[j, 'startaxis'] - meta$width),
                       y = c(newcoords1[j, c('start', 'start', 'end', 'end', 'end')],
                             newcoords1[j, c('end', 'end', 'end', 'start', 'start')] - 
                               new_selribs$V1[i], 2),
                       stroke = 'yellow',
                       fill = alpha('yellow', .6))
        }
        
      }
      newcoords2 <- temp2[temp2[, names(new_selribs)[1]] == new_selribs[i, 1] &
        temp2[, names(new_selribs)[2]] == new_selribs[i, 2], 
                          c('start', 'end', 'startaxis', 'endaxis', 'color', 'slope')]
      newwidth2 <- newcoords2[, 'slope'] * meta$r$mults[[new_raxis]][[2]]
      
      if(nrow(newcoords2) > 0){
        for(j in 1:nrow(newcoords2)){
          qdrawPolygon(painter,
                       x = c(newcoords2[j, 'startaxis'] - meta$width,
                             newcoords2[j, 'startaxis'],
                             newcoords2[j, 'endaxis'] - 
                               ifelse(newwidth2[j] < (newcoords2[j, 'endaxis'] - newcoords2[j, 'startaxis']),
                                      newcoords2[j, 'endaxis'] - newcoords2[j, 'startaxis'] - newwidth2[j],
                                      0),
                             newcoords2[j, 'endaxis'],
                             newcoords2[j, 'endaxis'] + meta$width,
                             newcoords2[j, 'endaxis'] + meta$width,
                             newcoords2[j, 'endaxis'],
                             newcoords2[j, 'endaxis'] - 
                               ifelse(newwidth2[j] < (newcoords2[j, 'endaxis'] - newcoords2[j, 'startaxis']),
                                      newcoords2[j, 'endaxis'] - newcoords2[j, 'startaxis'] - newwidth2[j],
                                      0),
                             newcoords2[j, 'startaxis'],
                             newcoords2[j, 'startaxis'] - meta$width),
                       y = c(newcoords2[j, c('start', 'start', 'end', 'end', 'end')],
                             newcoords2[j, c('end', 'end', 'end', 'start', 'start')] - 
                               new_selribs$V1[i], 2),
                       stroke = 'yellow',
                       fill = alpha('yellow',.6))
        }
        
      }
      
    }
    if(ribs == 'left'){
      temp <- ddply(new_selribs, names(new_selribs)[1], function(x){sum(x$V1)})
      newbars <- cbind(ddply(new_selribs, names(new_selribs)[1], function(x){sum(x$V1)}), 
                     ttl = ddply(meta$y, names(new_selribs)[1], function(x){sum(x[, meta$freq])})$V1)
      qdrawRect(painter,
                ytop = cumsum(newbars$ttl),
                ybottom = c(0, cumsum(newbars$ttl)[-nrow(newbars)]),
                
                xleft = new_raxis -  0.5 * meta$width * newbars$V1 / newbars$ttl,
                xright = new_raxis +  0.5 * meta$width * newbars$V1 / newbars$ttl,
                fill = 'yellow')
    } else {

      newbars <- cbind(ddply(new_selribs, names(new_selribs)[2], function(x){sum(x$V1)}),
                       ttl = ddply(meta$y, names(new_selribs)[2], function(x){sum(x[, meta$freq])})$V1)
      qdrawRect(painter,
                ytop = cumsum(newbars$ttl),
                ybottom = c(0, cumsum(newbars$ttl)[-nrow(newbars)]),
                
                xleft = 1 + new_raxis -  0.5 * meta$width * newbars$V1 / newbars$ttl,
                xright = 1 + new_raxis +  0.5 * meta$width * newbars$V1 / newbars$ttl,
                fill = 'yellow')
    }
    
   
  }
  
  ## figure out nobs for cat combos on raxis < & > current
  range_raxis <- c(1:length(meta$paralines$subtables))
  
  ## recursive function: look at left raxis
  lookleft_raxis <- function(cur_raxis, range_raxis, origdft){
    if(cur_raxis <= min(range_raxis)){
      return(NULL)
    }else{
      new_raxis <- cur_raxis - 1
      .common_extendaxis(new_raxis, origdft, ribs = 'left')
      lookleft_raxis(new_raxis, range_raxis, origdft)
    }
  }
  lookright_raxis <- function(cur_raxis, range_raxis, origdft){
    if(cur_raxis >= max(range_raxis)){
      return(NULL)
    } else {
      new_raxis <- cur_raxis + 1
      .common_extendaxis(new_raxis, origdft, ribs = 'right')
      lookright_raxis(new_raxis, range_raxis, origdft)
    }
  }
  origdft <- NULL
  for(i in 1:nrow(selribs)){
    origdft <- rbind(origdft, meta$y[meta$y[,names(selribs)[4]] == 
      selribs[i, 4] & meta$y[,names(selribs)[5]] == selribs[i, 5],] )
  }
  
  lookleft_raxis(cur_raxis = raxis, range_raxis, origdft = origdft)
  lookright_raxis(cur_raxis = raxis, range_raxis, origdft = origdft)
}


## draw ribbons in vertical orientation 
.drawverticalribbons <- function(meta, painter){
    ## guide lines
    for(k in 1:(length(meta$variables) - 1)){  
    for(i in 1:nrow(meta$r$straightlines[[k]])){
      qdrawPolygon(painter,
                   x = c(meta$r$straightlines[[k]][i, 'startaxis'] - 
                     meta$width/2, meta$r$straightlines[[k]][i, 'endaxis'] + 
                     meta$width/2, meta$r$straightlines[[k]][i, 'endaxis'] + meta$width/2, 
                         meta$r$straightlines[[k]][i, 'startaxis'] - meta$width/2),
                   y = meta$r$straightlines[[k]][i, c('start', 'start', 'end', 'end')] - c(0, 0, 
                             rep(meta$r$straightlines[[k]][i, 'thick'], 2)),
                   fill = alpha(as.character(meta$r$straightlines[[k]][i, 'color']), 0.5),
                   stroke = alpha(as.character(meta$r$straightlines[[k]][i, 'color']), 0.5))
    }
    for(j in 1:2){
      for(i in 1:nrow(meta$r$sortedlines[[k]][[j]])){
        newwidth <- meta$r$sortedlines[[k]][[j]][i, 'slope'] * meta$r$mults[[k]][[j]]
        qdrawPolygon(painter,
                     x = c(meta$r$sortedlines[[k]][[j]][i, 'startaxis'] - meta$width/2,
                           meta$r$sortedlines[[k]][[j]][i, 'startaxis'],
                           meta$r$sortedlines[[k]][[j]][i, 'endaxis'] - 
                             ifelse(newwidth < (meta$r$sortedlines[[k]][[j]][i, 'endaxis'] - meta$r$sortedlines[[k]][[j]][i, 'startaxis']),
                                    meta$r$sortedlines[[k]][[j]][i, 'endaxis'] - meta$r$sortedlines[[k]][[j]][i, 'startaxis'] - newwidth,
                                    0),
                           meta$r$sortedlines[[k]][[j]][i, 'endaxis'],
                           meta$r$sortedlines[[k]][[j]][i, 'endaxis'] + meta$width/2,
                           meta$r$sortedlines[[k]][[j]][i, 'endaxis'] + meta$width/2,
                           meta$r$sortedlines[[k]][[j]][i, 'endaxis'],
                           meta$r$sortedlines[[k]][[j]][i, 'endaxis'] - 
                             ifelse(newwidth < (meta$r$sortedlines[[k]][[j]][i, 'endaxis'] - meta$r$sortedlines[[k]][[j]][i, 'startaxis']),
                                    meta$r$sortedlines[[k]][[j]][i, 'endaxis'] - meta$r$sortedlines[[k]][[j]][i, 'startaxis'] - newwidth,
                                    0),
                           meta$r$sortedlines[[k]][[j]][i, 'startaxis'],
                           meta$r$sortedlines[[k]][[j]][i, 'startaxis'] - meta$width/2),
                     y = c(meta$r$sortedlines[[k]][[j]][i, c('start', 'start', 'end', 'end', 'end')],
                           meta$r$sortedlines[[k]][[j]][i, c('end', 'end', 'end', 'start', 'start')] - 
                             meta$r$sortedlines[[k]][[j]][i, 'thick']),
                     stroke = alpha(as.character(meta$r$sortedlines[[k]][[j]][i, 'color']), 0.5),
                     fill = alpha(as.character(meta$r$sortedlines[[k]][[j]][i, 'color']), 0.5))
        
      }}

  }
  
}

#' @param x the data in qdata or data.frame format. Either long form data or frequency table
#' @param variables the name of variables to display
#' @param freq name of frequency data column, if data is frequency table
#' @param xat location of x gridlines
#' @param yat location of y gridlines
#' @param width size of bars [0, 1]
#' @param pal color palatte
#' @param main plot title
#' @param whether to display identify layer
#' @param labels whether to display labels on bars
#' @param horizontal whether to display in horizontal orientation
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
    
    return(paste(sapply(X = capture.output(print(sumfreq, quote = FALSE, row.names = FALSE)), 
                        FUN = str_trim), collapse = "\n"))
  }
  
  meta <- .instantiateHammocks(environment())
  meta$main_plotvalues <- .getmainplotting(meta)
  meta$paralines <- .getparalines(meta)

  if(meta$horizontal){
    meta$r <- .getHribbonVals(meta)
  } else {
    meta$r <- .getVribbonVals(meta)
  }
  .findhitsdata <- function(meta) {
    axishits <- meta$hits[meta$hits <= sum(meta$paralines$ncats)]
    ribbonhits <- meta$hits[meta$hits > sum(meta$paralines$ncats)]
    h <- logical(nrow(x))
   

  # figure out which variables the lines are between   
  # generate a list of the no. of lines between each variable
  # check subset values part against this list
    if(length(ribbonhits) > 0){
      for(i in ribbonhits){
        which_raxis <- max(which((i - sum(meta$paralines$ncats)) >  cumsum(meta$paralines$nlines)), 
                           0, na.rm = TRUE) + 1
        
  
        sel_ribbon <- rbind(meta$r$straightlines[[which_raxis]][, meta$variables[which_raxis:(which_raxis+1)]],
          meta$r$sortedlines[[which_raxis]][[1]][, meta$variables[which_raxis:(which_raxis+1)]],
          meta$r$sortedlines[[which_raxis]][[2]][, meta$variables[which_raxis:(which_raxis+1)]])[i - 
          c(0, cumsum(meta$paralines$nlines))[which_raxis] - sum(meta$paralines$ncats),]
         h <- h | (x[,names(meta$y)[which_raxis]] == sel_ribbon[, meta$variables[which_raxis]] &
           x[,names(meta$y)[which_raxis + 1]] == sel_ribbon[, meta$variables[which_raxis + 1]])
     }
    }  

  
      
  # case 2: category
  # subset the hits values for bar cat

    if (length(axishits) > 0) {
       for(i in axishits){
          whichvar <- max(which(axishits > cumsum(meta$paralines$ncats)), 0) + 1
  
          h <- h | x[,names(meta$y)[whichvar]] %in% 
             levels(meta$y[, whichvar])[i - 
             c(0, cumsum(meta$paralines$ncats))[whichvar]]
        }
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
    meta$hits <- layer$locate(rect) + 1
#     if (length(meta$hits)) {
      hits <- .findhitsdata(meta)
#     } 
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

    sub <- as.data.frame(x[selected(x), ][meta$variables])
    
    ## draw selected lines when brushing
    # determine if the selected values represent axis selection
    if (nrow(sub) > 0) {
      which_axis <- NULL
      which_raxis <- NULL
      if(meta$horizontal){
        axisrange <- round(meta$pos[2], 0) + c(-1, 1) * (meta$width/2)
##TODO
        if(meta$pos[2] > min(axisrange) & (meta$pos[2] - abs(meta$brush.size[2])) <
          max(axisrange)){
          which_axis <- round(meta$pos[2], 0)
        }
      } else {
         axisrange <- c(-1,1) * (meta$width/2) + round(meta$pos[1], 0)
         if(meta$pos[1] > min(axisrange) & (meta$pos[1] - abs(meta$brush.size[1])) < 
           max(axisrange)){
            which_axis <- round(meta$pos[1], 0)
         }
         
         temp <- matrix((c(-1,1) * (meta$width/2)) + rep(meta$xat, each = 2), ncol = 2, 
                      nrow = length(meta$variables), byrow = TRUE)
         
         if((is.null(which_axis)) ||
           (meta$pos[1] < (length(meta$variables) - meta$width/2) &&
           meta$pos[1] > 1 &&
           which_axis != min(which(meta$pos[1] < temp[, 2])))){
           which_raxis <- min(which(meta$pos[1] < temp[,2]))
         }
         if(is.null(which_axis) || 
           ((meta$pos[1] - abs(meta$brush.size[1])) > 
              (1 + (meta$width/2))  && 
           (meta$pos[1] - abs(meta$brush.size[1])) <
              (length(meta$variables) - (meta$width/2)) &&
           min(which((meta$pos[1] - abs(meta$brush.size[1])) < 
           temp[, 2])) == which_axis )){
           which_raxis <- c(which_raxis, min(which((meta$pos[1] - abs(meta$brush.size[1])) < 
             temp[, 2])))
          }
         which_raxis <- unique(which_raxis) - 1
      }
     
      ##todo!!!
      if(length(which_axis)){
        summaryCats <- sapply(meta$variables, 
                              FUN = function(a){summary(x[,a])})
        summarySubs <- sapply(meta$variables,
                              FUN = function(a){summary(sub[,a])})
        catid <- which(summaryCats[[which_axis]] == summarySubs[[which_axis]])
        which_cats <- c((c(0, cumsum(sapply(summaryCats, FUN = length)))[which_axis] + 1):
          cumsum(sapply(summaryCats, FUN = length))[which_axis])

        qdrawRect(painter,
                  xleft = meta$main_plotvalues$rectleft[which_cats][catid],
                  xright = meta$main_plotvalues$rectright[which_cats][catid],
                  ytop = meta$main_plotvalues$recttop[which_cats][catid],
                  ybottom = meta$main_plotvalues$rectbottom[which_cats][catid],
                  stroke = "yellow",
                  fill = 'yellow')
 
      }
      if(length(which_raxis)){
 
        rhits <- meta$hits[meta$hits > sum(meta$paralines$ncat)]

        for(i in which_raxis){
          varnames <- names(meta$paralines$subtables[[i]])[1:2]
          parorders <- rbind(cbind(meta$r$straightlines[[i]], slope = 0), 
                      meta$r$sortedlines[[i]][[1]],
                meta$r$sortedlines[[i]][[2]])
          rownames(parorders) <- 1:nrow(parorders)

          selribs <- parorders[rhits[rhits > c(0, cumsum(meta$paralines$nlines))[i] + 
            sum(meta$paralines$ncat) & rhits <= cumsum(meta$paralines$nlines)[i] + 
            sum(meta$paralines$ncat)] - (c(0, cumsum(meta$paralines$nlines))[i] + 
            sum(meta$paralines$ncat) ),]
          if(meta$horizontal){
            .drawHbrushribs(selribs, meta, painter, raxis = i)
          } else {
            .drawVbrushribs(selribs, meta, painter, raxis = i)
          }

          
        }
        
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

    ## draw the axis bars
    qdrawRect(painter, 
              xleft = meta$main_plotvalues$rectleft, 
              xright = meta$main_plotvalues$rectright, 
              ybottom = meta$main_plotvalues$rectbottom, 
              ytop = meta$main_plotvalues$recttop, 
              stroke = "grey60", 
              fill = as.character(meta$pal[1:meta$main_plotvalues$ncat]))
    
    
    
    ## draw the ribbons
    if(meta$horizontal){
      .drawhorizontalribbons(meta, painter)
    } else {
      .drawverticalribbons(meta, painter)
    }
    if (labels) {
      .drawlabels(painter, meta)
           
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
# color_pal(qtitanic)<-.new_pal()(6)
## qhammock(x = qtitanic, variables = c('Class', 'Survived'))
qhammock(x = qtitanic, variables = c("Class", "Survived", "Age", "Sex"))#, 
    #    horizontal = TRUE)
  
  
  # temp <- ddply(data.frame(Titanic), c('Class', 'Survived'), .fun
# = function(x){sum(x$Freq)}) names(temp)[3] <- 'Freq' qhammock(x
# = qdata(temp), variables = c('Class', 'Survived'), freq =
# 'Freq') qhammock(x = qdata(temp), variables = c('Class',
# 'Survived'), freq = 'Freq', horizontal = TRUE) 
