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

  limits <- matrix(c(c(-1, 1) * diff(range(xat)) * 2 * param$width + range(xat), 
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
                           nlines = cumsum(unlist(sapply(1:length(param$variables), FUN = function(x){if(x > 1){
                             length(unique(y[,param$variables[x]])) * length(unique(y[,param$variables[x - 1]]))
                           }}))), 
                           active = TRUE))
}

.getmainplotting <- function(meta) {
  ## allocate the variables
  right <- vector(mode = 'list', length = length(meta$variables))
  left <- vector(mode = 'list', length = length(meta$variables))
  rectbottom <- vector(mode = 'list', length = length(meta$variables))
  recttop <- vector(mode = 'list', length = length(meta$variables))
  linex <- vector(mode = 'list', length = length(meta$variables) - 1)
  liney <- vector(mode = 'list', length = length(meta$variables) - 1)
 
  if (meta$horizontal) {
    selectlabels <- meta$ylabels
    selectaxis <- meta$yat
  } else {
    selectlabels <- meta$xlabels
    selectaxis <- meta$xat
  }
     ## populate the variables

  for(i in 1:length(meta$variables)){
    right[[i]] <- cumsum(ddply(meta$y,
                               .variables = names(selectlabels)[i],
                               .fun = function(x){
                                 sum(x[meta$freq])})$V1)
    left[[i]] <- c(0, right[[i]][-length(right[[i]])])
    rectbottom[[i]] <- rep(selectaxis[i] - 0.5 * meta$width, 
                           length(unique(meta$y[,meta$variables[i]])))
    recttop[[i]] <- rep(selectaxis[i] + 0.5 * meta$width, 
                        length(unique(meta$y[, meta$variables[i]])))

    if(i > 1){
      tmp1 <- cumsum(ddply(meta$y[order(meta$y[names(selectlabels)[i - 1]]),], 
                         .variables = meta$variables[c(i - 1, i)], 
                         .fun = function(a){sum(a[meta$freq])})$V1)
#       tmp1 <- c(0, tmp1[-length(tmp1)])  + 0.5 * (tmp1 - c(0, tmp1[-length(tmp1)]))
      tmp2 <- ddply(meta$y[order(meta$y[names(selectlabels)[i]]),], 
                           .variables = meta$variables[c(i, i - 1)], 
                           .fun = function(a){sum(a[meta$freq])})
      neworder <- order(tmp2[meta$variables[i - 1]])
      tmp2 <- cumsum(tmp2$V1)
#       tmp2 <- c(0, tmp2[-length(tmp2)])  + 0.5 * (tmp2 - c(0, tmp2[-length(tmp2)]))
      tmp2 <- tmp2[neworder]
      linex[[i - 1]] <- c(rbind(tmp1,tmp1, NA, tmp2, tmp2, NA))
      liney[[i - 1]] <- rep(c(c(0, .1) + unique(recttop[[i - 1]]), NA, c(-.1, 0) + unique(rectbottom[[i]]), NA),
                            length(unique(meta$y[,meta$variables[i-1]])) * 
                              length(unique(meta$y[,meta$variables[i]])))


    }
    
  }
   
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
  linex <- unlist(linex)
  liney <- unlist(liney)
  
  rectleft <- unlist(left)
  rectright <- unlist(right)
  rectbottom <- unlist(rectbottom)
  recttop <- unlist(recttop)

  return(list(rectleft = rectleft, rectright = rectright, rectbottom = rectbottom, 
              recttop = recttop, liney = liney, linex = linex, 
              ncat = sum(sapply(meta$variables, FUN = function(y){length(unique(meta$y[,y]))})) ))
}
.getparalines <- function(meta, main_plotvalues, sub){
  if(meta$horizontal){
    
    
  } else{
    # create a list of the subtables
    subtables <- vector(mode = 'list', length = length(meta$variables) - 1)
    for(i in 1:length(subtables)){
        subtables[[i]] <- ddply(meta$y, .variables = meta$variables[c(i, i + 1)], 
                                .fun = function(a){sum(a[meta$freq])})
 
    }

    closest <- as.integer(meta$pos[1])
    if(meta$pos[1] < (closest + 0.5 * meta$width) & meta$pos[1] > (closest - 0.5 * meta$width)){
      seps <- cumsum(ddply(meta$y, .variables = meta$variables[closest], .fun = function(a){sum(a[meta$freq])})$V1)

    }

    
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
  # subset the hits values for lines only
  part <- hits[which(hits <= max(meta$nlines))]
  # figure out which variables the lines are between   
  # generate a list of the no. of lines between each variable
  # check subset values part against this list
  if(length(part)){

    for(i in part){
      nvar <- min(which(i <= meta$nlines))

      newpart <- ddply(meta$y, .variables = meta$variables[c(nvar, nvar + 1)],
                  .fun = function(x){sum(x[meta$freq])})
      newpart <- newpart[order(newpart[,meta$variables[nvar]]),]
 
      if(nvar > 1){
        newpart <- newpart[(i - meta$nlines[nvar - 1]),]
      } else {
        newpart <- newpart[i,]
      }
      if (nrow(newpart) > 0) {     
         for(j in 1:nrow(newpart)){
           
           h <- h | x[, meta$variables[nvar]] == newpart[j, meta$variables[nvar]] &
                         x[, meta$variables[nvar + 1]] == newpart[j, meta$variables[nvar + 1]]
           
           
         }
        
      }
    }
    

  }  

  
      
  # case 2: category
  # subset the hits values for bar cat
  part <- hits[which(hits - max(meta$nlines)  > 1)]
  if (length(part) > 0) {
    ncat <- cumsum(sapply(1:length(meta$variables), FUN = function(x){length(unique(meta$y[,meta$variables[x]]))}))
    for(i in part){
      curcat <- min(which((i - 1 - max(meta$nlines)) <= ncat))  
      if(curcat > 1){
        newpart <- unique(meta$y[, meta$variables[curcat]])[i - 1 - max(meta$nlines) - ncat[curcat - 1]]
      } else {
        newpart <- unique(meta$y[, meta$variables[curcat]])[i - 1 - max(meta$nlines)]
      }
      h <- h | x[, meta$variables[curcat]] == newpart
    }

    
  }
    if(!is.null(ncol(h))){

      h <- Reduce(`|`, as.data.frame(h))
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
    sub <- as.data.frame(x[selected(x), ][meta$variables])
#     sub <- unique(as.data.frame(sub))

    
    ## draw selected lines when brushing
    lineid <- NULL
    if (nrow(sub) > 0) {
      paralines <- .getparalines(meta, main_plotvalues, sub)
     
      
#       
#       
#       lineid <- sapply(1:nrow(sub), FUN = function(x){
#         which(meta$y[order(meta$y[,meta$variables[2]]), meta$variables[1]] == sub[x, meta$variables[1]] &
#               meta$y[order(meta$y[,meta$variables[2]]), meta$variables[2]] == sub[x, meta$variables[2]])})
    }
#     lineid <- unique(lineid)
#     if (length(lineid) > 1) {
#       thickness <- meta$y[order(meta$y[, meta$variables[2]]), 
#                           meta$freq][lineid]
#       newy <- c(rbind(sapply(1:length(lineid), FUN = function(a) {
#         rep(c(main_plotvalues$liney[3 * lineid[a] - 2], 
#               main_plotvalues$liney[3 * lineid[a] - 1]), each = 2) + 
#                 rep(c(-0.5, 0.5) * ifelse(!meta$horizontal, thickness[a], 
#                                           0), 2)})[c(1, 2, 4, 3), ], NA))
#       newx <- c(rbind(sapply(1:length(lineid), FUN = function(a) {
#         rep(c(main_plotvalues$linex[3 * lineid[a] - 2], 
#               main_plotvalues$linex[3 * lineid[a] - 1]), each = 2) + 
#                 rep(c(-0.5, 0.5) * ifelse(meta$horizontal, thickness[a], 
#                                           0), 2)})[c(1, 2, 4, 3), ], NA))
#       
#       qdrawPolygon(painter, y = newy, x = newx, fill = (attr(x, 
#                                                              "Brush")$color), stroke = NA)
#       qdrawLine(painter, y = c(rbind(main_plotvalues$liney[3 * 
#         lineid - 2], main_plotvalues$liney[3 * lineid - 1], 
#                                      NA)), x = c(rbind(main_plotvalues$linex[3 * lineid - 
#                                        2], main_plotvalues$linex[3 * lineid - 1], NA)), stroke = "grey60")
#       if (meta$horizontal) {
#         qdrawRect(painter, ytop = min(newy, na.rm = T), ybottom = min(newy, 
#                                                                       na.rm = T) - meta$width, xleft = newx[5 * (1:length(lineid)) - 
#                                                                         4], xright = newx[5 * (1:length(lineid)) - 3], fill = attr(x, 
#                                                                                                                                    "Brush")$color, stroke = NA)
#         qdrawRect(painter, ytop = max(newy, na.rm = T), ybottom = max(newy, 
#                                                                       na.rm = T) + meta$width, xleft = newx[5 * (1:length(lineid)) - 
#                                                                         1], xright = newx[5 * (1:length(lineid)) - 2], fill = attr(x, 
#                                                                                                                                    "Brush")$color, stroke = NA)
#       } else {
#         
#         qdrawRect(painter, ytop = newy[5 * (1:length(lineid)) - 
#           4], ybottom = newy[5 * (1:length(lineid)) - 3], xright = min(newx, 
#                                                                        na.rm = T), xleft = min(newx, na.rm = T) - meta$width, 
#                   fill = attr(x, "Brush")$color, stroke = NA)
#         qdrawRect(painter, ytop = newy[5 * (1:length(lineid)) - 
#           1], ybottom = newy[5 * (1:length(lineid)) - 2], xleft = max(newx, 
#                                                                       na.rm = T), xright = max(newx, na.rm = T) + meta$width, 
#                   fill = attr(x, "Brush")$color, stroke = NA)
#       }
#     } else if (length(lineid) == 1) {
#       thickness <- meta$y[order(meta$y[, meta$variables[2]]), 
#                           meta$freq][lineid]
#       newy <- c(rep(c(main_plotvalues$liney[3 * lineid - 2], 
#                       main_plotvalues$liney[3 * lineid - 1]), each = 2) + 
#                         rep(c(-0.5, 0.5) * ifelse(!meta$horizontal, thickness, 
#                                                   0), 2))[c(1, 2, 4, 3)]
#       newx <- c(rep(c(main_plotvalues$linex[3 * lineid - 2], 
#                       main_plotvalues$linex[3 * lineid - 1]), each = 2) + 
#                         rep(c(-0.5, 0.5) * ifelse(meta$horizontal, thickness, 
#                                                   0), 2))[c(1, 2, 4, 3)]
#       
#       qdrawPolygon(painter, y = newy, x = newx, fill = (attr(x, 
#                                                              "Brush")$color), stroke = NA)
#       
#       qdrawLine(painter, y = c(main_plotvalues$liney[3 * lineid - 
#         2], main_plotvalues$liney[3 * lineid - 1]), x = 
#         c(main_plotvalues$linex[3 * lineid - 2], 
#           main_plotvalues$linex[3 * lineid - 1]), stroke = "grey60")
#       if (meta$horizontal) {
#         qdrawRect(painter, ytop = min(newy), ybottom = min(newy) - 
#           meta$width, xleft = newx[1], xright = newx[2], fill = attr(x, 
#                                                                      "Brush")$color, stroke = NA)
#         qdrawRect(painter, ybottom = max(newy), ytop = max(newy) + 
#           meta$width, xleft = newx[4], xright = newx[3], fill = attr(x, 
#                                                                      "Brush")$color, stroke = NA)
#         
#       } else {
#         qdrawRect(painter, ytop = newy[1], ybottom = newy[2], 
#                   xleft = min(newx) - meta$width, xright = min(newx), 
#                   fill = attr(x, "Brush")$color, stroke = NA)
#         qdrawRect(painter, ytop = newy[4], ybottom = newy[3], 
#                   xleft = max(newx), xright = max(newx) + meta$width, 
#                   fill = attr(x, "Brush")$color, stroke = NA)
#       }
#     }
#     
#     
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
 
    lineyends <- data.frame(matrix(main_plotvalues$liney, nrow = length(main_plotvalues$liney)/6, 
                                   ncol = 6, byrow = TRUE)[,c(1,4)])
    names(lineyends) <- c('start', 'end')
    lineyends <- cbind(lineyends, thick = lineyends$start - c(0, lineyends$start[-nrow(lineyends)]))
    lineyends <- cbind(lineyends, data.frame(matrix(main_plotvalues$linex, nrow = length(main_plotvalues$linex)/6,
                                   ncol = 6, byrow = TRUE)[,c(2, 4)]))
    names(lineyends)[4:5] <- c('startaxis', 'endaxis')

    uplines <- lineyends[lineyends$end > lineyends$start,]
    downlines <- lineyends[lineyends$end < lineyends$start,]
    straightlines <- lineyends[lineyends$end == lineyends$start,]


    
        qdrawLine(painter, 
                  x = main_plotvalues$linex, 
                  y = main_plotvalues$liney, 
                  stroke = "grey60")
    qdrawRect(painter, 
              xleft = main_plotvalues$rectleft, 
              xright = main_plotvalues$rectright, 
              ybottom = main_plotvalues$rectbottom, 
              ytop = main_plotvalues$recttop, 
              stroke = "grey60", 
              fill = meta$pal[1:main_plotvalues$ncat])
    
    if (labels) {
   
      qdrawText(painter, 
                text = unlist(sapply(names(meta$variables), FUN = function(y){unique(meta$y[,y])})), 
                x = 0.5 * (main_plotvalues$rectright - main_plotvalues$rectleft) + 
                  main_plotvalues$rectleft, 
                y = 0.5 * (main_plotvalues$recttop - main_plotvalues$rectbottom) + 
                  main_plotvalues$rectbottom, color = "black", valign = "center", 
                cex = 1)
      
    }
    
    ## guide lines

    

    qdrawLine(painter,
              y = c(downlines$start[nrow(downlines)], downlines$end[1], NA,
                    c(sapply(1:nrow(downlines), FUN = function(a){c(rep(downlines$start[a], 2), 
                                                                    rep(downlines$end[a], 2), NA)}))),
              x = c(downlines$startaxis[nrow(downlines)] + 0.6 *((sum(meta$y[meta$freq]) - 
                    downlines$start[nrow(downlines)])/sum(meta$y[meta$freq])), 
                    downlines$endaxis[1] - 0.6 * (downlines$end[1]/sum(meta$y[meta$freq])), NA,
                    c(sapply(1:nrow(downlines), FUN = function(a){c(downlines$startaxis[a], 
                      downlines$startaxis[a] + 0.6 *((sum(meta$y[meta$freq]) - downlines$start[a])/sum(meta$y[meta$freq])), 
                      downlines$endaxis[a] - 0.6 * (downlines$end[a]/sum(meta$y[meta$freq])), 
                      downlines$endaxis[a], NA)}))),
              stroke = 'red'
              )
    
print(matrix(c(downlines$start[nrow(downlines)], downlines$end[1], NA,
        c(sapply(1:nrow(downlines), FUN = function(a){c(rep(downlines$start[a], 2), 
                                                        rep(downlines$end[a], 2), NA)}))), ncol = 3, byrow = TRUE))
print(c(downlines$startaxis[nrow(downlines)] + 0.6 *((sum(meta$y[meta$freq]) - 
  downlines$start[nrow(downlines)])/sum(meta$y[meta$freq])), 
        downlines$endaxis[1] - 0.6 * (downlines$end[1]/sum(meta$y[meta$freq])), NA,
        c(sapply(1:nrow(downlines), FUN = function(a){c(downlines$startaxis[a], 
                                                        downlines$startaxis[a] + 0.6 *((sum(meta$y[meta$freq]) - downlines$start[a])/sum(meta$y[meta$freq])), 
                                                        downlines$endaxis[a] - 0.6 * (downlines$end[a]/sum(meta$y[meta$freq])), 
                                                        downlines$endaxis[a], NA)}))))
    
    qdrawLine(painter,
              y = c(uplines$start[1], uplines$end[nrow(uplines)], NA, 
                    c(sapply(1:nrow(uplines), FUN = function(a){c(rep(uplines$start[a], 2),   
                    rep(uplines$end[a], 2), NA)}))),
              x = c(uplines$start[1]/sum(meta$y[meta$freq]) * 0.6 + uplines$startaxis[1], 
                    uplines$endaxis[nrow(uplines)] - 0.6 * (sum(meta$y[meta$freq]) - 
                    uplines$end[nrow(uplines)])/sum(meta$y[meta$freq]), NA, 
                    c(sapply(1:nrow(uplines), FUN = function(a){c(uplines$startaxis[a], 
                    uplines$start[a]/sum(meta$y[meta$freq]) * 0.6 + uplines$startaxis[a],  
                    uplines$endaxis[a] - 0.6 * (sum(meta$y[meta$freq]) - uplines$end[a])/sum(meta$y[meta$freq]), 
                    uplines$endaxis[a], NA)}))),
              stroke = 'black'
              )
    
    qdrawLine(painter,
              y = c(sapply(1:nrow(straightlines), FUN = function(a){
                c(rep(straightlines$start[a], 2), NA)})),
              x = c(sapply(1:nrow(straightlines), FUN = function(a){
                c(straightlines$startaxis[a], straightlines$endaxis[a], NA) })),
              stroke = 'blue'
            )
    
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
qhammock(x = qtitanic, variables = c("Class", "Survived", "Age", "Sex"))#, 
#          horizontal = TRUE)
  
  
  # temp <- ddply(data.frame(Titanic), c('Class', 'Survived'), .fun
# = function(x){sum(x$Freq)}) names(temp)[3] <- 'Freq' qhammock(x
# = qdata(temp), variables = c('Class', 'Survived'), freq =
# 'Freq') qhammock(x = qdata(temp), variables = c('Class',
# 'Survived'), freq = 'Freq', horizontal = TRUE) 
