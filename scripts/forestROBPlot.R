####################### forest ROB plot #######################
drawCI <- function(LL, OR, UL, size){ 
  ## size: Effect size(OR) point value 
  ## draw CI for each effect size 
  if (is.na(UL)){
    grid.rect(x = unit(OR, "native"),
              width = unit(size, "snpc"), height = unit(size, "snpc"),
              gp = gpar(fill = "blue"))
  }else{
    grid.rect(x = unit(OR, "native"), 
              width = unit(size, "snpc"), height = unit(size, "snpc"),
              gp = gpar(fill = "black"))
    
    if (!is.na(UL)) {
      if (convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 1) {
        ## truncated case 
        grid.lines(x = unit(c(LL,1), c("native","npc")), y = 0.5,
                   arrow = arrow(length = unit(0.05, "inches")))
      }else{
        lineCol <- if ((convertX(unit(OR, "native") + unit(0.5*size, "lines"),
                                 "native", valueOnly = TRUE) > UL) &&
                         (convertX(unit(OR, "native") - unit(0.5*size, "lines"),
                                   "native", valueOnly = TRUE) < LL))
          "white"
        else
          "black"
        grid.lines(x = unit(c(LL, UL), "native"), y = 0.5,
                   gp = gpar(col = lineCol))
      }
    } 
  }
}

draw.semicircle <- function(base.x, base.y, base.length,
                            height = base.length, side = 2, 
                            col = NULL, units = "native"){
  ## draw semicircle filled with given color: side <- 2 -> left side; side <- 4-> right side  
  radius <- base.length/2
  x <- radius*seq(-1,1,length = 40)
  #y<-height/radius*sqrt(radius^2-x^2)
  y <- sqrt(radius^2 - x^2)
  co <- as.integer(cos(pi*(3 - side)/2))
  so <- as.integer(sin(pi*(3 - side)/2))
  tx <- co*x - so*y 
  ty <- so*x + co*y
  base.y <- base.y + radius
  x <- base.x + tx
  y <- base.y + ty
  cx_y <- max(x,y)  
  grid.polygon(x,y,default.units = units,gp = gpar(fill = col))
}

assignColor <- function(s){
  ## assign color for each ROB case 
  s <- gsub("\\s+","",s)
  s <- col_ROB[s]
  return(s)
}

forest_ROB_plot <- function(M_first, M_second,labeltext, 
                            mean, lower, upper,
                            is.summary, xlab, Nms, measure.method,
                            clip = c(-20, 100), graphwidth = unit(2, "inches"),
                            xlog = FALSE,
                            align = "l", fontsize = 11,  numROB = 6, 
                            r = 0.9,  cx = 0.8, cy = 0,
                            col = list(box = "black", lines = "gray", summary = "black",
                                        zero = "lightgray", mirror = "lightblue",
                                        text = "black", axes = "black", background = NA) 
                            ){
  ## Ref: forestplot & ROB plot  ps: forest plot is referred to forestplot function in rmeta package
  ## Args: 
  ##      labeltext: LHS table in matrix form, first row is summary,
  ##      mean: vector, effect size  
  ##      lower: vector, lower bound 
  ##      upper: vector, upper bound, for OR, if mean <- 1, upper <- lower <- NA 
  ##      is.summary: vector, indicates which row in labeltext is summary or not 
  ##      xlab: string, xlabel for x-axis, 
  ##      Nms: vector of strings, Nms for risk of bias plot
  ##      measure.method: string
  ##      numROB: number of columns of ROB plot
  ##      r: radius for circle in ROB plot;  cx,cy: center for circle in ROB plot 

  
  #### plot
  plot.new()
  widthcolumn <- !apply(is.na(labeltext), 1, any)
  
  nc <- NCOL(labeltext)
  labels <- vector("list", nc)
  
  align <- rep(align,length = nc)
  nr <- NROW(labeltext)
  is.summary <- rep(is.summary,length = nr)
  
  ## LHS table 
  for (j in 1:nc){
    labels[[j]] <- vector("list", nr)
    for (i in 1:nr){
      if (is.na(labeltext[i,j]))
        next
      x <- switch(align[j], l = 0, r = 1, c = 0.5)
      just <- switch(align[j], l = 'left', r = 'right', c = 'center')
      
      labels[[j]][[i]] <- textGrob(labeltext[i,j], x = x,just = just,
                                   gp = gpar(fontface = if (is.summary[i]) "bold" else "plain",
                                             fontsize = fontsize, 
                                             col = rep(col$text, length = nr)[i]))
    }
  }
  
  colgap <- unit(5, "mm")
  colwidths <- unit.c(max(unit(rep(1,sum(widthcolumn)),
                             "grobwidth",labels[[1]][widthcolumn])),colgap)
  if (nc > 1) {
    for (i in 2:nc)
      colwidths <- unit.c(colwidths, 
                          max(unit(rep(1,sum(widthcolumn)),
                                   "grobwidth",labels[[i]][widthcolumn])),
                          colgap)
  }
  
  ROBwidth <- unit(0.3, "inches")
  colwidths <- unit.c(colwidths, graphwidth, rep(ROBwidth,numROB))
  
  hg <- nr + numROB + 1 
  
  pushViewport(viewport(layout = grid.layout(hg + 1, 2*nc + 10,
                                             widths = colwidths,
                                             heights = unit(c(rep(1,hg),0.3),"lines"))))
  ### ROB plot 
  for (i in 2:nr) {
    for (j in 1:6) {
      pushViewport(viewport(layout.pos.row = i, layout.pos.col =  2*nc + 1 + j))
      #  pushViewport(viewport(layout.pos.row <- i, layout.pos.col <-  j))
      if (M_first[i,j] != "") {
        col_first <- assignColor(M_first[i,j])
        col_second <- assignColor(M_second[i,j])
        draw.semicircle(cx,cy, r, r, 2, col = col_first,units = "native")
        draw.semicircle(cx,cy, r, r, 4, col = col_second,units = "native")
        popViewport()
      } else {
        draw.semicircle(cx,cy, r, r, 2, col = "black",units = "native")
        draw.semicircle(cx,cy, r, r, 4, col = "black",units = "native")
      }
    }
  }
  
  ### LHS table 
  for (i in 1:nr) {
    if (i == 1) {
      for (k in 1:numROB) {
        ## ROB plot label 
        pushViewport(viewport(layout.pos.row = i, 
                              layout.pos.col =  2*nc + 1 + k))
        aux <-  textGrob(Nms[k], x = 0.7, just = "left",rot = 90,
                         gp = gpar(fontsize = 10,fontface = "plain" ))
        grid.draw(aux)
        popViewport()
      }
    }
    for (j in 1:(nc)) {      
      if (!is.null(labels[[j]][[i]])) {
        pushViewport(viewport(layout.pos.row = i,
                              layout.pos.col = 2*j - 1))
        grid.draw(labels[[j]][[i]])
        popViewport()
      }
    }
  }
  
  ### main plot 
  if (xlog) {
    lower <- log(lower)
    mean  <- log(mean)
    upper <- log(upper)
    xrange <-  c(max(min(lower, na.rm = TRUE),clip[1]), 
                 min(max(upper, na.rm = TRUE),clip[2]))
#    xrange <- c(min(lower, na.rm =  T), max(upper, na.rm = T))
    xrange[1] <- min(0.01, xrange[1])
  }else{
    if (!any(!is.na(upper))) {
       xrange <- c(min(mean, na.rm = TRUE), max(mean, na.rm  = TRUE))
       if (diff(xrange) == 0) {
         ## xrange only have one value case; +/- 2 
          xrange[1] <- xrange[1] - 2
          xrange[2] <- xrange[2] + 2 
        }
    }else{
       cwidth <- upper - lower
       xrange <- c(max(min(lower,na.rm = TRUE), clip[1]), 
                   min(max(upper,na.rm = TRUE), clip[2]))
    }
  }
  
#  pushViewport(viewport(layout.pos.row = nr + 1, layout.pos.col= 2*nc+1, xscale=xrange))
#  ticks <- pretty(xrange)
  ## plot vertical reference line 
  if (measure.method == "OR" || measure.method == "PR") {
    pushViewport(viewport(layout.pos.row = nr + 1, 
                          layout.pos.col = 2*nc + 1,
                          xscale = xrange))
    if (xlog) {
      if (min(lower, na.rm = T) < 0) {
        grid.lines(x = unit(0,"native"),
                   y = 0:hg,
                   gp = gpar(col = "lightgray", lty = 2, lwd = 3))
      }
      ticks <-  pretty(exp(xrange))
      ticks <-  ticks[ticks > 0]
      if (length(ticks)) {
          if (min(lower,na.rm = TRUE) < clip[1]) ticks <- c(exp(clip[1]),ticks)
          if (max(upper,na.rm = TRUE) > clip[2]) ticks <- c(ticks,exp(clip[2]))
          ticks <- unique(c(exp(min(lower, na.rm = T)), exp(max(upper, na.rm = T)), exp(0),ticks))

          xax <- xaxisGrob(gp = gpar(cex = 0.6, col = col$axes),
                           at = log(ticks), name = "xax")
          xax1 <- editGrob(xax, gPath("labels"), 
                           label = format(ticks, digits = 2))
          grid.draw(xax1)
      }
    } else {
      if (min(lower, na.rm = T) < 1) {
        grid.lines(x = unit(0,"native"), 
                   y = 0:hg,gp = gpar(col = "lightgray", 
                                      lty = 2, lwd = 3))
      }
      ticks <- pretty(xrange)
      if (min(lower, na.rm = TRUE) < clip[1]) ticks <- c(clip[1], ticks)
      if (max(upper, na.rm = TRUE) > clip[2]) ticks <- c(ticks, clip[2])
      xax <- xaxisGrob(gp = gpar(cex = 0.6,col = col$axes), at = ticks,name = "xax")
      xax1 <- editGrob(xax, gPath("labels"), label = format(ticks,digits = 2))
      grid.draw(xax1)
    }    
  } else if (measure.method == "beta" || measure.method == "Mean difference") {
    
    pushViewport(viewport(layout.pos.row = nr + 1, 
                          layout.pos.col = 2*nc + 1,
                          xscale = xrange))
    if (!all(is.na(lower))) {
      if (min(lower, na.rm = T) < 0) {
        grid.lines(x = unit(0,"native"), 
                   y = 0:hg, gp = gpar(col = "lightgray", lty = 2, lwd = 3))
      }
    }

      if (xlog) {
        ticks <-  pretty(exp(xrange))
        ticks <- ticks[ticks > 0]
        if (length(ticks)) {
          if (min(lower,na.rm = TRUE) < clip[1]) ticks <- c(exp(clip[1]),ticks)
          if (max(upper,na.rm = TRUE) > clip[2]) ticks <- c(ticks,exp(clip[2]))
          ticks <- unique(c(exp(min(lower, na.rm = T)), 
                            exp(max(upper, na.rm = T)), exp(0),ticks))

          xax <- xaxisGrob(gp = gpar(cex = 0.6,
                                     col = col$axes),
                           at = log(ticks), 
                           name = 
                             "xax")
          xax1 <- editGrob(xax, gPath("labels"), 
                           label = format(ticks, digits = 2))
          grid.draw(xax1)
        }
      }else{      
        ticks <- pretty(xrange)
        if (min(lower, na.rm = TRUE) < clip[1]) ticks <- c(clip[1], ticks)
        if (max(upper, na.rm = TRUE) > clip[2]) ticks <- c(ticks, clip[2])
        xax <- xaxisGrob(gp = gpar(cex = 0.6,col = col$axes), 
                         at = ticks,name = "xax")
        xax1 <- editGrob(xax, gPath("labels"), 
                         label = format(ticks, 
                                        digits = 2))
        grid.draw(xax1)
      }    
  #    popViewport()
#    }
  }else{
    pushViewport(viewport(layout.pos.row = nr + 1, 
                          layout.pos.col = 2*nc+1,
                          xscale = xrange))
    ticks <- pretty(xrange)
    if (min(lower, na.rm = TRUE) < clip[1]) ticks <- c(clip[1], ticks)
    if (max(upper, na.rm = TRUE) > clip[2]) ticks <- c(ticks, clip[2])
    xax <- xaxisGrob(gp = gpar(cex = 0.6,col = col$axes), at = ticks,name = "xax")
    xax1 <- editGrob(xax, gPath("labels"), label = format(ticks,digits = 2))
    grid.draw(xax1)
  }


  grid.text(xlab, y = unit(-2, "lines"), gp = gpar(col = col$axes, fontsize = 11, offset = 0.4))
  popViewport()
  
  ## plot CI 
  for (i in 2:nr) {
    #   if (is.na(mean[i])) next
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 2*nc+1,
                          xscale = xrange))
    info <- 0.3 ## diamond size 
    drawCI(lower[i], mean[i], upper[i],info)
    popViewport()
  }
  
  ### add legend
  ## add legend for circle ROB 

  
  nlegend <- length(col_ROB)
  for(i in 1:nlegend){
    pushViewport(viewport(layout.pos.row = (hg - 1), layout.pos.col = 2*nc+2+i))
    grid.rect(width = unit(0.2,"inches"), height = unit(0.2,"inches"),
              just = "bottom", gp = gpar(fill = col_ROB[i],lty = "dashed"))
    grid.text(names(col_ROB)[i], y = unit(0.01,"npc") - unit(1,"lines"),
              rot = 90,
              gp = gpar(fontsize = 9))
    popViewport()
  }
  popViewport()
}
