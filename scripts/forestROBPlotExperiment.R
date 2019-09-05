draw.semicircle <- function(base.x, base.y, base.length,
                            height = base.length, side = 2, 
                            col = NULL, units = "native"){
  ## draw semicircle filled with given color: side <- 2 -> left side; side <- 4-> right side  
  radius <- base.length/2
  x <- radius*seq(-1,1,length = 40)
  #y<-height/radius*sqrt(radius^2-x^2)
  y <- sqrt(radius^2 - x^2)
  co <- as.integer(cos(pi*(3-side)/2))
  so <- as.integer(sin(pi*(3-side)/2))
  tx <- co*x - so*y 
  ty <- so*x + co*y
  base.y <- base.y+radius
  x <- base.x+tx
  y <- base.y+ty
  cx_y <- max(x,y)  
  grid.polygon(x,y,default.units= units,gp = gpar(fill=col))
}

assignColor <- function(s){
  ## assign color for each ROB case 
  s <- gsub("\\s+","",s)
  s <- col_ROB[s]
  return(s)
}

forest_ROB_plot_expriment <- function(){
  
  first <- rbind(c("High", "Unclear","Low","Low","Unclear","Low","High"),
                 c("High", "Unclear", "High","High","Unclear","High","High"))
  second <- rbind(c("High","Unclear","Low","Low","Unclear","Low","High"),
                  c("High","Unclear","High","High","Unclear","High"," High"))
  
  ROBwidth <- unit(0.3, "inches")
  colwidths <- unit.c(rep(ROBwidth,7))
  
  r = 0.9
  cx = 0.8 
  cy = 0
  
  plot.new()
  pushViewport(viewport(layout = grid.layout(3, 8,
                                             widths = colwidths,
                                             heights = unit(c(rep(1,3),0.3),"lines"))))
  ### ROB plot 
  for(i in 1:2){
    for(j in 1:7){
      pushViewport(viewport(layout.pos.row = i + 1, layout.pos.col =  j))
      #  pushViewport(viewport(layout.pos.row <- i, layout.pos.col <-  j))
      if(M_first[i,j] != ""){
        col_first <- assignColor(first[i,j])
        col_second <- assignColor(second[i,j])
        draw.semicircle(cx,cy, r, r, 2, col= col_first,units = "native")
        draw.semicircle(cx,cy, r, r, 4, col = col_second,units = "native")
        popViewport()
      }else{
        draw.semicircle(cx,cy, r, r, 2, col= "black",units = "native")
        draw.semicircle(cx,cy, r, r, 4, col = "black",units = "native")
      }
    }
  }
  
  Nms <-  c("Random sequence generation", "Allocation concealment", "Blinding owners/personnel", 
            "Blinding outcome assessors", "Incomplete outcome data", "Selective reporting", 
            "Other sources of bias")
  
  for(k in 1:7){
    ## ROB plot label 
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col =  k))
    aux <-  textGrob(Nms[k], x = 0.7, just = "left",rot = 90,
                     gp = gpar(fontsize = 10,fontface = "plain" ))
    grid.draw(aux)
    popViewport()
  }
  
  popViewport()
  
} 

