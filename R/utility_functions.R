# Input data validation
input.val <- function(x, col.na){
  if(all(c("base", "top", col.na) %in% names(x)) == FALSE) {
    stop(call.=FALSE, paste("The names of the ", substitute(x), 
    	" does not match with the required", " ('base', 'top', '", 
    	substitute(col.na), "')", sep=""))
  }
  if(any(lapply(x[,c("base", "top")], is.numeric) == FALSE)) {
    stop(call.=FALSE, paste("Check ", substitute(x), 
      ", base and top must be numeric class", sep="")) 
  }
  if(is.factor(x[[col.na]])) {
    stop(call.=FALSE, paste("Check ", substitute(x), 
      col.na, " must to be numeric or character class"))
  }
}
input.val.log <- function(x, col.na){
  if(all(c("Depth", col.na) %in% names(x)) == FALSE) { 
    stop(call.=FALSE, paste("The names of the ", substitute(x), 
      " does not match with the required", " ('Depth', '", 
      substitute(col.na), "')", sep=""))
  }
}
input.check <- function(x, col.na, table.na) {
  if(is.numeric(x[[col.na]])) {
    notIn <- which(!x[[col.na]] %in% table.na[, "id"])
    if(length(notIn) != 0) {
      stop(call.=FALSE, paste(c("Row(s) number(s):", head(notIn), 
           "is(are) not", substitute(x), "valid value(s)"), collapse=" "))
    }
  }else{
    notIn <- which(!x[[col.na]] %in% table.na[, "name"])        # pendiente lowercase and trim
    if(length(notIn) != 0) {
      stop(call.=FALSE, paste(c("Row(s) number(s):", head(notIn), 
           "is(are) not", substitute(x), "valid value(s)"), collapse=" "))
    }else{
      x[[col.na]] <- match(x[[col.na]], table.na[, "name"])
    }
  }  
 return(x) 
}
int.event <- function(x, datum, subset.lim, arg){
  if(datum == "top") {  
    if(any(x$base < x$top)) {
      notIn <- which(x$base > x$top)
      stop(call.=FALSE, paste(c(arg, "as datum='top', base must be greather than top, Check rows number: ", 
           head(notIn)), collapse=", "))
    }
    x[, c("base", "top")] <- x[, c("base", "top")] * -1
  }
  event.x <- as_events(x, from.col="base", to.col="top")
  sub.x <- crop_events(event.x, subset.lim)
}

# Drawing functions
drawHeader <- function(poscol, posrow, h.label, rot, h.cex, h.font) {
  pushViewport(viewport(layout.pos.col=poscol,	
 	       layout.pos.row=posrow, name=h.label))
    grid.rect(x=0.5, y=0.5, width=1, 
              height=1, gp=gpar(lwd=0.25))
    grid.text(h.label, x=0.5, y=0.5, rot=rot, 
              gp=gpar(cex=h.cex, font=h.font))
  upViewport()	
}
open.viewport <- function(uno, poscol, datum, iv, y.sc){
  	pushViewport(viewport(layout.pos.col=poscol, 
  layout.pos.row = uno + 4 - iv,
                    xscale = c(0, 1), 
	        yscale = y.sc, name="bednumber"))
        	grid.rect(gp=gpar(lwd=0.45, 
			col="grey", lty=2))
}
well_logGrob <- function(two_col_log, y.sc=NULL, name=NULL, grille=20, vp=NULL, xlim.GR=NULL) {
  xscale <- xlim.GR
  if(is.null(y.sc)) {
    y.sc <- c(min(two_col_log$from), max(two_col_log$from))
  }
  axislab <- seq(xscale[1], xscale[2], 50)
  lindiv <- seq(xscale[1], xscale[2], 50)
  ydiv20ft <- seq(round_up(unlist(y.sc[1]), to=20), 
  round_up(unlist(y.sc[2]), to=20) - 20, 20)
  gplot <- gTree(children=gList(
    linesGrob(x=two_col_log[["GR"]], 
      y=two_col_log[["from"]],
      default.units = 'native', 
      gp=gpar(col="forestgreen", lwd=0.5, alpha=0.8)),
    rectGrob(gp=gpar(col="grey", lwd=0.7, lty=2, alpha=0.7)),
    segmentsGrob(x0 = lindiv, y0= y.sc[1], x1= lindiv, y1 = y.sc[2],
      gp=gpar(col="grey", lwd=0.7, lty=2, alpha=0.7),  # Dashed guide lines
      default.units = "native"),
    segmentsGrob(x0 = xscale[1], y0= ydiv20ft, x1= xscale[2], y1 = ydiv20ft,
      gp=gpar(col="grey60", lwd=1, lty=2, alpha=0.9),  # Dashed guide lines every 10 ft
      default.units = "native"),
    xaxisGrob(name="axis", at=axislab, label= axislab, main=F, 
      gp=gpar(cex=0.7, col="black"))), name=name, vp=vp, cl="well_logGrob")
  gplot
}
round_up <- function(x, to=10) {
  to*(x%/%to + as.logical(x%%to))
}
#
`%ni%` <- Negate(`%in%`)
#
draw.header.litho <- function(poscol, posrow) {
  pushViewport(viewport(layout.pos.col=poscol, 
    layout.pos.row=posrow, name="grainSizeHeader"))
      hL <- headerLines; hT <- headerText
    for(i in seq_along(seq_along(hL$x0))) {
      grid.polyline(x=c(hL$x0[i], hL$x1[i]), 
        y=c(hL$y0[i], hL$y1[i]), 
        gp=gpar(lwd=hL$lwd[i], col=hL$colour[i]))
    }
    for(i in seq_along(seq_along(hT$x))) {
      grid.text(hT$label[i], x=hT$x[i], y=hT$y[i], 
        just=hT$just[i], rot=hT$rot[i], 
        gp=gpar(cex=hT$cex[i], font=hT$font[i]))
    }
  upViewport() 
}
draw.guidelines <- function(y.sc, pos.vert){
  for(i in pos.vert) {
    grid.lines(x = c(i, i), y = y.sc,
      gp=gpar(col="grey75", lwd=0.45, lty=2),
      default.units = "native")
  }
}
draw.cover  <- function(sp.base, sp.top, x_axis){       
  grid.lines(x=c(0, x_axis), y=c(sp.base, sp.top),
    default.units = 'native', gp=gpar(lwd=0.1))
  grid.lines(x=c(0.19,0), y=c(sp.base, sp.top), 
    default.units = 'native', gp=gpar(lwd=0.1))
  grid.rect(x=0, y=sp.base, width=0.19, 
    height=(sp.top - sp.base), just=c("left", "bottom"), 
    default.units = 'native', gp=gpar(lwd=0.5))
}
ndec <-function(x) {
  min(which( x*10^(0:10) == floor(x*10^(0:10)) )) - 1
}