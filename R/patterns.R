## Sedimentary Lithology Patterns
gridPts <- function(xExp, yExp, from, to, xP, yP) {
  pMed <- abs(to-from) / 2
  nT <- expand.grid(x=seq(-0.1, 0.99, xExp), 
	    	   y= sort(unique(c(from + pMed + seq(0, pMed, yExp), 
			  pMed + from - seq(0, pMed, yExp)))))
  indPo <- seq(1,nrow(nT), 2)
  nTa <- data.frame(x=nT[indPo, 1] + xExp/3, y=nT[indPo, 2] + yExp/3)
  nTb <- data.frame(x=nT[indPo + 1, 1] + xExp/3, y=nT[indPo + 1, 2] + yExp/3)
  nTc <- data.frame(x=nT[indPo, 1] - xExp/3, y=nT[indPo, 2] - yExp/3)
  nTd <- data.frame(x=nT[indPo + 1, 1] - xExp/3, y=nT[indPo + 1, 2] - yExp/3)
  xyT <- nT[which(inpolygon(nT$x, nT$y, xP, yP, boundary = F) == TRUE),]
  xyTa <- nTa[which(inpolygon(nTa$x, nTa$y, xP, yP, boundary = F) == TRUE),]
  xyTb <- nTb[which(inpolygon(nTb$x, nTb$y, xP, yP, boundary = F) == TRUE),]
  xyTc <- nTc[which(inpolygon(nTc$x, nTc$y, xP, yP, boundary = F) == TRUE),]
  xyTd <- nTd[which(inpolygon(nTd$x, nTd$y, xP, yP, boundary = F) == TRUE),]
  return(list(xyT, xyTa, xyTb, xyTc, xyTd))
}
## Claystone N 620
clayP <- function(xP, yP, from, to, fill, sc.fac, ...) {
  yExp <- 0.25 * sc.fac
  pMed <- abs(to - from) / 2
  yPt <- sort(unique(c(pMed + from + seq(0, pMed, yExp), 
		   pMed + from - seq(0, pMed, yExp))))
  yCord <- yPt[yPt > from & yPt < to]
  difY <-   (yCord[2] - yCord[1]) / 2
  tmpyCord <- c(yCord[1] - difY, yCord + difY)
  yCord2 <- tmpyCord[tmpyCord > from & tmpyCord < to]
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), 
    default.units="native")
    if(length(yCord) != 0) {
      grid.polyline(x=c(rep(0, length(yCord)), rep(max(xP) - 0.03, length(yCord))),
        y=c(yCord, yCord), id=rep(1:length(yCord), 2),
	    default.units = "native", gp=gpar(lty="81888188", lwd=0.7))
    }
    if(length(yCord2) != 0) {
      grid.polyline(x=c(rep(0, length(yCord2)), rep(max(xP) - 0.02, length(yCord2))),
        y=c(yCord2, yCord2), id=rep(1:length(yCord2), 2),
        default.units = "native", gp=gpar(lty="68818881", lwd=0.7))
    }
}
## Mudstone pattern
mudP <- function(xP, yP, from, to, fill, sc.fac, datum, ...) {
  yExp <- 0.275 * sc.fac
  pMed <- abs(to - from) / 2
  yPt <- sort(unique(c(pMed + from + seq(0, pMed, yExp), 
		   pMed + from - seq(0, pMed, yExp))))
  yCord <- yPt[yPt > from & yPt < to]
  difY <-   (yCord[2] - yCord[1]) / 2
  tmpyCord <- c(yCord[1] - difY, yCord + difY)
  yCord2 <- tmpyCord[tmpyCord > from & tmpyCord < to]
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), 
    default.units="native")
  if(length(yCord) != 0) {
    grid.polyline(x=c(rep(0, length(yCord)), rep(max(xP) - 0.03, length(yCord))),
      y=c(yCord, yCord), id=rep(1:length(yCord), 2),
	  default.units = "native", gp=gpar(lty="81818828", lwd=0.7))
   }
   if(length(yCord2) != 0) {
     grid.polyline(x=c(rep(0, length(yCord2)), rep(max(xP) - 0.02, length(yCord2))),
       y=c(yCord2, yCord2), id=rep(1:length(yCord2), 2),
       default.units = "native", gp=gpar(lty="88288181", lwd=0.7))
   }
}
# Silstone pattern
siltP <- function(xP, yP, from, to, fill, pSize, sc.fac, datum, ...) {
  xExp <- 0.18
  yExp <- 0.2 * sc.fac
  pMed <- abs(to-from)/2
  xGrid <- seq(0.02, 1, xExp)
  lengthLine <- 0.08
  nT <- expand.grid(x=xGrid, 
          y=sort(unique(c(pMed + from + seq(0, pMed, yExp), 
		  pMed + from - seq(0, pMed, yExp)))))
  xP2 <- c(nT$x, ifelse(gl(2,length(xGrid),nrow(nT)) == 1, 
	       nT$x - lengthLine, nT$x + lengthLine))
  yP2 <- c(nT$y, nT$y)
  xyP2 <- data.frame(xP2, yP2, id=rep(2:(nrow(nT) + 1), 4))
  inxyPt <- xyP2[which(inpolygon(xyP2$xP2, xyP2$yP2, xP, yP, boundary = F) == T),]
  inxyP2 <- data.frame(table(inxyPt$id))
  inxyPt2 <- inxyP2[which(inxyP2$Freq == 4),1]
  xypP <- inxyPt[which(inxyPt$id %in% inxyPt2),]
  grid.path(c(xP, xypP$xP2), c(yP, xypP$yP2),
    id=c(rep(1,length(xP)), xypP$id), 
	gp=gpar(fill=fill, lwd=0.025),
	default.units="native", rule="evenodd")
  xP3 <- c(ifelse(gl(2,length(xGrid),nrow(nT)) == 1, 
    nT$x + 0.05, nT$x - 0.05))
  xyP3 <- data.frame(xP3=c(xP3, xP3 + 0.025, xP3 - 0.025), 
		    yP3=c(rep(nT$y, 3)))
  xypP2 <- xyP3[which(inpolygon(xyP3$xP3, xyP3$yP3, xP, yP, boundary = F) == T),]
  if(nrow(xypP2) != 0) {
  	grid.points(xypP2$xP3, xypP2$yP3,
  	  default.units="native", size = unit(0.5, "mm"), pch=16)
  }
}
## Shale pattern
shaleP <-  function(xP, yP, from, to, fill, sc.fac, datum, ...) {
  yExp <- 0.25 * sc.fac
  xExp <- 0.05
  pMed <- abs(to-from)/2
  xGrid <- seq(0, 1, xExp)
  line <- 0.05
  nT <- expand.grid(x=xGrid, 
   	      y=sort(unique(c(pMed + yExp/2 + from + seq(0, pMed, yExp), 
		  pMed + from - seq(0, pMed, yExp) - yExp/2))))
  xP2 <- nT$x + (line * 2)
  xP3 <- nT$x + line-(line / 2)
  yP4 <- ifelse(gl(2,1,nrow(nT)) == 1, 
  	       nT$y + yExp, nT$y - yExp)
  xyP2 <- data.frame(x=c(nT$x, xP2, rep(xP3, 3)), 
            y=c(rep(nT$y, 3), yP4, nT$y),
   		    id=c(rep(2:(length(xP2)+1), 5)))
  inxyPt <- xyP2[which(inpolygon(xyP2$x, xyP2$y, xP, yP, boundary = T) == T),]
  inxyP2 <- data.frame(table(inxyPt$id))
  inxyPt2 <- inxyP2[which(inxyP2$Freq >= 4),1]
  xypP <- inxyPt[which(inxyPt$id %in% inxyPt2),]
  grid.path(c(xP, xypP$x), c(yP, xypP$y),
	id=c(rep(1,length(xP)), xypP$id), gp=gpar(fill=fill, lwd=0.05),
	default.units="native", rule="evenodd")
}
# New Sandstone pattern
sandP <- function(xP, yP, from, to, fill, sc.fac, pSize, datum) {
  xExp <- 0.035 
  yExp <- 0.275 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[4]], lsPt[[2]], lsPt[[3]])
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(nrow(grnT) != 0) {
    grid.points(grnT$x, grnT$y, default.units="native", 
	  size = unit(0.3, "mm"), pch=16)
  }
  if(nrow(lsPt[[5]]) != 0) {
    grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
	  lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
	  default.units="native", size = unit(0.3, "mm"), pch=16)
    grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
	  lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
	  default.units="native", size = unit(0.3, "mm"), pch=16)
  }
  if(nrow(lsPt[[1]]) != 0) {
    grid.points(lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], 
      lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0],
	  default.units="native", size = unit(0.3, "mm"), pch=16)
    grid.points(lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0] - 0.025, 
      unit(lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], "native") + unit(0.175, "mm"),
      default.units="native", size = unit(0.3, "mm"), pch=16)
    grid.points(lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0] - 0.05, 
      unit(lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], "native") - unit(0.35, "mm"),
      default.units="native", size = unit(0.3, "mm"), pch=16)
  }
}
## Conglomerate pattern
congP <- function(xP, yP, from, to, fill, sc.fac, pSize, datum) {
  xExp <- 0.04 
  yExp <- 0.3 * sc.fac
  adjustLwd <- function(gp) {
    gp$lwd <- 0.25
    gp$col <- "grey20"
    gp
  }
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[2]], lsPt[[3]], lsPt[[4]])
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.25), default.units="native")
  if(nrow(grnT) != 0) {
    grid.symbols(SDAR.sym[["pat.sym"]][["conglomerate.svg"]], grnT$x, grnT$y, default.units="native", 
	  size = unit(pSize * 5 * 1.75, "mm"),
	  gpFUN=adjustLwd)
    }
  if(nrow(lsPt[[5]]) != 0) {
    grid.symbols(SDAR.sym[["pat.sym"]][["conglomerate.svg"]], 
      lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
	  lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
	  default.units="native", size = unit(pSize * 10 * 1.75, "mm"),
	  gpFUN=adjustLwd)
    grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
	  lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
	  default.units="native", size = unit(0.5, "mm"), pch=16)
  }
  if(nrow(lsPt[[1]]) != 0) {
    grid.symbols(SDAR.sym[["pat.sym"]][["conglomerate.svg"]], 
      lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], 
	  lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0],
	  default.units="native", size = unit(pSize * 7 * 1.75, "mm"),
	  gpFUN=adjustLwd)
    grid.symbols(SDAR.sym[["pat.sym"]][["conglomerate.svg"]], 
      lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0] - 0.025, 
	  unit(lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], "native") + unit(0.175, "mm"),
	  default.units="native", size = unit(pSize * 3 * 1.75, "mm"),
	  gpFUN=adjustLwd)
    grid.symbols(SDAR.sym[["pat.sym"]][["conglomerate.svg"]], 
      lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0] - 0.05, 
	  unit(lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], "native") - unit(0.35, "mm"),
	  default.units="native", size = unit(pSize * 3.5 * 1.75, "mm"),
	  gpFUN=adjustLwd)
  }
}
## Breccia pattern
brecP <- function(xP, yP, from, to, fill, sc.fac, pSize, datum) {
  xExp <- 0.05 
  yExp <- 0.4 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[2]], lsPt[[4]])
  adjustLwd <- function(gp) {
    gp$lwd <- 0.25
    gp$col <- "grey20"
    gp
  }
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.25), default.units="native")
  if(nrow(grnT) != 0) {
    grid.points(grnT$x, grnT$y, default.units="native", 
	  size = unit(pSize * 1, "mm"))
  }
  if(nrow(lsPt[[1]]) != 0) {
    grid.points(lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 == 0], 
 	  lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 == 0], 
 	  default.units="native", size = unit(pSize * 1.25, "mm"))
    grid.symbols(SDAR.sym[["pat.sym"]][["breccia_7.svg"]], 
      lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0], 
	  lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0],
	  default.units="native", size = unit(pSize * 3.5 * 1.5, "mm"),
	  gpFUN=adjustLwd)
  }
  if(nrow(lsPt[[5]]) != 0) {
    grid.symbols(SDAR.sym[["pat.sym"]][["breccia_3.svg"]],
      lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
	  lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
	  default.units="native", size = unit(pSize * 4.5 * 1.5, "mm"),
	  gpFUN=adjustLwd) 
    grid.symbols(SDAR.sym[["pat.sym"]][["breccia_4.svg"]], 
      lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
	  lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
	  default.units="native", size = unit(pSize * 5 * 1.5, "mm"),
	  gpFUN=adjustLwd)
  }
  if(nrow(lsPt[[3]]) != 0) {
    grid.symbols(SDAR.sym[["pat.sym"]][["breccia_5.svg"]],
      lsPt[[3]]$x[as.numeric(row.names(lsPt[[3]])) %% 2 == 0], 
	  lsPt[[3]]$y[as.numeric(row.names(lsPt[[3]])) %% 2 == 0],
	  default.units="native", size = unit(pSize * 4 * 1.5, "mm"),
	  gpFUN=adjustLwd)	      
  }
}
## Limestone pattern
limeP <-  function(xP, yP, from, to, fill, sc.fac, datum, ...) {
  yExp <- 0.15 * sc.fac
  xExp <- 0.05
  pMed <- abs(to-from)/2
  xGrid=seq(0, 1, xExp)
  line <- 0.075
  nT <- expand.grid(x=xGrid, 
    	  y=sort(unique(c(pMed + yExp/2 + from + seq(0, pMed, yExp), 
		  pMed + from - seq(0, pMed, yExp) - yExp/2))))
  xP2 <- nT$x + (line * 2)
  xP3 <- nT$x + line-(line / 2)
  yP4 <- ifelse(gl(2,1,nrow(nT)) == 1, 
    	   nT$y + yExp, nT$y - yExp)
  xyP2 <- data.frame(x=c(nT$x, xP2, rep(xP3, 3)), 
            y=c(rep(nT$y, 3), yP4, nT$y),
   		    id=c(rep(2:(length(xP2)+1), 5)))
  inxyPt <- xyP2[which(inpolygon(xyP2$x, xyP2$y, xP, yP, boundary = T) == T),]
  inxyP2 <- data.frame(table(inxyPt$id))
  inxyPt2 <- inxyP2[which(inxyP2$Freq >= 4),1]
  xypP <- inxyPt[which(inxyPt$id %in% inxyPt2),]
  grid.path(c(xP, xypP$x), c(yP, xypP$y),
    id=c(rep(1,length(xP)), xypP$id), gp=gpar(fill=fill, lwd=0.015),
	default.units="native", rule="evenodd")
}
## Dolomite pattern
dolP <- function(xP, yP, from, to, fill, sc.fac, datum, ...) {
  yExp <- 0.45 * sc.fac
  pMed <- abs(to - from) / 2
  yPt <- sort(unique(c(pMed + yExp/2 + from + seq(0, pMed, yExp), 
    pMed + from - seq(0, pMed, yExp) - yExp/2)))
  yCord <- yPt[yPt > from & yPt < to]
  difY <- (yCord[2] - yCord[1]) / 2
  tmpyCord <- c(yCord[1] - difY, yCord + difY)
  yCord2 <- tmpyCord[tmpyCord > from & tmpyCord < to]
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.03), 
    default.units="native")
  if(length(yCord) != 0) {
    grid.polyline(x=c(rep(0, length(yCord)), rep(max(xP) - 0.03, length(yCord))),
      y=c(yCord, yCord), id=rep(1:length(yCord), 2),
	  default.units = "native", gp=gpar(lty=1, lwd=0.7))
  }
  if(length(yCord2) != 0) {
    grid.polyline(x=c(rep(0, length(yCord2)), rep(max(xP) - 0.02, length(yCord2))),
      y=c(yCord2, yCord2), id=rep(1:length(yCord2), 2),
      default.units = "native", gp=gpar(lty=1, lwd=0.7))
  }
  yExp <- 0.47 * sc.fac
  xExp <- 0.15
  pSize <- 0.1
  if(length(yCord) != 0) {
    nT <- expand.grid(x=seq(0.05, 0.99, xExp) + xExp/2, 
	   	    y= yCord + yExp/4)
    inxyPt <- nT[which(inpolygon(nT$x, nT$y, xP, yP, boundary = T) == T),]
    grid.points(inxyPt$x, inxyPt$y, pch="/", 
	  size = unit((pSize) * sc.fac, "native"))
  }
  if(length(yCord) != 0) {
    nT2 <- expand.grid(x=seq(0.05, 0.99, xExp), 
	   	     y= yCord2 + yExp/4)
    inxyPt2 <- nT2[which(inpolygon(nT2$x, nT2$y, xP, yP, boundary = T) == T),]
    grid.points(inxyPt2$x, inxyPt2$y, pch="/", 
	  size = unit((pSize) * sc.fac, "native"))
  }
}
## Coal pattern
coalP <- function(xP, yP, from, to, fill, datum, ...) {
    grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.15), 
      default.units="native")
  }

## chert pattern
## chalk pattern
## Phosphorite pattern
## Siderite pattern
## pyroclastic pattern
pyroP <- function(xP, yP, from, to, fill, sc.fac, datum, ...) { 
  pO <- 0.3 * sc.fac
  pM <- abs(to-from)/2
  xGrid=seq(0.025, 0.975, 0.1)
  nT <- expand.grid(xGrid=xGrid,
          yGrid= sort(unique(c(pM + from + seq(0, pM, pO), 
		  pM + from - seq(0, pM, pO)))))
  xyP2 <- data.frame(xGrid=c(ifelse(gl(2,length(xGrid),nrow(nT)) == 1, 
   	        nT$xGrid + 0.05, nT$xGrid)), yGrid=nT$yGrid)
  inxyP <- inpolygon(xyP2$xGrid, xyP2$yGrid, xP, yP, boundary = F)
  xypP <- xyP2[which(inxyP == TRUE),]
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.5), 
    default.units="native")
  grid.points(xypP$xGrid, xypP$yGrid, pch = 4, 
    size = unit(0.02, "native"), gp=gpar(lwd=0.4))
}
# Igneous pattern
ignP <- function(xP, yP, from, to, fill, sc.fac, datum, ...) { 
  pO <- 0.3 * sc.fac
  pM <- abs(to-from)/2
  xGrid=seq(0.025, 0.975, 0.1)
  nT <- expand.grid(xGrid=xGrid,
	      yGrid= sort(unique(c(pM + from + seq(0, pM, pO), 
		  pM + from - seq(0, pM, pO)))))
  xyP2 <- data.frame(xGrid=c(ifelse(gl(2,length(xGrid),nrow(nT)) == 1, 
     	    nT$xGrid + 0.05, nT$xGrid)), yGrid=nT$yGrid)
  inxyP <- inpolygon(xyP2$xGrid, xyP2$yGrid, xP, yP, boundary = F)
  xypP <- xyP2[which(inxyP == TRUE),]
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.5), 
    default.units="native")
  grid.points(xypP$xGrid, xypP$yGrid, pch = 3, 
    size = unit(0.025, "native"), gp=gpar(lwd=0.4))
}
## Halite pattern
haliteP <- function(xP, yP, from, to, fill, sc.fac, datum, ...) {
  pO <- 0.25 * sc.fac
  pM <- abs(to - from) / 2
  yCord <- sort(unique(c(pM + from + seq(0, pM, pO), 
             pM + from - seq(0, pM, pO))))
  difY <-   (yCord[2] - yCord[1]) / 2
  tmpyCord <- c(yCord[1] - difY, yCord + difY)
  yCord2 <- tmpyCord[tmpyCord > from & tmpyCord < to]
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.5), 
    default.units="native")
  grid.polyline(x=c(rep(0, length(yCord)), rep(max(xP) - 0.03, length(yCord))),
    y=c(yCord, yCord), id=rep(1:length(yCord), 2),
    default.units = "native", gp=gpar(lty=1, lwd=0.4))
  grid.polyline(x=c(rep(0, length(yCord2)), rep(max(xP) - 0.02, length(yCord2))),
    y=c(yCord2, yCord2), id=rep(1:length(yCord2), 2),
    default.units = "native", gp=gpar(lty=1, lwd=0.4))
  seqPts <- seq(0.05, max(xP), 0.05)
  grid.polyline(x=c(seqPts, seqPts),
    y=c(rep(from, length(seqPts)), rep(to, length(seqPts))), 
    id=rep(1:length(seqPts), 2),
    default.units = "native", gp=gpar(lty=1, lwd=0.4))
}
## function inpolygon from pracma library
## Thanks to to Hans W Borchers
inpolygon <- function (x, y, xp, yp, boundary = FALSE) {
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y), 
    is.numeric(xp), is.numeric(yp), length(xp) == length(yp))
    n <- length(x)
    np <- length(xp)
    if (xp[1] != xp[np] || yp[1] != yp[np]) {
        xp <- c(xp, xp[1])
        yp <- c(yp, yp[1])
        np <- np + 1
    }
    inpoly <- rep(FALSE, n)
    onpoly <- rep(FALSE, n)
    j <- np
    for (i in 1:np) {
        dxp <- xp[j] - xp[i]
        dyp <- yp[j] - yp[i]
        dist <- dxp * (y - yp[i]) - (x - xp[i]) * dyp
        idx1 <- (((yp[i] <= y & y < yp[j]) | (yp[j] <= y & y < 
            yp[i])) & (0 < dist * dyp))
        inpoly[idx1] <- !inpoly[idx1]
        idx2 <- (((yp[i] <= y & y <= yp[j]) | (yp[j] <= y & y <= 
            yp[i])) & ((xp[i] <= x & x <= xp[j]) | (xp[j] <= 
            x & x <= xp[i])) & (0 == dist | !dxp))
        onpoly[idx2] <- TRUE
        j <- i
    }
    if (boundary) 
        inpoly[onpoly] <- TRUE
    else inpoly[onpoly] <- FALSE
    return(inpoly)
}