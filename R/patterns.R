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
##
is.even <- function(x) x %% 2 == 0

## Claystone pattern
clayP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <-  0.009
  yExp <- 0.15 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- c(1, 7, 15, 21, 29, 35, 43, 49, 57, 63, 71)
      xf <- df_s[nrow(df_s), 1]
      yf <- df_s[nrow(df_s), 2]
      n_df_s <- nrow(df_s) %% 14
      if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], 
            df_s[i+5, 1], df_s[i+5, 2],
            default.units="native", gp=gpar(lwd=0.25))
        }
        if(n_df_s >= 2 & n_df_s < 6){
          ind_p <-  nrow(df_s) %/% 14 * 14
          grid.segments(df_s[ind_p + 1, 1], df_s[ind_p + 1, 2], 
            xf, yf, default.units="native", gp=gpar(lwd=0.25))
        }
        if(n_df_s > 6 & n_df_s < 12){
          ind_p <-  nrow(df_s) %/% 14 * 14
          grid.segments(df_s[ind_p + 7, 1], df_s[ind_p + 7, 2], 
            xf, yf, default.units="native", gp=gpar(lwd=0.25))
        }
      }else{
        s_e <- s_e + 4
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], 
            df_s[i+5, 1], df_s[i+5, 2], 
            default.units="native", gp=gpar(lwd=0.25))
        }
        grid.segments(df_s[1, 1], df_s[1, 2], 
          df_s[2, 1], df_s[2, 2],
          default.units="native", gp=gpar(lwd=0.25)) 
        if(n_df_s - 4 >= 2 & n_df_s - 4 < 6){
          ind_p <-  nrow(df_s) %/% 14 * 14 + 4
          grid.segments(df_s[ind_p + 1, 1], df_s[ind_p + 1, 2], 
            xf, yf, default.units="native", gp=gpar(lwd=0.25))
        }
        if((nrow(df_s) - 4) %% 14 > 6 & (nrow(df_s) - 4) %% 14 < 12){
          ind_p <-  (nrow(df_s) - 4) %/% 14 * 14
          grid.segments(df_s[ind_p + 11, 1], df_s[ind_p + 11, 2], 
            xf, yf, default.units="native", gp=gpar(lwd=0.25))
        }
      }
    }   
  }
}
## Mudstone pattern
mudP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.0052
  yExp <- 0.2 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), 
    default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 5)
      ind <- s_e[c(4, 8, 12, 16, 20)]
      s_e <- s_e[!s_e %in% ind]
      n_df_s <- nrow(df_s) %/% 5 + 1
      s_o <- seq(3, nrow(df_s), 5)
      ind_o <- s_o[c(2, 6, 10, 14, 18)]
      s_o <- s_o[!s_o %in% ind_o]
      if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+4, 1], df_s[i+4, 2],
            gp=gpar(lwd=0.35), default.units="native")
        }
        for(i in ind) {
          grid.points(df_s[i + 2, 1], df_s[i + 2, 2], 
            size = unit(0.3, "mm"), pch=16, default.units="native")
        } 
        if(nrow(df_s) - max(s_e) >= 2 & (nrow(df_s) - max(s_e) < 5)){
          grid.segments(df_s[max(s_e), 1], df_s[max(s_e), 2], df_s[nrow(df_s), 1], 
            df_s[nrow(df_s), 2], gp=gpar(lwd=0.35), default.units="native")
        }
      }else{
        for(i in s_o) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+4, 1], df_s[i+4, 2], 
            gp=gpar(lwd=0.35), default.units="native")
        }
        for(i in ind_o) {
          grid.points(df_s[i + 2, 1], df_s[i + 2, 2], 
            size = unit(0.3, "mm"), pch=16, default.units="native")
        }
        if(nrow(df_s) - max(s_o) >=1 & (nrow(df_s) - max(s_o) < 5)){
          grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2], df_s[nrow(df_s), 1], 
            df_s[nrow(df_s), 2], gp=gpar(lwd=0.35), default.units="native")
        }
      } 
    }
  }
}  
## Siltstone pattern
siltP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.019
  yExp <- 0.2 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), 
    default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(3, nrow(df_s), 8)
      n_df_s <- nrow(df_s) %/% 8 + 1
      s_o <- seq(7, nrow(df_s), 8)
      if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+4, 1], df_s[i+4, 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
        grid.points(df_s[1:2, 1], df_s[1:2, 2], 
          size = unit(0.6, "mm"), pch=16, default.units="native")
          vpts <- rep.int(8:10, n_df_s)
          ad_vpts <- sort(rep(seq(0,nrow(df_s), 8)[1:n_df_s], 3))
          ind_pts <- vpts + ad_vpts
        grid.points(df_s[ind_pts, 1], df_s[ind_pts, 2], 
          size = unit(0.6, "mm"), pch=16, default.units="native")
        if(nrow(df_s) - max(s_e) >= 2 & (nrow(df_s) - max(s_e) < 5)){
          grid.segments(df_s[max(s_e), 1], df_s[max(s_e), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2],
            gp=gpar(lwd=0.25), default.units="native")
        }
      }else{
        for(i in s_o) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+4, 1], df_s[i+4, 2],
            gp=gpar(lwd=0.25), default.units="native")
        }
        grid.segments(df_s[1, 1], df_s[1, 2], df_s[3, 1], df_s[3, 2], 
          gp=gpar(lwd=0.25), default.units="native") 
          vpts <- rep.int(4:6, n_df_s)
          ad_vpts <- sort(rep(seq(0,nrow(df_s), 8)[1:n_df_s], 3))
          ind_pts <- vpts + ad_vpts
        grid.points(df_s[ind_pts, 1], df_s[ind_pts, 2], 
          size = unit(0.5, "mm"), pch=16, default.units="native")
        if(nrow(df_s) - max(s_o) >=1 & (nrow(df_s) - max(s_o) < 5)){
          grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
      }
    }
  }
}
## Shale pattern
shaleP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.025 / 0.9
  yExp <- 0.15 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]  
      if(!is.even(which(j == dy_i))){
        s_e <- seq(1, nrow(df_s), 3)
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+2, 1], df_s[i+2, 2], 
            default.units="native", gp=gpar(lwd=0.25))
        }
        if(nrow(df_s) - max(s_e) == 1){
          grid.segments(df_s[max(s_e), 1], df_s[max(s_e), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
      }else{
        s_o <- seq(2, nrow(df_s), 3)
        for(i in s_o) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+2, 1], df_s[i+2, 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
        if(nrow(df_s) - max(s_o) == 1){
          grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
      }
    }
  }
}  
## Sandstone pattern
sandP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize) {
  xExp <- 0.05 
  yExp <- 0.425 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[4]], lsPt[[2]], lsPt[[3]])
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    if(nrow(grnT) != 0) {
      grid.points(grnT$x, grnT$y, default.units="native", 
	    size = unit(0.4 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[5]]) != 0) {
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
	    lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
	    default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
	    lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
	    default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[1]]) != 0) {
      ypM <- lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      xpM <- lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      grid.points(xpM, ypM,
	      default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.025, 
        unit(ypM, "native") + unit(0.175, "mm"),
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.05,  
        unit(ypM, "native") - unit(0.35, "mm"),
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
    }
  }
}
## Conglomerate pattern
congP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize) {
  xExp <- 0.05 
  yExp <- 0.5 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[4]], lsPt[[2]], lsPt[[3]])
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    if(nrow(grnT) != 0) {
      grid.points(grnT$x, grnT$y, default.units="native", 
      size = unit(0.5, "mm"), pch=1)
    }
    if(nrow(lsPt[[5]]) != 0) {
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
        lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
        default.units="native", size = unit(1.4, "mm"), pch=1)
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
        lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
        default.units="native", size = unit(0.3, "mm"), pch=16)
    }
    if(nrow(lsPt[[1]]) != 0) {
      ypM <- lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      xpM <- lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      grid.points(xpM, ypM,
        default.units="native", size = unit(1.3, "mm"), pch=1)
      grid.points(xpM - 0.025, 
        unit(ypM, "native") + unit(0.175, "mm"),
        default.units="native", size = unit(0.4, "mm"), pch=16)
      grid.points(xpM - 0.05, 
        unit(ypM, "native") - unit(0.35, "mm"),
        default.units="native", size = unit(0.3, "mm"), pch=16)
    }
  }
}
## Breccia pattern
brecP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize) {
  xExp <- 0.05 
  yExp <- 0.6 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[2]], lsPt[[4]])
  adjustLwd <- function(gp) {
    gp$lwd <- 0.25
    gp$col <- "grey20"
    gp
  }
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.25), default.units="native")
  if(isTRUE(fill.pattern)) {
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
}
## Limestone pattern
limeP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.025
  yExp <- 0.25 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2])
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 6)
      grid.segments(df_s[1, 1] - xExp, df_s[1, 2], 
        df_s[nrow(df_s), 1], df_s[nrow(df_s), 2],
        gp=gpar(lwd=0.25), default.units="native")
        if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i, 1], df_s[i, 2] - yExp,
            gp=gpar(lwd=0.25), default.units="native")
        }  
      }else{
        for(i in s_e) {
          grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1], 
            df_s[i+3, 2] - ifelse(j != dy_i[1], yExp, 0),
            gp=gpar(lwd=0.25), default.units="native")
          if(j == dy_i[1]) {
            grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1], yP[1],
            gp=gpar(lwd=0.25), default.units="native")
          }
          if(j == dy_i[length(dy_i)]) {
            grid.segments(df_s[i, 1], df_s[i, 2], df_s[i, 1], yP[2],
              gp=gpar(lwd=0.25), default.units="native")
          }
        }
      }
    } 
  }
}
## Dolomite pattern
dolP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.025
  yExp <- 0.25 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2])
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 6)
      grid.segments(df_s[1, 1] - xExp, df_s[1, 2], df_s[nrow(df_s), 1], df_s[nrow(df_s), 2],
        gp=gpar(lwd=0.25), default.units="native")
      if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i, 1] - xExp, df_s[i, 2] - yExp,
            gp=gpar(lwd=0.25), default.units="native")
        }  
      }else{
        for(i in s_e) {
          grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1] - xExp, 
            df_s[i+3, 2] - ifelse(j != dy_i[1], yExp, 0),
            gp=gpar(lwd=0.25), default.units="native")
          if(j == dy_i[1]) {
            grid.segments(df_s[i+3, 1] + (xExp /2), df_s[i+3, 2], df_s[i+3, 1], yP[1],
            gp=gpar(lwd=0.25), default.units="native")
          }
          if(j == dy_i[length(dy_i)]) {
            grid.segments(df_s[i, 1] - (xExp /2), df_s[i, 2], df_s[i, 1], yP[2],
              gp=gpar(lwd=0.25), default.units="native")
          }
        }
      }
    } 
  }
} 
## Coal pattern
coalP <- function(xP, yP, from, to, fill, sc.fac, ...) {
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.15), 
    default.units="native")
}
## chalk pattern
chalkP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.035
  yExp <- 0.2 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]  
      if(!is.even(which(j == dy_i))){
        s_e <- seq(1, nrow(df_s), 3)
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+2, 1], df_s[i+2, 2], 
            default.units="native", gp=gpar(lwd=0.25))
          grid.segments(df_s[i+1, 1] - xExp/2, df_s[i+1, 2], 
            df_s[i+1, 1] - xExp/2, df_s[i+1, 2] + yExp/2,
            gp=gpar(lwd=0.25), default.units="native")
        }
        if(nrow(df_s) - max(s_e) == 1){
          grid.segments(df_s[max(s_e), 1], df_s[max(s_e), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
      }else{
        s_o <- seq(2, nrow(df_s), 3)
        for(i in s_o) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+2, 1], df_s[i+2, 2], 
            gp=gpar(lwd=0.25), default.units="native")
          grid.segments(df_s[i+1, 1] + xExp/2, df_s[i+1, 2], 
            df_s[i+1, 1] + xExp/2, df_s[i+1, 2] + yExp/2,
            gp=gpar(lwd=0.25), default.units="native")
        }
        if(nrow(df_s) - max(s_o) == 1){
          grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.25), default.units="native")
        }
      }
    }
  }
}  
## Phosphorite pattern
phoP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.011
  yExp <- 0.25 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 6)
        for(i in s_e) {
        grid.points(df_s[i+1, 1], df_s[i+1, 2], 
          default.units="native", size = unit(1.8, "mm"), 
          pch=16)
      }
    }
  }
}  
## Diatomite pattern
diatP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.021
  yExp <- 0.25 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 12)
      if(!is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i+2, 1], df_s[i+2, 2] + yExp/6.5, 
            df_s[i+6, 1], df_s[i+6, 2] + yExp/6.5, 
            gp=gpar(lwd=0.25), default.units="native")
          grid.segments(df_s[i+2, 1], df_s[i+2, 2] - yExp/6.5, 
            df_s[i+6, 1], df_s[i+6, 2] - yExp/6.5,
            gp=gpar(lwd=0.25), default.units="native")
          grid.circle(df_s[i+4, 1], df_s[i+4, 2], r=unit(0.085, "cm"),
            default.units="native", gp=gpar(fill="beige", lwd=0.25))
        }
      }else{
        for(i in s_e) {
          grid.segments(df_s[i+8, 1], df_s[i+8, 2] + yExp/6.5, 
            df_s[i+12, 1], df_s[i+12, 2] + yExp/6.5, 
            gp=gpar(lwd=0.25), default.units="native")
          grid.segments(df_s[i+8, 1], df_s[i+8, 2] - yExp/6.5, 
            df_s[i+12, 1], df_s[i+12, 2] - yExp/6.5,
            gp=gpar(lwd=0.25), default.units="native")
          grid.circle(df_s[i+10, 1], df_s[i+10, 2], r=unit(0.085, "cm"),
            default.units="native", gp=gpar(fill="beige", lwd=0.25))
        }
      } 
    }
  }
}  
## tuff pattern
tufP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = 4, 
      size = unit(0.02, "native"), gp=gpar(lwd=0.4))
  }
}  
## Igneous pattern
ignP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = 3, 
      size = unit(0.025, "native"), gp=gpar(lwd=0.4))
  }
}
## Bentonite pattern
benP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid + 0.011, xypP$yGrid, pch = "<", 
      size = unit(0.0025 * pSize, "native"), gp=gpar(lwd=0.1))
    grid.points(xypP$xGrid - 0.013, xypP$yGrid, pch = "-----", 
      size = unit(0.035 * pSize, "native"), gp=gpar(lwd=0.3))
  }
}
## Halite pattern
haliteP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.025
  yExp <- 0.21 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    dx_i <- unique(df[,1])
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 6)
        grid.segments(0, df_s[1, 2], df_s[nrow(df_s), 1], df_s[nrow(df_s), 2],
          gp=gpar(lwd=0.25), default.units="native")
    }
    max_dy <- max(df$y)
    min_dy <- min(df$y)
    for(j in dx_i) {
      df_s <- df[which(df$x == j),]
        grid.segments(df_s[1, 1], ifelse(max(df_s[, 2]) == max_dy, yP[2], max(df_s[, 2])), 
          df_s[1, 1], ifelse(min(df_s[, 2]) == min_dy, yP[1], min(df_s[, 2])),
          gp=gpar(lwd=0.25), default.units="native")
    }
  }
}  
## Limonite pattern
limoP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = 1, 
      size = unit(0.04 * pSize, "native"), gp=gpar(lwd=0.4))
    grid.points(xypP$xGrid, xypP$yGrid, pch = 0, 
      size = unit(0.015 * pSize, "native"), gp=gpar(lwd=0.4))
  }
}  
## Siderite pattern
sidP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = 10, 
      size = unit(0.035 * pSize, "native"), gp=gpar(lwd=0.4))
  }
}  
## Chert pattern
chertP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = 6, 
      size = unit(0.03 * pSize, "native"), gp=gpar(lwd=0.2))
  }
}  
## marl pattern
marlP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = "~", 
      size = unit(0.35 * pSize, "native"), gp=gpar(lwd=0.3, cex=1.25))
  }
}  
## Gypsum pattern
gypP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = ">", 
      size = unit(0.04 * pSize, "native"), gp=gpar(lwd=0.4))
  }
}
## Lapillistone
lapP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1, ...) { 
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
  if(isTRUE(fill.pattern)) {
    grid.points(xypP$xGrid, xypP$yGrid, pch = "^", 
      size = unit(0.04 * pSize, "native"), gp=gpar(lwd=0.4))
  }
}  
## Basalt pattern
basP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.0125
  yExp <- 0.21 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2])
    if(yP[2] - max(dy_i) < yExp) {
      dy_i <- dy_i[-length(dy_i)]
    }
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 12)
      if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i+7, 1], df_s[i+7, 2], df_s[i+7, 1] + xExp, 
            df_s[i+7, 2] + yExp/1.5,  gp=gpar(lwd=0.25), default.units="native")
          grid.segments(df_s[i+7, 1], df_s[i+7, 2], df_s[i+7, 1] - xExp, 
            df_s[i+7, 2] + yExp/1.5,  gp=gpar(lwd=0.25), default.units="native")
        }
      }else{
        for(i in s_e) {
          grid.segments(df_s[i+1, 1], df_s[i+1, 2], df_s[i+1, 1] + xExp, 
            df_s[i+1, 2] + yExp/1.5,  gp=gpar(lwd=0.25), default.units="native")
          grid.segments(df_s[i+1, 1], df_s[i+1, 2], df_s[i+1, 1] - xExp, 
            df_s[i+1, 2] + yExp/1.5,  gp=gpar(lwd=0.25), default.units="native")
        }
      }
    }
  }
}  
## Metamorphic pattern
metaP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize=1) {
  xExp <- 0.035 
  yExp <- 0.275 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[4]], lsPt[[2]], lsPt[[3]])
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    if(nrow(grnT) != 0) {
      grid.points(grnT$x, grnT$y, default.units="native", 
      size = unit(0.3 * pSize, "mm"), pch=16)
    } 
    if(nrow(lsPt[[5]]) != 0) {
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
        lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
        default.units="native", size = unit(0.3 * pSize, "mm"), pch=16)
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
        lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
        default.units="native", size = unit(0.3 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[1]]) != 0) {
      ypM <- lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      xpM <- lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      grid.points(xpM, ypM,
        default.units="native", size = unit(0.3 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.025, 
        unit(ypM, "native") + unit(0.175, "mm"),
        default.units="native", size = unit(0.3 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.05,  
        unit(ypM, "native") - unit(0.35, "mm"),
        default.units="native", size = unit(0.3 * pSize, "mm"), pch=16)
    }
    xExp <- 0.02
    yExp <- 0.05 * sc.fac
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    if(yP[1] - min(dy_i) < yExp/2) {
      dy_i <- dy_i[-1]
    }
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 9)
      if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i+6, 1], df_s[i+6, 2], df_s[i+6, 1] - xExp*2,
            df_s[i+6, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
          grid.segments(df_s[i+6, 1], df_s[i+6, 2], df_s[i+6, 1] + xExp*2,
            df_s[i+6, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
          grid.segments(df_s[i+8, 1], df_s[i+8, 2], df_s[i+8, 1] - xExp*2, 
            df_s[i+8, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
          grid.segments(df_s[i+8, 1], df_s[i+8, 2], df_s[i+8, 1] + xExp*2, 
            df_s[i+8, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
        }
      }else{
        for(i in s_e) {
          grid.segments(df_s[i+1, 1], df_s[i+1, 2], df_s[i+1, 1] - xExp*2, 
            df_s[i+1, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
          grid.segments(df_s[i+1, 1], df_s[i+1, 2], df_s[i+1, 1] + xExp*2, 
            df_s[i+1, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
          grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1] - xExp*2,
            df_s[i+3, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
          grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1] + xExp*2,
            df_s[i+3, 2] - yExp*4, gp=gpar(lwd=1), default.units="native")
        }
      }
    }
  }
}
## Interbedded Sandstone and Shale Pattern
int_sand_shaleP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.025
  yExp <- 0.2 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2])
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      df_sbp <- df[which(df$y == dy_i[which(j == dy_i) - 1]),]
      s_e <- seq(1, nrow(df_s), 2)
      s_o <- seq(2, nrow(df_s), 3)
      if(is.even(which(j == dy_i))){
        grid.polygon(x=c(0, 0, ifelse(nrow(df_sbp) > 0, max(df_sbp[, 1]), max(df_s[, 1])), 
          max(df_s[, 1])), 
          y=c(df_s[nrow(df_s), 2], df_s[nrow(df_s), 2] - yExp, 
          max(df_s[, 2]) - yExp, max(df_s[, 2])),
          gp=gpar(fill="grey70", lwd=0.05), default.units="native")
          for(i in s_o) {
            grid.segments(df_s[i, 1], df_s[i, 2] - yExp/2, df_s[i+2, 1], df_s[i, 2] - yExp/2,
              gp=gpar(lwd=0.25), default.units="native")
          }
          if(nrow(df_s) - max(s_o) == 1){
            grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2] - yExp/2, 
              df_s[nrow(df_s), 1], df_s[nrow(df_s), 2] - yExp/2, 
              gp=gpar(lwd=0.25), default.units="native")
          }
        if(j == max(dy_i) & abs(max(dy_i) - yP[2]) < yExp) {
          grid.polygon(x=c(0, 0, max(df_s[, 1]), max(df_s[, 1])), 
            y=c(df_s[nrow(df_s), 2], yP[2], yP[2], max(df_s[, 2])),
            gp=gpar(fill="grey70", lwd=0.05), default.units="native")
        }
      }else{
        if(abs(j - yP[1]) > yExp/2) {
        for(i in s_e) {
          grid.points(df_s[i, 1], df_s[i, 2] - yExp /2, 
            size = unit(0.5, "mm"), pch=16, default.units="native")
        } 
      }
      if(j == max(dy_i) & abs(max(dy_i) - yP[2]) < yExp) {
          grid.polygon(x=c(0, 0, max(df_s[, 1]), max(df_s[, 1])), 
            y=c(df_s[nrow(df_s), 2], yP[2], yP[2], max(df_s[, 2])),
            gp=gpar(fill="grey70", lwd=0.05), default.units="native")
          for(i in s_o) {
         grid.segments(df_s[i, 1], df_s[i, 2] + abs(df_s[i, 2] - yP[2])/2, df_s[i+2, 1], 
          df_s[i, 2] + abs(df_s[i, 2] - yP[2])/2,
              gp=gpar(lwd=0.25), default.units="native")
          }
        }

      }
    }
  }
}
## Interbedded Limestone and Shale Pattern
int_lime_shaleP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, ...) {
  xExp <- 0.025
  yExp <- 0.2 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill="grey40", lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2])
    d <- seq_along(dy_i)
    if(length(d) > 1){
      dx <- dy_i[seq(2, max(d), by=4)]
    }
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      df_sbp <- df[which(df$y == dy_i[which(j == dy_i) - 1]),]
      s_e <- seq(1, nrow(df_s), 6)
      if(is.even(which(j == dy_i))){
        grid.polygon(x=c(0, 0, ifelse(nrow(df_sbp) > 0, max(df_sbp[, 1]), max(df_s[, 1])), 
          max(df_s[, 1])), 
          y=c(df_s[nrow(df_s), 2], df_s[nrow(df_s), 2] - yExp, 
          max(df_s[, 2]) - yExp, max(df_s[, 2])),
          gp=gpar(fill="skyblue1", lwd=0.05), default.units="native")
        if(j %in% dx){
          for(i in s_e) {
            grid.segments(df_s[i, 1], df_s[i, 2], df_s[i, 1], df_s[i, 2] - yExp,
              gp=gpar(lwd=0.25), default.units="native")
          }
        }else{
          for(i in s_e) {
            grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1], df_s[i+3, 2] - yExp,
              gp=gpar(lwd=0.25), default.units="native")
          }
        }  
      }else{
        if(abs(j - yP[1]) > yExp/2) {
          s_o <- seq(2, nrow(df_s), 3)
          for(i in s_o) {
            grid.segments(df_s[i, 1], df_s[i, 2] - yExp/2, df_s[i+2, 1], df_s[i, 2] - yExp/2,
              gp=gpar(lwd=0.25), default.units="native")
          }
          if(nrow(df_s) - max(s_o) == 1){
            grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2] - yExp/2, 
              df_s[nrow(df_s), 1], df_s[nrow(df_s), 2] - yExp/2, 
              gp=gpar(lwd=0.25), default.units="native")
          }
        }
        if(j == max(dy_i) & abs(max(dy_i) - yP[2]) < yExp) {
          grid.polygon(x=c(0, 0, max(df_s[, 1]), max(df_s[, 1])), 
            y=c(df_s[nrow(df_s), 2], yP[2], yP[2], max(df_s[, 2])),
            gp=gpar(fill="skyblue1", lwd=0.05), default.units="native")
          for(i in s_e) {
          grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1], yP[2],
            gp=gpar(lwd=0.25), default.units="native")
          }
        }
      }
      grid.segments(0, df_s[1, 2], df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
        gp=gpar(lwd=0.25), default.units="native")
    }
  }
}
##
## Calcareous Sandstone pattern
m_sand_limeP <- function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize) {
  xExp <- 0.05 
  yExp <- 0.42 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[4]], lsPt[[2]], lsPt[[3]])
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    if(nrow(grnT) != 0) {
      grid.points(grnT$x, grnT$y, default.units="native", 
      size = unit(0.4 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[5]]) != 0) {
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
      lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
      default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
      lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
      default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[1]]) != 0) {
      ypM <- lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      xpM <- lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      grid.points(xpM, ypM,
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.025, 
        unit(ypM, "native") + unit(0.175, "mm"),
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.05,  
        unit(ypM, "native") - unit(0.35, "mm"),
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
    }
  }
  xExp <- 0.04
  yExp <- 0.35 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2]) 
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]  
      if(!is.even(which(j == dy_i))){
        s_e <- seq(1, nrow(df_s), 4)
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+3, 1], df_s[i+3, 2], 
            default.units="native", gp=gpar(lwd=0.35))
          grid.segments(df_s[i+1, 1] - xExp/2, df_s[i+1, 2], 
            df_s[i+1, 1] - xExp/2, df_s[i+1, 2] + yExp/2.5,
            gp=gpar(lwd=0.45), default.units="native")
        }
        if(nrow(df_s) - max(s_e) == 1){
          grid.segments(df_s[max(s_e), 1], df_s[max(s_e), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.35), default.units="native")
        }
      }else{
        s_o <- seq(2, nrow(df_s), 4)
        for(i in s_o) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i+3, 1], df_s[i+3, 2], 
            gp=gpar(lwd=0.35), default.units="native")
          grid.segments(df_s[i, 1] + xExp/2, df_s[i, 2], 
            df_s[i, 1] + xExp/2, df_s[i, 2] + yExp/2.5,
            gp=gpar(lwd=0.45), default.units="native")
        }
        if(nrow(df_s) - max(s_o) == 1){
          grid.segments(df_s[max(s_o), 1], df_s[max(s_o), 2], 
            df_s[nrow(df_s), 1], df_s[nrow(df_s), 2], 
            gp=gpar(lwd=0.35), default.units="native")
        }
      }
    }
  }
}  
## Sandy Limestone pattern
m_lime_sandP <-  function(xP, yP, from, to, fill, sc.fac, fill.pattern, pSize, ...) {
  xExp <- 0.025
  yExp <- 0.25 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP)
  grid.path(xP, yP, gp=gpar(fill=fill, lwd=0.05), default.units="native")
  if(isTRUE(fill.pattern)) {
    df <- as.data.frame(lsPt[[1]])
    dy_i <- unique(df[,2])
    for(j in dy_i) {
      df_s <- df[which(df$y == j),]
      s_e <- seq(1, nrow(df_s), 6)
      grid.segments(df_s[1, 1] - xExp, df_s[1, 2], 
        df_s[nrow(df_s), 1], df_s[nrow(df_s), 2],
        gp=gpar(lwd=0.25), default.units="native")
        if(is.even(which(j == dy_i))){
        for(i in s_e) {
          grid.segments(df_s[i, 1], df_s[i, 2], df_s[i, 1], df_s[i, 2] - yExp,
            gp=gpar(lwd=0.25), default.units="native")
        }  
      }else{
        for(i in s_e) {
          grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1], 
            df_s[i+3, 2] - ifelse(j != dy_i[1], yExp, 0),
            gp=gpar(lwd=0.25), default.units="native")
          if(j == dy_i[1]) {
            grid.segments(df_s[i+3, 1], df_s[i+3, 2], df_s[i+3, 1], yP[1],
            gp=gpar(lwd=0.25), default.units="native")
          }
          if(j == dy_i[length(dy_i)]) {
            grid.segments(df_s[i, 1], df_s[i, 2], df_s[i, 1], yP[2],
              gp=gpar(lwd=0.25), default.units="native")
          }
        }
      }
    } 
  xExp <- 0.05 
  yExp <- 0.5 * sc.fac
  lsPt <- gridPts(xExp, yExp, from, to, xP, yP) 
  grnT <- rbind(lsPt[[4]], lsPt[[2]], lsPt[[3]])
  if(isTRUE(fill.pattern)) {
    if(nrow(grnT) != 0) {
      grid.points(grnT$x, grnT$y, default.units="native", 
      size = unit(0.4 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[5]]) != 0) {
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 == 0], 
      lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 == 0],
      default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(lsPt[[5]]$x[as.numeric(row.names(lsPt[[5]])) %% 2 != 0], 
      lsPt[[5]]$y[as.numeric(row.names(lsPt[[5]])) %% 2 != 0],
      default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
    }
    if(nrow(lsPt[[1]]) != 0) {
      ypM <- lsPt[[1]]$y[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      xpM <- lsPt[[1]]$x[as.numeric(row.names(lsPt[[1]])) %% 2 != 0]
      grid.points(xpM, ypM,
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.025, 
        unit(ypM, "native") + unit(0.175, "mm"),
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
      grid.points(xpM - 0.05,  
        unit(ypM, "native") - unit(0.35, "mm"),
        default.units="native", size = unit(0.4 * pSize, "mm"), pch=16)
    }
  }
  }
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