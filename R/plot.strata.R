############################################################################
#  File library/SDAR/R/plot.strata.R			                           #
#									                                       #
#  Copyright (C) 					                                       # 
#									                                       #
#  This program is free software; you can redistribute it and/or modify	   #
#  it under the terms of the GNU General Public License as published by	   #
#  the Free Software Foundation; either version 2 of the License, or	   #
#  (at your option) any later version.						               #					
#										                                   #
#  This program is distributed in the hope that it will be useful,		   #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of		   #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 		   #
#  GNU General Public License for more details.					           #
#										                                   #
#  A copy of the GNU General Public License is available at			       #
#  http://www.r-project.org/Licenses/						               #
#										                                   #
############################################################################

plot.strata <- function(
  # Mandatory parameters to draw a stratigraphic log
  x,
  datum = "top",
  data.units = "feet",
  d.scale = 100,
  d.barscale = 2,
  lithology = TRUE,
  bed.number = TRUE,
  file.name = "SDAR_graphical_output",
  # Optional
  GR.log = NULL,
  ncore = NULL,
  samples = NULL,
  oil.stain = NULL,
  sed.structures = NULL,
  fossils = NULL,
  tracefossils = NULL,
  other.sym = NULL,
  bioturbation = NULL,
  lithostrat = NULL,
  d.legend = TRUE,
  metadata = NULL,
  main = NULL,
  sub = NULL,
  subset.base = NULL,
  subset.top = NULL, 
  plot.order = NULL, 
  d.color = 0,
  bar.type = 0,
  symbols.size = 1,
  xlim.GR = c(0, 300),
  family = "serif",
  fontsize = 10,
  cex.main = 1,
  ...){ # dots for generic compatibility

  # Data and parameters validation
  if(!datum %in% c("base", "top")) {  # Validation for datum
    stop(call.=FALSE, "datum should be 'base' or 'top'. 'base' when thickness are measured up from the bottom (e.g. stratigraphic section); 'top' when depths are measured from the surface (e.g. core)")
  }
  if(datum == "top" & any(x$from < x$to) & all(x$from >= 0)) {
      stop(call.=FALSE, paste("In", substitute(x), "object 'top' is grether than 'base', in this case datum should be 'base'."))
  }
  if(datum == "base" & any(x$from < x$to) & all(x$from <= 0)) {
    stop(call.=FALSE, paste("In", substitute(x), "object 'base' is grether than 'top', in this case datum should be 'top'."))
  }
  if(!data.units %in% c("feet", "meters")) {  # Validation for units.data 
    stop(call. = FALSE, 'units.data should be "feet" or "meters"')
  }
  if(!is.numeric(d.scale)) {  # Validation for d.scale
    stop(call. = FALSE, "d.scale should be numeric.")
  }
  if(!is.numeric(d.barscale)) {  # Validation for d.barscale
    stop(call. = FALSE, "d.barscale should be numeric.")
  }
  if(!is.logical(lithology)) {  # Validation for lithology
    stop(call. = FALSE, 'bed.number need to be a logical value, TRUE or FALSE')
  }
  if(!is.logical(bed.number)) {  # Validation for bed.number
    stop(call. = FALSE, 'bed.number need to be a logical value, TRUE or FALSE')
  }
  if(is.null(subset.base)){
    subset.base <- min(x$from)
  }else{
  	if(datum=="top") {
  	  subset.base <- subset.base * -1	
  	}
  }
  if(is.null(subset.top)){
    subset.top <- max(x$to)
  }else{
  	if(datum=="top"){
  	  subset.top <- subset.top * -1
  	}
  }
  subset.lim <- events(from=subset.base, to=subset.top) # limits for interval subset
  # Subset strata class
  x <- crop_events(x, subset.lim) 
  nx <- data.frame(n.view= 1, from= subset.base, 
	    to = subset.top, sc.cover=0, 
	    thick= subset.top - subset.base)
  if(bed.number == TRUE) {
    if(all(is.na(x$bed_number)) == TRUE) {
	  stop(call.=FALSE, "Bed number is empty, this is a mandatory field")
    }
    if(any(duplicated(x$bed_number) == TRUE)) {
      stop(call.=FALSE, capture.output(cat("bed number must to be unique for each bed, rows number", 
				       head(rownames(x[duplicated(x$bed_number),]), 10), 
				       "contains duplicated values")))
    }
  }
  # to maintain compatibility with SDAR database (GUI, version),
  # two columns named "grain_size_base" and "grain_size_top".
  if("grain_size_base" %ni% colnames(x)) {
    x$grain_size_base <- x[,"grain_size"]
  }
  if("grain_size_top" %ni% colnames(x)) {
    x$grain_size_top <- x[, "grain_size"]
  }
  x$gz_xAxis <- gs.table[match(x[, "grain_size"], 
    gs.table[, "name"]), "xAxis"]
  if("grain_size_base" %in% colnames(x)) {
    x$base_xAxis2 <- gs.table[match(x[, "grain_size_base"], 
      gs.table[, "name"]), "xAxis"]
  }
  if("grain_size_top" %in% colnames(x)) {
    x$top_xAxis2 <- gs.table[match(x[, "grain_size_top"], 
      gs.table[, "name"]), "xAxis"]
  }
  # bed color
  if(lithology == TRUE) {
    if(d.color == 0) {
      x$color <- litho.table[with(x, factor(prim_litho, 
                 levels=litho.table$name)), "def_col"] # Replace color for default sdar colors
    }
    if(d.color == 3) {        # if the user select white and black option
      x$color <- rep("transparent", nrow(x))
    }
  }  
  # Input data validation
  if(!is.null(GR.log)) {
    input.val.log(GR.log, col.na="GR")
    if(datum == "top") {
  	  GR.log$Depth <- GR.log$Depth * -1
    }
    GR.log$Depth2 <- GR.log$Depth
    event.GR.log <- as_events(GR.log, from.col="Depth", to.col="Depth2")
    sub.GR.log <- crop_events(event.GR.log, subset.lim)
  }
  if(!is.null(ncore)) {
  	if(datum == "base"){
	  stop(call.=FALSE, "ncore applies only for wells when datum is set to 'top'")
  	}
	input.val(ncore, "core_number")
	  sub.ncore <- int.event(ncore, datum, subset.lim, "ncore")
	}
  if(!is.null(samples)) {
    input.val(samples, "type")
    check.samples <- input.check(samples, "type", samples.table)
    sub.samples <- int.event(check.samples, datum, subset.lim, "samples")
    sub.samples$pch <-  samples.table[match(sub.samples[["type"]], 
  	  samples.table[, "id"]), "pch"]
    sub.samples$color <-  samples.table[match(sub.samples[["type"]], 
  	  samples.table[, "id"]), "color"]
  }
  if(!is.null(oil.stain)) {
    input.val(oil.stain, "intensity")
    check.oil.stain <- input.check(oil.stain, "intensity", stain.table)
    sub.oil.stain <- int.event(check.oil.stain, datum, subset.lim, "oil.stain")
    sub.oil.stain$color <-  stain.table[match(sub.oil.stain[["intensity"]], 
  	  stain.table[, "id"]), "color"]
  }
  if(!is.null(sed.structures)) {
    input.val(sed.structures, "sed_structure")
    check.sed.structures <- input.check(sed.structures, "sed_structure", sed.struc.table)
    sub.sed.structures <- int.event(check.sed.structures, datum, subset.lim, "sed.structures")
  	leg.sed <- unique(sub.sed.structures$sed_structure)
  }
  if(!is.null(fossils)) {
    input.val(fossils, "fossil")
    check.fossils <- input.check(fossils, "fossil", fossil.table)
    sub.fossils <- int.event(check.fossils, datum, subset.lim, "fossils")
  	leg.fos <- unique(sub.fossils$fossil)
  }
  if(!is.null(tracefossils)) {
    input.val(tracefossils, "tracefossil")
    check.tracefossils <- input.check(tracefossils, "tracefossil", tracefossil.table)
    sub.tracefossils <- int.event(check.tracefossils, datum, subset.lim, "tracefossils")
    leg.trac <- unique(sub.tracefossils$tracefossil)
  }
  if(!is.null(other.sym)) {
    input.val(other.sym, "other_symbols")
    check.other.sym <- input.check(other.sym, "other_symbols", others.table)
    sub.other.sym <- int.event(check.other.sym, datum, subset.lim, "other.sym")
    leg.other <- unique(sub.other.sym$others)
  }
  if(!is.null(lithostrat)) {
    input.val(lithostrat, "litho_unit_type")
    check.lithostrat <- input.check(lithostrat, "litho_unit_type", litho.unit.table)
    sub.lithostrat <- int.event(check.lithostrat, datum, subset.lim, "lithostrat")
  }
  if(!is.null(bioturbation)) {
    input.val(bioturbation, "index")
  	val.bioturbation <- input.check(bioturbation, "index", biotur.table)
    val.bioturbation$percent <-  biotur.table[match(val.bioturbation[["index"]], 
  	  biotur.table[, "id"]), "percent"]
    sub.bioturbation <- int.event(val.bioturbation, datum, subset.lim, "bioturbation")
  }
  # Define drawing parameters
  sc.fac <- d.scale/100  # Define scale factor, equal to 1:100
  if(data.units == "feet") {
    sc.fac <- (d.scale/100) / 0.3048
  }
  if(d.legend == TRUE) {
  	h.litho <- 0
    if(lithology == TRUE) {
      leg.litho <- unique(na.omit(x[,c("prim_litho", "color")]))
      rows_leg <- sum(nrow(leg.litho), if("covered" %in% x$rock_type) 3)
      if(rows_leg > 9) {
        h.litho <- (rows_leg - 9) * 0.75
      }
    }  
    h.sed <- 0
    if(!is.null(sed.structures)) {
      if(length(leg.sed) > 13) {
        h.sed <- (length(leg.sed) - 13) * 0.5
      }
    }
    h.fos <- 0
    if(!is.null(fossils)) {
      if(length(leg.fos) > 13) {
        h.fos <- (length(leg.fos) - 13) * 0.5
      }
    }
    h.trac <- 0
    if(!is.null(tracefossils)) {
      if(length(leg.trac) > 13) {
        h.trac <- (length(leg.trac) - 13) * 0.5
      }
    }
    h.other <- 0
    if(!is.null(other.sym)) {
      if(length(leg.other) > 13) {
        h.other <- (length(leg.other) - 13) * 0.5
      }
    }
    h.legend <- 8 + max(h.litho, h.sed, h.fos, h.trac, h.other) + 0.2 
  }
  ## Design of margins, width and height of panels for the output frame (Measurement are in cm).
  if(is.null(plot.order)) {
    arrng <- c(if(!is.null(lithostrat)) "d.lithostrat",
  	           if(!is.null(ncore)) "d.ncore",
  	           if(!is.null(GR.log)) "d.GR.log",
               if(!is.null(samples)) "d.samples",
               if(bed.number == TRUE) "d.bed.number",
               if(!is.null(d.barscale)) "d.graphic_bar",
               if(lithology == TRUE) "d.lithology",
               if(!is.null(sed.structures)) "d.sed.structures",
               if(!is.null(fossils)) "d.fossils",
               if(!is.null(tracefossils)) "d.tracefossils",
               if(!is.null(other.sym)) "d.other.sym",
               if(!is.null(oil.stain)) "d.oil.stain",
               if(!is.null(bioturbation)) "d.bioturbation")
  }else{ 
    arrng <- plot.order  # plot features arrangement, this vector is provide for the user
  }
  row.head <- 2
  # compute layout widths 
  width.layout <- width.table[order(match(width.table[, "arg"],  arrng)), 
                            "value"][1:length(arrng)]
  # Create objects for the features to be displayed with its frame witdh
  for (i in 1:length(arrng)) {
    assign(arrng[i], width.layout[i])
  }
  w.oma <-  width.table[width.table[,"name"] == "w.oma", "value"]
  tmp_width <- sum(width.layout, (w.oma *2))
  width_leg <- sum(5 + ifelse(lithology == TRUE, 5.5, 0),
                  ifelse(any(!is.null(c(bioturbation, oil.stain))), 5.5, 0), 
                  ifelse(!is.null(sed.structures), 5.5, 0),
			      ifelse(!is.null(fossils), 5.5, 0),
			      ifelse(!is.null(tracefossils), 5.5, 0),
			      ifelse(!is.null(other.sym), 5.5, 0))
if(d.legend == TRUE) {
  if(width_leg > tmp_width) {
    w.oma <- (width_leg - tmp_width) / 2 + w.oma
  }
}
  widths <- c(w.oma, width.layout, w.oma)
  heights <- c(3.5, 3.75, 1, rev(nx$thick / sc.fac), 1, 
               if(d.legend == TRUE) h.legend, 1)
  unitlay <- grid.layout(length(heights), length(widths),
        widths=unit(widths, "cm"), 
        heights=unit(heights, "cm"))
  pdf(paste(file.name, ".pdf", sep=""), 
    width=sum(widths) / 2.54 + 1, 
    height=sum(heights) / 2.54 + 1, 
    family=family, pointsize=10)  ## Open the PDF device

    pushViewport(viewport(layout=unitlay))  # Open Viewport "unitlay". It is the main design of viewports
    # Lithology
      if(lithology == TRUE) {
      draw.header.litho(which(arrng == "d.lithology") + 1, row.head) # draw the header of lithology
      open.viewport(1, poscol=which(arrng == "d.lithology") + 1, datum, iv=1, subset.lim)  # Openviewport to draw lithology
      draw.guidelines(subset.lim, c(0.33, 0.45, 0.63, 0.77, 0.91)) # draw dashed guidelines in the backgound
        lay.v <- x
        for(i in 1:nrow(lay.v)) {  # Loop to draw each stratum acording of thickness, grainSize and others parameters. 
        # Move points to shape the graing size
        nodesComp <- c(0, 0.0095, 0.0185, 0.0165, 0.0215, 0.0165, 0.0185, 0.0095, 0)  # Points to shape competent lithologies
        nodesMud <- c(0, -0.0075, -0.0125, -0.0145, -0.0125, -0.0145, -0.0125, -0.0075, 0) 	# Points to shape soft lithologies
        movY <- c(1 , .93, .8, .65, .5, .35, .2, .07)  # Points to draw curved ending of the beds
        ## Create xP and yP points. This points are the position in the X and Y axis to draw each layer.
        ## The position of this points will be modified in further steps, to represent features as grading, contact type.
        yP <- c(lay.v$from[i], lay.v$to[i], 
              (lay.v$to[i] - lay.v$from[i]) *
	          movY + lay.v$from[i], lay.v$from[i])
        xP <- c(0,0, if(lay.v$prim_litho[i] %in% 
		      c(5:9, 14, 17:19, litho.table[c(5:9, 14, 17:19),2])) {
		      nodesComp + lay.v$gz_xAxis[i]
                     }else{
	                 nodesMud + lay.v$gz_xAxis[i]
                     })
        #  Grading
        if(!is.na(lay.v$grain_size_base[i])) {
          xP[11] <- lay.v$gz_xAxis[i] 	# Add the correspondent grain size at the base of the layer
          if(!is.na(lay.v$top_xAxis[i])) {
            xP[3] <- lay.v$top_xAxis[i]  	# Add the correspondent grain size at the top of the layer
        ## Shape bedding when there are two points "Grain_size_base" and "Grain_size_top"   
            shapePts <- rep(c(-0.004, 0.004), 4)
            if(lay.v$gz_xAxis[i] > lay.v$top_xAxis[i]) {
	          xP[4:10] <- cumsum(rep(abs(xP[3] - xP[11])/8, 7)) + xP[3]
	          xP[4:10] <- xP[4:10] + shapePts[-1]
	        }
            if(lay.v$gz_xAxis[i] < lay.v$top_xAxis[i]) {	    
 	          xP[4:10] <- rev(cumsum(rep(abs(xP[3] - xP[11])/8, 7))) + xP[11] 
	          xP[4:10] <-  xP[4:10] + shapePts[-1]
	        } 
          }
        }	
#### Draw the lithologic patterns according to the functions included in "patterns.R" file.
 	   
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[1,]) { 
          clayP(xP, yP, lay.v$from[i], lay.v$to[i],
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[2,]) { 
          mudP(xP, yP, lay.v$from[i], lay.v$to[i],  
          ifelse(d.color %in% c(1,2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[3,]) { 
          siltP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac, pSize=0.5)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[4,]) { 
          shaleP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[5,]) { 
          sandP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac, pSize = 0.4)
        }   
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[6,]) { 
          congP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac, pSize=0.105)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[7,]) { 
          brecP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]),
          sc.fac, pSize=0.39)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[8,]) { 
          limeP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[9,]) { 
          dolP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]), 
          sc.fac)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[13,]) { 
          coalP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]))
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[26,]) { 
          ignP(xP, yP, lay.v$from[i], lay.v$to[i], 
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]),
          sc.fac)
        }
        if(!is.na(lay.v$prim_litho[i]) & lay.v$prim_litho[i] %in% litho.table[24,]) { 
          pyroP(xP, yP, lay.v$from[i], lay.v$to[i],
          ifelse(d.color %in% c(1, 2), eval(parse(text=lay.v$color[i])),lay.v$color[i]),
          sc.fac)
        } 
        ##  To draw cover segments included in the same viewport
        if(lay.v$rock_type[i] == "covered") {
          draw.cover(lay.v$from[i], lay.v$to[i], 0.19)
        }
      }  #  Temporal close loop open to draw each strata.
      upViewport()  # Close Viewport opened with function open.viewport
    }
    ##  barscale
    if(!is.null(d.barscale)) {      
      u.meas <- ifelse(data.units == "meters", '(meters)', '(feet)')
      unit.label <- ifelse(datum == "base", 'Thickness', 'Depth')
      rem.from <- subset.base %% d.barscale
      over.to <- subset.top %% d.barscale
      over.from <- d.barscale - rem.from
      pt.int <- subset.base + over.from
      pt.end <- subset.top - over.to
      mark.lab <- data.frame(from = c(subset.base, seq(pt.int, pt.end, d.barscale)))
      mark.lab$to <- c(seq(pt.int, pt.end, d.barscale), subset.top)
      mark.lab$color <- rep(c("black", "white"), 
                         nrow(mark.lab))[1:nrow(mark.lab)]
      drawHeader(which(arrng == "d.graphic_bar") + 1, row.head, 
        paste(unit.label, "\n", u.meas) , 90, 1.1, 2)
      open.viewport(1, poscol=which(arrng == "d.graphic_bar") + 1, datum, iv=1, subset.lim)
          grid.rect(x=0.5, y=0.5, width=1, 
          height=1, gp=gpar(lwd=0.1, col="grey", lty=2))
        if(nrow(mark.lab) != 0) { 
          grid.text(abs(c(mark.lab$from, subset.top)), 
	          x=0.75, y=c(mark.lab$from, subset.top),
	          just = "right", default.units = 'native', 
	          check.overlap = TRUE, gp=gpar(cex=0.8))
          grid.segments(x0=0.8, y0=c(mark.lab$from, subset.top),
                        x1=0.965, y1=c(mark.lab$from, subset.top),
            default.units="native",
            gp=gpar(lwd=1))
          if(bar.type == 1) {
            grid.lines(x=c(0.965, 0.965), y=subset.lim,
              default.units="native",
              gp=gpar(lwd=1.5))
            grid.segments(x0=0.85, y0=mark.lab$from[-c(1:2)] - d.barscale/2,
                        x1=0.965, y1=mark.lab$from[-c(1:2)] - d.barscale/2,
              default.units="native",
              gp=gpar(lwd=0.75))
          }else{
            grid.rect(x=0.85, y=mark.lab$from, 
              width=0.12, height=mark.lab$to - mark.lab$from, 
              just=c("left", "bottom"), default.units = 'native', 
              gp=gpar(fill=mark.lab$color, lwd=0.5))
          }       
        }
      upViewport()
    }
    #  Bed number
    if(bed.number != "FALSE") {			
      drawHeader(which(arrng == "d.bed.number") + 1, row.head, "Bed number", 90, 1, 2)
      open.viewport(1, poscol=which(arrng == "d.bed.number") + 1, datum, iv=1, subset.lim)
        if(nrow(x) != 0) {
          grid.text(x$bed_number,
	      x=0.75, y=(x$to - x$from)/2 + x$from,
	      just = "right", default.units = 'native', check.overlap = TRUE,
	      gp=gpar(cex=0.8))
        }
      upViewport()
    }
    #  Samples
    if(!is.null(samples)) {      
      drawHeader(which(arrng == "d.samples") + 1, row.head, "Samples", 90, 1, 2)
      open.viewport(1, poscol=which(arrng == "d.samples") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.samples) != 0) {
        grid.points(x=rep(0.2, nrow(sub.samples)), 
          y=(sub.samples[,"to"] - sub.samples[, "from"])/ 2 + sub.samples[, "from"], 
          pch=sub.samples$pch,
          size = unit(symbols.size * 6, "pt"),
          default.units = 'native', 
          gp=gpar(col=sub.samples$color))
        grid.rect(x=0.35, y=sub.samples$from,
          width = 0.01, height= sub.samples$to - sub.samples$from,
          just=c("center", "bottom"),
          gp=gpar(fill="black", col=NA),
          default.units = "native")
        grid.segments(x0=0.3, y0=sub.samples$from, x1=0.4, y1=sub.samples$from,  
          gp=gpar(col="black", lwd=1),
          default.units = "native")
        grid.segments(x0=0.3, y0=sub.samples$to, x1=0.4, y1=sub.samples$to,  
          gp=gpar(col="black", lwd=1),
          default.units = "native")
        if("label" %in% colnames(sub.samples)) {
          grid.text(sub.samples$label, x=0.475, 
      	    y=(sub.samples[,"to"] - sub.samples[, "from"])/ 2 + sub.samples[, "from"], 
  		    just="left", gp=gpar(cex=0.8), default.units="native")
          }
        }
      upViewport() 
    }
    # Core Number
    if(!is.null(ncore)) {      
      drawHeader(which(arrng == "d.ncore") + 1, row.head, "Core number", 90, 1, 2)
      open.viewport(1, poscol=which(arrng == "d.ncore") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.ncore) != 0) {
          grid.text(sub.ncore$n_core,
            x=0.5, y=(sub.ncore$to - sub.ncore$from)/2 + sub.ncore$from,
            just="center", default.units='native', check.overlap=TRUE,
            gp=gpar(cex=1.1, font=2))
          grid.rect(x=0.075, y=sub.ncore$from,
            width = 0.85, height= sub.ncore$to - sub.ncore$from,
            just=c("left", "bottom"), gp=gpar(lwd=0.5, col="black"),
            default.units="native")
        }
        grid.rect(x=0.5, y=0.5, width=1, 
          height=1, gp=gpar(lwd=0.1, col="grey", lty=2))
      upViewport()    
    }  
    # Lithostrat
    if(!is.null(lithostrat)) {      
      drawHeader(which(arrng == "d.lithostrat") + 1, row.head, "Lithostratigraphy", 90, 1, 2)
      open.viewport(1, poscol=which(arrng == "d.lithostrat") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.lithostrat) != 0) {
          grid.text(sub.lithostrat$name,
            x=0.5, y=(sub.lithostrat$to - sub.lithostrat$from)/2 + sub.lithostrat$from,
            just="center", default.units='native', check.overlap=TRUE,
            gp=gpar(cex=1.1, font=2), rot=90)
          grid.rect(x=0.075, y=sub.lithostrat$from,
            width = 0.85, height= sub.lithostrat$to - sub.lithostrat$from,
            just=c("left", "bottom"), gp=gpar(lwd=0.5, col="black"),
            default.units = "native")
        }
        grid.rect(x=0.5, y=0.5, width=1, 
          height=1, gp=gpar(lwd=0.1, col="grey", lty=2))
      upViewport()    
    }  
    #  Visual Oil
    if(!is.null(oil.stain)) {      
      drawHeader(which(arrng == "d.oil.stain") + 1, row.head, "Oil Stain", 90, 1, 2)
      open.viewport(1, poscol=which(arrng == "d.oil.stain") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.oil.stain) != 0) {
          grid.rect(x=0.075, y=sub.oil.stain$from,
            width = 0.85, just=c("left", "bottom"),
            height= sub.oil.stain$to - sub.oil.stain$from,
            gp=gpar(fill=sub.oil.stain$color, col=NA),
            default.units = "native")
        } 
        grid.rect(x=0.5, y=0.5, width=1, 
          height=1, gp=gpar(lwd=0.1, col="grey", lty=2))
      upViewport()    
    }     
    #  Bioturbation  
    if(!is.null(bioturbation)) {			
      drawHeader(which(arrng == "d.bioturbation") + 1, row.head, "Bioturbation\nIndex\n", 90, 1, 2)
      pushViewport(viewport(layout.pos.col=which(arrng == "d.bioturbation") + 1,
	    layout.pos.row=row.head))
        grid.text("(Taylor & Goldring, 1993)", 
      	  x= 0.75, y= 0.5, rot=90,
	      gp=gpar(cex=0.9, font=3, col="blue"))
        grid.text(c(1,2,3,4,5,6), x=seq(1/6, 1, 1/6) - 1/12, 
      	  y=unit(-0.8, "cm"), rot=0, 
  		  just="center", gp=gpar(cex=0.9)) 
      upViewport()
      open.viewport(1, poscol=which(arrng == "d.bioturbation") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.bioturbation) != 0) {    
      	  grid.rect(x=sub.bioturbation$percent - 1/6, y=sub.bioturbation$from,
            width = 1/6, just=c("left", "bottom"),
	        height= sub.bioturbation$to - sub.bioturbation$from,
	        gp=gpar(fill="grey75", col=NA),
            default.units = "native")
        }
        col=rep(c("black" , "grey"),4)
        lin= seq(0, (1/6) * 6, 1/6)
        for( i in 1:6) {
          grid.lines(x = c(lin[i], lin[i]),
            y = subset.lim, gp=gpar(col=col[i], 
            lwd=0.6, lty=2),
            default.units = "native")
        }
        grid.rect(x=0.5, y=0.5, width=1, 
          height=1, gp=gpar(lwd=0.1, col="grey", lty=2))
      upViewport()     
    }
    #  GR.log  
    if(!is.null(GR.log)) {			
      pushViewport(viewport(layout.pos.col=which(arrng == "d.GR.log") + 1,
        layout.pos.row=row.head))
        grid.rect(x=0.5, y=0.5, width=1, 
          height=1, gp=gpar(lwd=0.25))
        grid.text(c("Gamma Ray", "API Units"), x=0.5, y=c(0.5, 0.20), 
          just = "center", rot=0, 
          gp=gpar(cex=c(1.2, 0.9), font=c(2,1), col="forestgreen"))
        grid.text(xlim.GR, x=c(0.1,0.9), y=0.175, 
          just = "center", gp=gpar(cex=0.9, col="forestgreen"))
        grid.lines(x = c(0.1, 0.9), y = 0.1,
          arrow = arrow(angle = 30, length = unit(0.2, "cm"), ends = "last", 
          type = "open"), gp=gpar(col="forestgreen", lwd=1.2))
      upViewport()          # Close Viewport "gamma ray curve".
      open.viewport(1, poscol=which(arrng == "d.GR.log") + 1, datum, iv=1, subset.lim)
	    tvp <- viewport(layout.pos.col= 1, layout.pos.row=1, xscale = xlim.GR, 
	                    yscale = subset.lim)
        xlogcur <- well_logGrob(two_col_log=sub.GR.log, y.sc=subset.lim, vp=tvp, xlim.GR=xlim.GR)
        grid.draw(xlogcur)    
      upViewport()
    }
    #  Fossils
    if(!is.null(fossils)) {			
      drawHeader(which(arrng == "d.fossils") + 1, row.head, "Fossils", 90, 1.1, 2)
      open.viewport(1, poscol=which(arrng == "d.fossils") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.fossils) > 0) {
          for(i in 1:nrow(sub.fossils)) {
            tmp_file <-  paste(fossil.table[sub.fossils$fossil[i], "file_name"], ".svg", sep="")
            ind_x <- rep(c(0.25, 0.75), nrow(sub.fossils))
            ind_x <- ind_x[1:nrow(sub.fossils)]
            grid.symbols(SDAR.sym[["fos.sym"]][[tmp_file]], x=ind_x[i], 
              (sub.fossils[i, "to"] - sub.fossils[i, "from"])/ 2 + sub.fossils[i, "from"], 
              default.units = "native", size = unit(symbols.size * 17, "pt"))
          }
        }  
      upViewport()
    }
    #  Tracefossils
    if(!is.null(tracefossils)) {			
      drawHeader(which(arrng == "d.tracefossils") + 1, row.head, "Trace fossils", 90, 1.1, 2)
      open.viewport(1, poscol=which(arrng == "d.tracefossils") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.tracefossils) > 0) {
          for(i in 1:nrow(sub.tracefossils)) {
            tmp_file <-  paste(tracefossil.table[sub.tracefossils$tracefossil[i], "file_name"], ".svg", sep="")
            ind_x <- rep(c(0.25, 0.75), nrow(sub.tracefossils))
            ind_x <- ind_x[1:nrow(sub.tracefossils)]
            grid.symbols(SDAR.sym[["trac.sym"]][[tmp_file]], x=ind_x[i], 
              (sub.tracefossils[i, "to"] - sub.tracefossils[i, "from"])/ 2 + sub.tracefossils[i, "from"], 
              default.units = "native", size = unit(symbols.size * 17, "pt"))
          }
        }  
      upViewport()
    }
    # Sedimentary structures
    if(!is.null(sed.structures)) {			
      drawHeader(which(arrng == "d.sed.structures") + 1, row.head, "Sedimentary\n structures", 90, 1.1, 2)
      open.viewport(1, poscol=which(arrng == "d.sed.structures") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.sed.structures) > 0) {
          for(i in 1:nrow(sub.sed.structures)) {
            tmp_file <- paste(sed.struc.table[sub.sed.structures$sed_structure[i], "file_name"], ".svg", sep="")
            ind_x <- rep(c(0.25, 0.75), nrow(sub.sed.structures))
            ind_x <- ind_x[1:nrow(sub.sed.structures)]
            grid.symbols(SDAR.sym[["sed.sym"]][[tmp_file]], x=ind_x[i],
              (sub.sed.structures[i, "to"] - sub.sed.structures[i, "from"])/ 2 + sub.sed.structures[i, "from"], 
              default.units = "native", size = unit(symbols.size * 24, "pt"))
          }
        }  
      upViewport()
    } 
    # Others symbols
    if(!is.null(other.sym)) {			
      drawHeader(which(arrng == "d.other.sym") + 1, row.head, "Others", 90, 1, 2)
      open.viewport(1, poscol=which(arrng == "d.other.sym") + 1, datum, iv=1, subset.lim)
        if(nrow(sub.other.sym) > 0) {
          for(i in 1:nrow(sub.other.sym)) {
          tmp_file <- paste(others.table[sub.other.sym$others[i], "file_name"], ".svg", sep="")
          ind_x <- rep(c(0.25, 0.75), nrow(sub.other.sym))
          ind_x <- ind_x[1:nrow(sub.other.sym)]
          grid.symbols(SDAR.sym[["other.sym"]][[tmp_file]], x=ind_x[i],
            (sub.other.sym[i, "to"] - sub.other.sym[i, "from"])/ 2 + sub.other.sym[i, "from"], 
            default.units = "native", size = unit(symbols.size * 17, "pt"))
          }
        }  
      upViewport()
    }
    # Header
    pushViewport(viewport(layout.pos.col=c(2, length(widths) - 1),        # Open Viewport to draw main.
      layout.pos.row=row.head - 1))
      grid.text(paste("Printed by SDAR, Ortiz J. et al. 2015, (", 
      	format(Sys.time(), "%b %d %Y"), ")", sep=""),
        x=0, y=0.075, just = "left", 
        gp=gpar(cex= 0.8, col="darkblue"))
      if(!is.null(main)) {
      grid.text(main, x=0.5, y=0.7,  
        gp=gpar(cex=1.2, lineheight=1.1, font=2))
      }
      if(!is.null(sub)){
        grid.text(sub, x=0.5, y=0.4,  
          gp=gpar(cex=1, lineheight=1.1, font=1))
      }
    upViewport()

    # Legend
    if(d.legend == TRUE) {
      pushViewport(viewport(layout.pos.col=c(1:length(widths)), 
        layout.pos.row= 6, name="legend"))
        if(is.null(metadata)){
          # warning('typically required metadata includes these information: Site Name, Latitude, Longitude, Elevation, Author, and References')
          metadata <- list(locality_name = '',
                   latitude = '',
                   longitude = '',
                   elevation = '',
                   author = '')
        }
        grid.rect(gp=gpar(lwd=0.5))
        grid.text("LEGEND", x=unit(0.5, "npc") + unit(0.5, "cm"),
          y=unit(1,"npc") - unit(0.4, "cm"), gp=gpar(font=2))
        grid.text(c("Locality Name", metadata$locality_name,
          paste("Scale: 1:", d.scale), "Location", "Authors"), x= unit(2.5, "cm"), y=unit(1, "npc") - unit(c(0.5, 1.25, 2, 3, 5.75), "cm"),
          just = "center", gp=gpar(cex=c(1.1, 1, 1, 1.1, 1.1), font=c(2, 2, 1, 2, 2))) 
        grid.text(c("Latitude:", "Longitude:", "Elevation:",
          ifelse(exists("latitude", where=metadata), metadata$latitude, ""), 
          ifelse(exists("longitude", where=metadata), metadata$longitude, ""),
          ifelse(exists("elevation", where=metadata), metadata$elevation, "")),
          x= unit(c(rep(0.25, 3), rep(2, 3)), "cm"), 
          y=unit(1, "npc") - unit(c(3.6, 4.2, 4.8, 3.6, 4.2, 4.8), "cm"),
          just = "left")
        grid.segments(x0=0, y0=unit(1, "npc") - unit(c(2.5, 5.3), "cm"), x1=unit(5, "cm"), 
          y1=unit(1, "npc") - unit(c(2.5, 5.3), "cm"), gp=gpar(lwd=0.5))
        grid.segments(x0=unit(5, "cm"), y0=0, x1=unit(5, "cm"), 
          y1=1, gp=gpar(lwd=0.5))      
        if(exists("author", where=metadata)) {
                splitText <- strwrap(metadata$author, 32, exdent=3) # split the string in several lines to be draw.
                t <- unit(1, "npc") - unit(6.35, "cm")
                for(i in 1:length(splitText)) {  # draw text
                  grid.text(splitText[i], 
                    x = unit(0.25, "cm"), 
                    y = t, just="left", 
                    gp=gpar(cex=1))
                    t <- t - unit(0.5, "cm")
                }
        }
        grid.text("* (http://wiki.aapg.org/Show_evaluation)", x=unit(0.1, "cm"),
          y=unit(-0.25,"cm"), gp=gpar(font=3, col="blue"), just="left")
        if(lithology == TRUE){
          pushViewport(viewport(x=unit(5, "cm"), y=0, 
            width=unit(4.5, "cm"), height=unit(h.legend - 0.65,"cm"),
            just=c("left", "bottom")))
            grid.text("Dominant lithology", x=0.5, y=unit(h.legend - 0.65 - 0.5, "cm"), 
            gp=gpar(font=2))
            draw.pattern.legend <- function(x, y, pat_func, d.color, sc.fac, pSize, label){
            pushViewport(viewport(x, y, width=unit(4.5, "cm"), 
              height=unit(0.5, "cm"), xscale=c(0,1),
              just=c("left","bottom"),  
              default.units = "cm"))   
              pat_func(c(0,0, 0.2, 0.2), c(0,1,1,0), 0,
                1, d.color, sc.fac, pSize)
            upViewport()  
            grid.text(label, x + 1.15, y + 0.25,
              default.units="cm",
              just="left", gp=gpar(cex=1.1))
          }
          t.leg <- data.frame(pos.pat=seq(1, rows_leg, 1), 
            xleg=rep(0.5, rows_leg), 
            yleg=rep((h.legend - 1.5) - cumsum(rep(0.7, rows_leg))))
          pos.pat <- 0
          if("claystone" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
              clayP, leg.litho[leg.litho$prim_litho == "claystone", 2],
              sc.fac=1.8, pSize=NULL, "claystone")
          }
          if("mudstone" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            mudP, leg.litho[leg.litho$prim_litho == "mudstone", 2],
            sc.fac=1.6, pSize=NULL, "mudstone")
          }
          if("siltstone" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            siltP, leg.litho[leg.litho$prim_litho == "siltstone", 2],
            sc.fac=0.5, pSize=1.05, "siltstone")
          }
          if("shale" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            shaleP, leg.litho[leg.litho$prim_litho == "shale", 2],
            sc.fac=1.8, pSize=NULL, "shale")
          }
          if("sandstone" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            sandP, leg.litho[leg.litho$prim_litho == "sandstone", 2],
            sc.fac=2, pSize=0.5, "sandstone")
          }
          if("conglomerate" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            congP, leg.litho[leg.litho$prim_litho == "conglomerate", 2],
            sc.fac=2, pSize=0.125, "conglomerate")
          }
          if("breccia" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            brecP, leg.litho[leg.litho$prim_litho == "breccia", 2],
            sc.fac=2.25, pSize=0.39, "breccia")
          }
          if("limestone" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            limeP, leg.litho[leg.litho$prim_litho == "limestone", 2],
            sc.fac=1.25, pSize=NULL, "limestone")
          }
          if("dolomite" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            dolP, leg.litho[leg.litho$prim_litho == "dolomite", 2],
            sc.fac=1.75, pSize=NULL, "dolomite")
          }
          if("coal" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            coalP, leg.litho[leg.litho$prim_litho == "coal", 2],
            sc.fac=NULL, pSize=NULL, "coal")
          }
          if("tuff" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat], 
            pyroP, leg.litho[leg.litho$prim_litho == "tuff", 2],
            sc.fac=1, pSize=NULL, "tuff")
          }
          if("granite" %in% leg.litho$prim_litho) {
            pos.pat <- pos.pat + 1
            draw.pattern.legend(t.leg$xleg[pos.pat], t.leg$yleg[pos.pat],
            ignP, leg.litho[leg.litho$prim_litho == "granite", 2],
            sc.fac=1.75, pSize=NULL, "granite")
          }
          if("covered" %in% x$rock_type) {
            pos.pat <- pos.pat + 1
            pushViewport(viewport(t.leg$xleg[pos.pat],  
              t.leg$yleg[pos.pat], 
              width=unit(0.9, "cm"), 
              height=unit(0.5, "cm"), xscale=c(0,1),
              just=c("left","bottom"),  
              default.units = "cm"))   
              grid.lines(x=c(0, 1), y=c(0, 1),
                gp=gpar(lwd=0.2))
              grid.lines(x=c(0, 1), y=c(1,0),
                gp=gpar(lwd=0.2))
              grid.rect()
            upViewport()  
            grid.text("covered", t.leg$xleg[pos.pat] + 1.15, 
              t.leg$yleg[pos.pat] + 0.25,
              default.units="cm",
              just="left", gp=gpar(cex=1.1))
          }  
          upViewport()
        }
        # Oil Stain
        need_pos <- c(lithology, any(!is.null(oil.stain), !is.null(bioturbation)), 
          !is.null(sed.structures), !is.null(fossils), !is.null(other.sym))
        if(!is.null(oil.stain)) {
        pos.oil.x <- ifelse(isTRUE(need_pos[1]), 10.5, 5) 
        pushViewport(viewport(x=unit(pos.oil.x, "cm"), y=unit(1, "npc") - unit(0.65,"cm"), 
          width=unit(4.5, "cm"), height=unit(4,"cm"),
          just=c("left", "bottom")))
          grid.text(c("Visual Oil Stain ", "*"), x=c(0.5, 0.81), y=unit(-0.5,"cm"),
            gp=gpar(cex=c(1,1.1), font=c(2,3), col=c("black","blue")))
          grid.rect(x=0.175, y=unit(seq(-1.15, -3.75, -0.65),"cm"),
          	width=0.15, height=unit(0.3, "cm"), gp=gpar(lwd=0.5, 
          	fill=c("grey100", "grey75", "grey50", "grey25", "grey0")))
          grid.text(c("Weak", "Moderate weak", "Moderate", "Moderate strong", "Strong"), 
          	x=0.325, y=unit(seq(-1.15, -3.75, -0.65),"cm"), 
            gp=gpar(cex=1), just="left")
        upViewport()
        }
        # bioturbation
        if(!is.null(bioturbation)) {
          pos.bio.x <- ifelse(isTRUE(need_pos[1]), 10.5, 5) 
          pos.bio.y <- ifelse(!is.null(oil.stain), 4.65, 0.65) 
          pushViewport(viewport(x=unit(pos.bio.x, "cm"), y=unit(1, "npc") - unit(pos.bio.y, "cm"), 
            width=unit(4.5, "cm"), height=unit(3.35,"cm"),
            just=c("left", "bottom")))
            grid.text("Bioturbation Index", x=0.5, y=unit(-0.5,"cm"), 
              gp=gpar(cex=1.1, font=2))
            grid.text("(Taylor & Goldring, 1983)", x= 0.5, y=unit(-0.9,"cm"),
              gp=gpar(cex=0.9, font=3, col="blue"))
            grid.text(c("1 - Sparse", "2 - Low", "3 - Moderate", 
          	  "4 - High", "5 - Intense", "6 - Complete"), x=c(rep(0.065, 3), rep(0.545, 3)),
          	  y=unit(rep(c(-1.6, -2.2, -2.8), 2),"cm"), just="left")
          upViewport()  
        }
        if(!is.null(sed.structures)) {
          if(sum(need_pos[1:2]) == 0) {
            pos.sed.x <- 5
          }
          if(sum(need_pos[1:2]) == 1) {
            pos.sed.x <- 10.5
          }
          if(sum(need_pos[1:2]) == 2) {
            pos.sed.x <- 16
          } 
          pushViewport(viewport(x=unit(pos.sed.x, "cm"), y=0, 
            width=unit(6, "cm"), height=unit(h.legend - 0.65,"cm"),
            just=c("left", "bottom")))
            grid.text("Sedimentary structures", x=0.5, y=unit(h.legend - 0.65 - 0.5, "cm"), 
              gp=gpar(font=2))
            if(length(leg.sed) > 0) {
              for(i in 1:length(leg.sed)) {
                tmp_file <- paste(sed.struc.table[leg.sed[i], "file_name"], ".svg", sep="")
                ind_y <- unit(rep((h.legend - 1.3) - cumsum(rep(0.5, length(leg.sed)))), "cm")
                grid.symbols(SDAR.sym[["sed.sym"]][[tmp_file]], x= 0.1,
                  y=ind_y[i], size = unit(symbols.size * 24, "pt"))
                grid.text(sed.struc.table[leg.sed[i], "name"], x=0.2,
                  y=ind_y[i], just="left")
              }
            }   
          upViewport()
        }
        if(!is.null(fossils)) {
          if(sum(need_pos[1:3]) == 0) {
            pos.sed.x <- 5
          }
          if(sum(need_pos[1:3]) == 1) {
            pos.sed.x <- 10.5
          }
          if(sum(need_pos[1:3]) == 2) {
            pos.sed.x <- 16
          }
          if(sum(need_pos[1:3]) == 3) {
            pos.sed.x <- 21.5
          }  		
          pushViewport(viewport(x=unit(pos.sed.x, "cm"), y=0, 
            width=unit(5, "cm"), height=unit(h.legend - 0.65,"cm"),
            just=c("left", "bottom")))
            grid.text("Fossils", x=0.5, y=unit(h.legend - 0.65 - 0.5, "cm"), 
              gp=gpar(font=2))
            if(length(leg.fos) > 0) {
              for(i in 1:length(leg.fos)) {
                tmp_file <-  paste(fossil.table[sub.fossils$fossil[i], "file_name"], ".svg", sep="")
                ind_y <- unit(rep((h.legend - 1.3) - cumsum(rep(0.5, length(leg.fos)))), "cm")
                grid.symbols(SDAR.sym[["fos.sym"]][[tmp_file]], x= 0.1,
                  y=ind_y[i], size = unit(symbols.size * 17, "pt"))
                grid.text(fossil.table[sub.fossils$fossil[i], "name"], x=0.2,
                  y=ind_y[i], just="left")
              }
            }   
          upViewport()
        }
        if(!is.null(tracefossils)) {			
          pushViewport(viewport(x=unit(25, "cm"), y=0, 
            width=unit(5, "cm"), height=unit(h.legend - 0.65,"cm"),
            just=c("left", "bottom")))
            grid.text("Trace fossils", x=0.5, y=unit(h.legend - 0.65 - 0.5, "cm"), 
              gp=gpar(font=2))
            if(length(leg.trac) > 0) {
              for(i in 1:length(leg.trac)) {
                tmp_file <-  paste(tracefossil.table[sub.tracefossils$tracefossil[i], "file_name"], ".svg", sep="")
                ind_y <- unit(rep((h.legend - 1.3) - cumsum(rep(0.5, length(leg.trac)))), "cm")
                grid.symbols(SDAR.sym[["trac.sym"]][[tmp_file]], x= 0.1,
                  y=ind_y[i], size = unit(symbols.size * 16, "pt"))
                grid.text(tracefossil.table[sub.tracefossils$tracefossil[i], "name"], x=0.2,
                  y=ind_y[i], just="left")
              }
            }   
          upViewport()
        }
        if(!is.null(other.sym)) {
          if(sum(need_pos[1:4]) == 0) {
            pos.sed.x <- 5
          }
          if(sum(need_pos[1:4]) == 1) {
            pos.sed.x <- 10.5
          }
          if(sum(need_pos[1:4]) == 2) {
            pos.sed.x <- 16
          }
          if(sum(need_pos[1:4]) == 3) {
            pos.sed.x <- 21.5
          }
          if(sum(need_pos[1:4]) == 4) {
            pos.sed.x <- 27
          }   
          pushViewport(viewport(x=unit(pos.sed.x, "cm"), y=0, 
            width=unit(5, "cm"), height=unit(h.legend - 0.65,"cm"),
            just=c("left", "bottom")))
            grid.text("Others", x=0.5, y=unit(h.legend - 0.65 - 0.5, "cm"), 
              gp=gpar(font=2))
            if(length(leg.other) > 0) {
              for(i in 1:length(leg.other)) {
                tmp_file <-  paste(others.table[sub.other.sym$others[i], "file_name"], ".svg", sep="")
                ind_y <- unit(rep((h.legend - 1.3) - cumsum(rep(0.5, length(leg.other)))), "cm")
                grid.symbols(SDAR.sym[["other.sym"]][[tmp_file]], x= 0.1,
                  y=ind_y[i], size = unit(symbols.size * 17, "pt"))
                grid.text(others.table[sub.other.sym$others[i], "name"], x=0.2,
                  y=ind_y[i], just="left")
              }
            }   
          upViewport()
        }

#########

        
      upViewport()
    }

  upViewport() 
  dev.off()
  message("The .pdf file has been created successfully")
}

