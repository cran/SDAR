
strata <- function(x, datum = "top") {
  # Coerce to a Data Frame
  if(!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors=FALSE)
  }
  # check variables names and type
  rqd_names <- c("bed_number", "base", "top", "rock_type", "prim_litho", "grain_size") # Required column names
  ind_names <- rqd_names %in% colnames(x)
  if(!all(ind_names == TRUE)) {
    stop(call.=FALSE, paste0("Column names does not agree with the column names required by a strata object. ",
      "Column names (", paste0(rqd_names[ind_names == FALSE], 
      collapse=", "), ") are missing. Check column names"), sep="")
  }
  if(!all(lapply(x[c("bed_number", "base", "top")], class) == "numeric")) {
    stop(call.=FALSE, "Columns (bed_number, base and top) should be numeric type")
  }
  ## check if base and top are equal
  if(any(x$base == x$top)) {
    stop(call.=FALSE, paste0("Check thickness 'base-top' in bed numbers (", 
      paste0(head(x[(x$base == x$top) == TRUE, "bed_number"], 7), 
        collapse=", "), ") 'base and top can not be equal'"))
  }
  if(is.null(datum)) {
    stop(call.=FALSE, "datum should be 'base' or 'top'. 'base' when thickness are measured up from the bottom (e.g. stratigraphic section); 'top' when depths are measured from the surface (e.g. core)")
  }
  if(datum == "base") {
  	if(any(x$base > x$top)) {
  	  stop(call.=FALSE, paste0("Check thickness 'base-top' in bed numbers (", 
  	    paste0(head(x[(x$base < x$top) == FALSE, "bed_number"], 7), collapse=", "), ") 'top should be greather than base'"))
  	}
  }else{  # SDAR will deal negative values of depth  
    if(any(x$base < x$top)) {
  	  stop(call.=FALSE, paste0("Check thickness 'base-top' in bed numbers (", 
  	    paste0(head(x[(x$base > x$top) == FALSE, "bed_number"], 7), collapse=", "), ") 'base should be greather than top'"))
  	}
  	x[, c("base", "top")] <- x[, c("base", "top")] * -1
  }
  # Coerce data frame to events (LRS implementation)
  x <- events(from = x$base, to = x$top, 
    x[,-which(names(x) %in% c("base","top"))])
  # check for overlaps
  overlaps <- event_overlaps(x)
  beds_over <- overlaps[which(overlaps$n>1),]
  if(nrow(beds_over) > 0) {
    colnames(beds_over) <- c("base","top", "n")
	  message("\n", "Error: overlapping beds are not allowed")
    message("       This function returned a dataframe with the overlapping intervals", "\n")
    return(beds_over)
  }  
  # check for gaps
  gaps <- event_gaps(x)
  if(nrow(gaps) > 0) {
    if(nrow(gaps) == 1) {
	    mes <- "There is a range without information"
    }else{
      mes <- "There are some ranges without information"
    }
    warning(call.=FALSE, mes) 
  }

  # test for columns of type "factor"
  ind_fac <- sapply(x, class) == "factor"
  # attempts to coerce factor to character type
  if(any(ind_fac == TRUE)) {
    x[ind_fac] <- apply(x[ind_fac], 2, as.character)
    warning("factor coerced to character type")
  }
  # test for columns of type "character"  
  ind <- sapply(x, class) == "character"	
  # transform to lowercase
  x[ind] <- apply(x[ind], 2, tolower)
  # removing leading/trailing whitespaces
  x[ind] <- apply(x[ind], 2, trimws)

###  Falta revisar si son id, validarlos y transformarlos en las variables (id_rock_type, id_prim_litho, id_grain_size)

  # convert strings (labels) to numeric id
  cnv_to_id <- function(x, colna, ref_tb) {
    new_col <- paste("id_", colna, sep="")
    x[, new_col] <- ref_tb[match(x[, colna], ref_tb[, 
    	ifelse(is.numeric(x[, colna]), "id", "name")]), "id"]
    # identify notIn and exclude cover segments
    return(x)
  }
  x <- cnv_to_id(x, "rock_type", rock.table)
  x <- cnv_to_id(x, "prim_litho", litho.table)
  x <- cnv_to_id(x, "grain_size", gs.table)
  # transform to strata class
  message("   'beds data has been validated successfully'")
  new("strata", x)
}



summary.strata <- function(object, grain.size=FALSE, ...) {
    object$thk <- abs(object$to - object$from)
    xc <- subset(object, object$rock_type == "covered")
    xc$thk <- abs(xc$to - xc$from)
  ans <- list()
  ans$nbeds <- c(length(object$bed_number) - nrow(xc))
  ans$ncover <- nrow(xc)
  ans$thk <- max(object[c("to", "from")]) - min(object[c("to", "from")])
  ans$thkcover <- sum(xc$thk)
## Summary by primary lithology  
    litho_factor <- factor(object[,"prim_litho"], levels=litho.table[, "name"]) # as factor

    thick = tapply(object[,"thk"], litho.table[litho_factor, 2], sum)
    xnbed <- data.frame(thick, round(thick * 100 / ans$thk, 2),
	     table(litho.table[litho_factor,2]))
    xnbed <- xnbed[order(xnbed$thick, decreasing=TRUE),c(1:2,4)]
             names(xnbed) <- c("Thickness", "Percent (%)", "Number beds")
    if(nrow(xc) > 0) {
      xnbedCA <- data.frame(sum(xc$thk), round(sum(xc$thk) *100 / ans$thk, 2), nrow(xc), row.names = "covered")
      names(xnbedCA) <- c("Thickness", "Percent (%)", "Number beds")
      xnbed <- rbind(xnbed, xnbedCA)
    }
  ans$table_res <- xnbed
## summary by grain size
  if(grain.size == TRUE) {
    if(!(grain.size %in% c("FALSE", "TRUE"))) {
      stop(call.=FALSE, "the 'litho' argument must be 'FALSE' or 'TRUE'")      
    } 
    sub_sed <- object[which(object$rock_type == "sedimentary"),]

    gs_factor <- factor(sub_sed[,"grain_size"], levels=gs.table[, "name"])  # as factor

    thick_GS <- tapply(sub_sed[,"thk"], gs.table[gs_factor, 2], sum)
    xnbed_GS <- data.frame(thick_GS, round(thick_GS * 100 / ans$thk, 2),
	     table(gs.table[gs_factor,2]))
    xnbed_GS <- xnbed_GS[match(gs.table[,"name"], xnbed_GS$Var1), ]
    xnbed_GS <- xnbed_GS[complete.cases(xnbed_GS),c(1:2,4)]
             names(xnbed_GS) <- c("Thickness", "Percent (%)", "Number beds")
  
    sub_other <- object[!(row.names(object) %in% row.names(sub_sed)),]
    if(nrow(sub_other) > 0) {

      rock_factor <- factor(sub_other[,"rock_type"], levels=rock.table[, "name"])  # as factor

      thick_other <- tapply(sub_other[,"thk"], rock.table[rock_factor, 2], sum)
      xnbed_other <- data.frame(thick_other, round(thick_other * 100 / ans$thk, 2),
	     table(rock.table[rock_factor, 2]))
      xnbed_other <- xnbed_other[match(rock.table[,"name"], xnbed_other$Var1), ]
      xnbed_other <- xnbed_other[complete.cases(xnbed_other),c(1:2,4)]
             names(xnbed_other) <- c("Thickness", "Percent (%)", "Number beds")
    }
    if(nrow(sub_other) > 0) {
      ans$table_GS <- rbind(xnbed_GS, xnbed_other)
    }else{
      ans$table_GS <- xnbed_GS
    }
  }
  class(ans) <- c("summary.strata", "listof")
ans  
}


print.summary.strata <- function(x, ...) {
  xn <- data.frame(c("Number of beds: ", 
	  "Number of covered intervals",
	  "Thickness of the section: ", 
	  "Thickness of covered intervals: "), 
          c(x$nbeds, x$ncover, x$thk, x$thkcover))
    names(xn) <- NULL
    print(format(xn[1:2,], width = 4, justify = "left"), row.names = F)
    print(format(xn[3:4,], width = 4, digits=3, justify = "left"), row.names = F)
    cat("\nSummary by lithology:", "\n", "\n")
    print(format(x$table_res, width = 12, digits=2, justify = "centre"))
  if("table_GS" %in% names(x)) {
    cat("\nSummary by Grain Size:", "\n", "\n")
    print(format(x$table_GS, width = 12, digits=2, justify = "centre"))
  }
  invisible(x)
}

