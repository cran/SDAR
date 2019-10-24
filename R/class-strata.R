### Define a S4 class "strata"
 
setClass("strata",
  representation (),
  contains = "data.frame",
  prototype (new("data.frame")),
  validity = function(object) {
    # check data input, all values should be included in "reference_tables.R"
    retval <- NULL   
    valEntry <- function(object, colna, ref_tb, retval) {
      # identify notIn and exclude cover segments
      notIn <- which(is.na(object[, paste0("id_", colna)]) & 
        object[, "rock_type"] %in% c(3, "covered") == FALSE)
      if(length(notIn) != 0) {
        retval <- c(retval, paste(" Check row numbers ", 
          paste(notIn, collapse=", "), ". values (", 
          paste(object[notIn, colna], collapse=", "), ") are '", colna, "'",
          " not register in ", "'", substitute(ref_tb), "'", " table.", sep=""))
      }
      return(retval)
    }
    # check prim_litho
    retval <- valEntry(object, "rock_type", rock.table, retval)
    retval <- valEntry(object, "prim_litho", litho.table, retval)
    retval <- valEntry(object, "grain_size", gs.table, retval)         
    if(is.null(retval)) {
      return(TRUE)
    }else{
      stop(call.=FALSE, retval)
    }
  }        
)