### Function "read.LAS". It is a function to read "Log ASCII Standard (LAS)(*)" files into R.
# 
# It ignore the file header and copy only the data which came after ~A or ~ASCii tag.
# Identify the header of each column.
#     "read.LAS" function search for the column headers in the line who 
#     contains ~A or ~ASCii tags, if they are not present there, 
#     the function extract it from the "~Curve Information Block". 
#     If "~Curve Information Block" is not contained in the .LAS file
#     the data will loaded without headers.
# Replace NULL values, usually the NULL values are represented in the file with numbers as -999.25 
#     read.LAS function automatically identifies the NULL value used in the file.
#     the "repl.null" parameter allows users to replace these big numbers for NA values.
# Convert .LAS to .CSV, the parameter "writecsv" allow the users load the file into the current
#     R session "Default parameter", or save it as a CSV file. When it is not possible identifies
#     automatically the headers of the columns, the csv file name will contain the caracters (WH)
#     to denote "file without header".

read.LAS <- function(filePath, repl.null = FALSE, writecsv=FALSE) {
  if(!is.logical(repl.null)) {
    stop("argument 'repl.null' is not interpretable as logical, \nparameter must be TRUE or FALSE")
  }
  if(!is.logical(writecsv)) {
    stop("argument 'writecsv' is not interpretable as logical, \nparameter must be TRUE or FALSE")
  }
  # create file connection
  ## url  ??
  conn=file(filePath, open="r")
  x <- readLines(conn)
  las_txt = 0
  # Identify the lines tags "~".
  tagsID <- which(grepl("~", x))
  tagAsc <- which(grepl("~A", x))
  tagHeader <- which(grepl("~C", x))
  tagNULL <- which(grepl("NULL", x))
  # Extract data
  # Verify that there are not others tags after "~A" or "~Ascii", and extract the data.
  if(tagsID[length(tagsID)] == tagAsc) {
    las_txt <- x[tagAsc:length(x)]
  }else{
    las_txt <- x[tagAsc:(tagsID[which(tagsID == tagAsc) + 1] - 1)]
  }
  # Detect if the column headers are present in the line tagged as ~A or ~ASCii
  xh <- unlist(strsplit(las_txt[1], " "))
  xh <- xh[xh != ""][-1]   
  if(length(xh) != 0) {
    las_txt[1] <- paste(c(xh), collapse=" ")	# set in the first line the columns headers
    las_table <- read.table(header = TRUE, text=las_txt)
    mod <- ""
  }else{
  # Try to get the header from "~Curve Information Block"
    if(!length(tagHeader) == FALSE) {
      curveInfoBlock <- x[(tagHeader + 1):
	      (tagsID[which(tagsID == tagHeader) + 1] - 1)]
      if(TRUE %in% grepl("#", curveInfoBlock)) {	# Delete the comments in "~Curve Information Block"     
        headersInfo <- curveInfoBlock[-which(grepl("#", curveInfoBlock))]
      }else{
        headersInfo <- curveInfoBlock
      }
      vecHeader <- 0
      for(i in 1:length(headersInfo)) {
        dd <- unlist(strsplit(headersInfo[i], " "))
        ad <- dd[dd != ""][1]
        vecHeader[i] <- unlist(strsplit(ad, "\\."))[1]
      }
      las_txt[1] <- paste(c(vecHeader), collapse=" ") # set in the first line the columns headers detected
      las_table <- read.table(header = TRUE, text=las_txt)
      mod <- "_AH"  # modifier to attach in the CSV file name to denote "Automatic Header (AH)" 
                  # detect from "~Curve Information Block"
      # The "~Curve Information Block" is not contained in the .las file 
    }else{
      las_txt <- las_txt[-1]
      las_table <- read.table(header = FALSE, text=las_txt)
      mod <- "_WH"    # modifier to attach in the CSV file name to denote "Without Headers (WH)"
      message("* The header names was not automatically detected,
	      the csv will saved without column names\n")
    }
  }
  close(conn)
  # Detect NULL value
  if(!length(tagNULL) == FALSE) {
    xnull <- gsub(" . ", "", x[tagNULL])
    nullValue <- as.numeric(gsub("[^0-9.,-]", "",  xnull))
    message(paste("The NULL value =", nullValue, "\n"))
  }else{
    message(paste("The NULL value was not detected\n"))
  }
  if(repl.null == TRUE & !length(tagNULL) == FALSE) {
    las_table[las_table == nullValue] <- NA
    message(paste("** The NULL's values", nullValue, "was replaced by NA's\n"))
  }
  # write the las_table data to file ... in CSV format
  if(writecsv == TRUE) {
    csv_file <- paste(unlist(strsplit(filePath, "\\."))[1], mod, ".csv", sep="")
    write.table(las_table, file = csv_file, row.names=FALSE, sep=",")
    message("*** The .csv was sucesfully created\n")
  }else{
    return(las_table)
  }
}

###  End

# Reference
## http://stackoverflow.com/questions/12804203/is-there-an-r-package-to-parse-geophysical-log-ascii-standard-files-las-file
## (*) http://en.wikipedia.org/wiki/Log_ASCII_Standard
