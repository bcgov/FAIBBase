#' Merge table and update values for duplicate column   
#' 
#' 
#' @description This is an extended function for \code{\link{merge}} function by updating values for duplicate 
#'              column for the first, second or both tables.
#'              
#' @param x data.table, The first table for merging.
#' @param y data.table, The second table for merging.
#' @param by character, The key to merge two tables.
#' @param updateDup logical, Specifies whether update duplicate column in merged table when its information is
#'                           available in y table, which means update from the second table. 
#'                           If missing, the function takes \code{TRUE}.
#' @param ... see \code{\link{merge}} for rest of arguments.
#'                   
#' @return A merged table without duplicate columes. A warning message is given if the duplicate column
#'         has different values.
#' 
#' @importFrom data.table ':=' as.data.table
#' @importFrom fpCompare '%!=%'
#' @seealso \code{\link{merge}}
#' 
#' @export
#' @docType methods
#' @rdname merge_dupUpdate
#'
#' @author Yong Luo
#'
setGeneric("merge_dupUpdate",
           function(x, y, by, updateDup, ...) {
             standardGeneric("merge_dupUpdate")
           })

#' @rdname merge_dupUpdate
setMethod(
  "merge_dupUpdate",
  signature = c(x = "data.table",
                y = "data.table",
                by = "character",
                updateDup = "logical"),
  definition = function(x, y, by, updateDup, ...){
    allnames <- c(names(x), names(y))
    ## check whether these two tables have same col names, and remove these col in vi_b dataset
    redundantNames <- unique(data.table::as.data.table(table(allnames))[N > 1 & !(allnames %in% by),
                                                            ]$allnames)
    rm(allnames)
    if(updateDup){
      mergedtable <- merge(data.table::copy(x)[, firsttable := TRUE], 
                           data.table::copy(y)[, secondtable := TRUE], by = by, ...)
      if(length(redundantNames) > 0){
        for (i in redundantNames){
          nonNaMergedTable <- mergedtable[firsttable == TRUE & secondtable == TRUE, ]
          if(class(unlist(nonNaMergedTable[, paste(i, ".x", sep = ""), with = FALSE])) == "numeric"){
            if(nrow(nonNaMergedTable[unlist(nonNaMergedTable[, paste(i, ".x", sep = ""), with = FALSE]) %!=%
                                     unlist(nonNaMergedTable[, paste(i, ".y", sep = ""), with = FALSE])]) > 0){
              warning(paste("The values in duplicate column ", i, 
                            " do not match for same group. Be aware of the column you intend to update.", sep = ""))
            }
          } else {
            if(nrow(nonNaMergedTable[unlist(nonNaMergedTable[, paste(i, ".x", sep = ""), with = FALSE]) !=
                                     unlist(nonNaMergedTable[, paste(i, ".y", sep = ""), with = FALSE])]) > 0){
              warning(paste("The values in duplicate column ", i, 
                            " do not match for same group. Be aware of the column you intend to update.", sep = ""))
            }
          }
          
        }
        if(nrow(mergedtable[firsttable == TRUE & is.na(secondtable),]) > 0){
          for(i in redundantNames){
            mergedtable[firsttable == TRUE & is.na(secondtable), 
                        paste(i, ".y", sep = "") := unlist(mergedtable[firsttable == TRUE & is.na(secondtable),
                                                                       paste(i, ".x", sep = ""),
                                                                       with = FALSE])]
          }
        }
        set(mergedtable, , paste(redundantNames, ".x", sep = ""), NULL)
        setnames(mergedtable, paste(redundantNames, ".y", sep = ""), redundantNames)
      }
      mergedtable[, c("firsttable", "secondtable") := NULL]
    } else {
      mergedtable <- merge(x, y, by = by, ...)
    }
    return(mergedtable)
  })


#' @rdname merge_dupUpdate
setMethod(
  "merge_dupUpdate",
  signature = c(x = "data.table",
                y = "data.table",
                by = "character",
                updateDup = "missing"),
  definition = function(x, y, by, ...){
    return(merge_dupUpdate(x, y, by, updateDup = TRUE, ...))
  })

