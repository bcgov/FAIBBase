#' Extended lm function by adding group functionality
#'
#'
#' @description A generic function by adding grouping functionality in \code{\link{lm}} function.
#'
#' @param formula character, Linear model formula.
#' @param data data.table, The data used for the models.
#' @param groupBy character, Specifies variables that used for the group.
#' @param ... see \code{\link{lm}} for the rest arguments.
#'
#' @return A list of regression analyses results
#'
#' @importFrom data.table ':='
#' @importFrom stats lm
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' @seealso \code{\link{lm}}
#'
#' @export
#' @docType methods
#' @rdname lm_group
#'
#' @author Yong Luo
#'
setGeneric("lm_group",
           function(formula, data, groupBy, ...) {
             standardGeneric("lm_group")
           })
#' @rdname lm_group
setMethod(
  "lm_group",
  signature = c(formula = "character",
                data = "data.table",
                groupBy = "character"),
  definition = function(formula, data, groupBy, ...){
    if(length(groupBy) == 1){
      if(is.na(groupBy)){

        allmodels <- lm(formula = formula, data = data, ...)
      } else {
        for(i in groupBy){
          if(i == groupBy[1]){
            data[, tempGroupName := unlist(data[, i, with = FALSE])]
          } else {
            data[, tempGroupName := paste(tempGroupName, unlist(data[, i, with = FALSE]), sep = " & ")]
          }
        }
        rm(i)
        modeldatalist <- list()
        for(i in unique(data$tempGroupName)){
          modeldatalist[[i]] <- data[tempGroupName == i,]
        }
        allmodels <- lapply(modeldatalist, function(s) lm(formula = formula, data = s, ...))
      }
    } else {
      ## prepare list data
      for(i in groupBy){
        if(i == groupBy[1]){
          data[, tempGroupName := unlist(data[, i, with = FALSE])]
        } else {
          data[, tempGroupName := paste(tempGroupName, unlist(data[, i, with = FALSE]), sep = " & ")]
        }
      }
      rm(i)
      modeldatalist <- list()
      for(i in unique(data$tempGroupName)){
        modeldatalist[[i]] <- data[tempGroupName == i,]
      }
      allmodels <- lapply(modeldatalist, function(s) lm(formula = formula, data = s, ...))
    }
    return(allmodels)
  })

#' @export
#' @rdname lm_group
setMethod(
  "lm_group",
  signature = c(formula = "character",
                data = "data.table",
                groupBy = "missing"),
  definition = function(formula, data, ...){
    return(lm_group(formula, data, groupBy = as.character(NA)))
  })

