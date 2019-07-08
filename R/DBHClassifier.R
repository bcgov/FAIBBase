#' Derive DBH class from DBH
#'
#'
#' @description This function derives DBH classes based on DBH.
#'              This function is equivalent to dbh_cl.sas macro.
#'
#' @param DBH numeric, Tree DBH.
#' @param classInterval numeric, The interval that used to categorize the DBH. If missing
#'                               5 cm is used.
#' @param maxDBH numeric, Upper class limit. DBH that surpasses this limit is groupped
#'                        in at this limit. If missing 175 is used.
#'
#' @return Classified DBH
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#'
#' @export
#' @docType methods
#' @rdname DBHClassifier
#'
#' @author Yong Luo
DBHClassifier <- function(DBH, classInterval = 5, maxDBH = 175){
    worktable <- data.table(uniObs = 1:length(DBH),
                            DBH)
    worktable[DBH %>>% (maxDBH - classInterval/2), DBH := maxDBH]
    worktable[, DBH_CL := (as.integer((DBH + classInterval/2)/classInterval)) * classInterval]
    worktable[DBH %<=% classInterval/2, DBH_CL := classInterval]
    return(worktable[order(uniObs)]$DBH_CL)
  }
