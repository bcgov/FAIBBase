#' Check the size change of a remeasured subject
#'
#' @description This function is to check the size change for a remeasured subject.
#'
#' @param subjectID character, Specifies subject ID, such as a tree id.
#' @param measTime numeric, Measurement number with bigger value indicates a later measurement.
#' @param size numeric, Measurement of an attribute.
#' @param change character, Change direction either from \code{increase} or \code{decrease}.
#'                          Default is \code{increase}.
#' @param maxChangeRate numeric, It determines the maximum change rate. If the change rate from previous
#'                               to current measurement exceeds the maximum change rate, then the pass of current
#'                               measurement will be flagged as \code{FALSE}. If missing, this term is set
#'                               as \code{NULL}.
#' @param toleranceMethod character, Method to allow acceptable measurement error in an opposite direction
#'                           of change argument.
#'                           It must be either \code{both} (break both absolute and relative tolerance),
#'                           \code{either} (break either absolute or relative tolerance),
#'                           \code{absolute} (break absolute tolerance only),
#'                            or \code{relative} (break relative tolerance only).
#'                           Default is \code{both}.
#' @param toleranceAbs numeric, Absolute tolerance value (exclusive) to allow measurement error.
#'                           It must be a a non-negative value.
#'                           If the change is \code{increase}, the change from current measurement to
#'                           last measurement will be compared to the negative tolerance value,
#'                           and vice versa. Default is \code{0} for zero tolerance.
#' @param toleranceRel numeric, Relative tolerance value (exclusive) to allow measurement error.
#'                           It must be a a non-negative value.
#'                           If the change is \code{increase}, the change from current measurement to
#'                           last measurement will be compared to the negative tolerance value,
#'                           and vice versa. Default is \code{0} for zero tolerance.
#' @return A data table that contains pass information. TRUE indicates pass, while FALSE indicates
#'         failure.
#' @importFrom data.table ':=' data.table copy
#' @importFrom fpCompare %<=% %<<% %>=%
#' @author Yong Luo
#' @export
#' @rdname checkSize_remeas
checkSize_remeas <- function(subjectID,
                             measTime,
                             size,
                             change = "increase",
                             maxChangeRate = NULL,
                             toleranceMethod = "both",
                             toleranceAbs = 0,
                             toleranceRel = 0){
  if(!change %in% c("increase", "decrease")){
    stop("change must be correctly defined from increase or decrease.")
  }
  if(!toleranceMethod %in% c("both", "either", "absolute", "relative")){
    stop("tolerance must be correctly defined from both, absolute, or relative.")
  }
  if(toleranceMethod %in% c("both", "either")){
    if(toleranceAbs %<<% 0 | toleranceRel %<<% 0){
      stop("tolerance value must be defined as a non-negative value.")
    }
  } else if(toleranceMethod == "absolute"){
    if(toleranceAbs %<<% 0){
      stop("tolerance value must be defined as a non-negative value.")
    }
  } else {
    if(toleranceRel %<<% 0){
      stop("tolerance value must be defined as a non-negative value.")
    }
  }
  if(is.null(maxChangeRate)){
    maxChangeRate <- Inf
  }
  thedata <- data.table(subjectID = subjectID,
                        measTime = measTime,
                        size = size)
  orgdata <- data.table::copy(thedata)
  if(nrow(thedata) != nrow(unique(thedata, by = c("subjectID", "measTime")))){
    stop("Multiple sizes were recorded for one subject/measurement, please check the duplicates.")
  }
  thedata <- thedata[order(subjectID, measTime),]
  thedata[, obsid := 1:length(measTime), by = "subjectID"]
  thedata[, lastobs := max(obsid), by = "subjectID"]
  thedata[, ':='(Fin_size = shift(size, n = 1L, fill = NA, type = "lead"),
                 Fin_measTime = shift(measTime, n = 1L, fill = NA, type = "lead"))]
  thedata <- thedata[obsid != lastobs,]
  thedata[, ':='(size_dif_abs = Fin_size - size,
                 size_dif_rel = (Fin_size - size)/size,
                 size_changeRate = (Fin_size - size)/(Fin_measTime - measTime))]
  thedata[, ':='(pass = TRUE, reason = as.character(NA), memo = as.character(NA))]
  thedata[is.na(size_dif_abs) & is.na(Fin_size),
          ':='(pass = FALSE, reason = "missing size")]
  if(change == "increase"){
    if(toleranceMethod == "both"){
      thedata[size_dif_abs %<=% (-toleranceAbs) &
                size_dif_rel %<=% (-toleranceRel),
              ':='(pass = FALSE, reason = "break both tolerance",
                   memo = paste0("tol_abs: ", size_dif_abs,
                                 ". tol_rel: ", round(size_dif_rel, 2)))]
    } else if(toleranceMethod == "either"){
      thedata[size_dif_abs %<=% (-toleranceAbs) |
                size_dif_rel %<=% (-toleranceRel),
              ':='(pass = FALSE, reason = "break either tolerance",
                   memo = paste0("tol_abs: ", size_dif_abs,
                                 ". tol_rel: ", round(size_dif_rel, 2)))]
    } else if(toleranceMethod == "absolute"){
          thedata[size_dif_abs %<=% (-toleranceAbs),
            ':='(pass = FALSE, reason = "break absolute tolerance",
                 memo = paste0("tol_abs: ", size_dif_abs))]
    } else {
      thedata[size_dif_rel %<=% (-toleranceRel),
              ':='(pass = FALSE, reason = "break relative tolerance",
                   memo = paste0("tol_rel: ", round(size_dif_rel, 2)))]
    }
    thedata[size_changeRate %>>% maxChangeRate,
            ':='(pass = FALSE, reason = "abnormal change rate",
                 memo = paste0("change rate: ", round(size_changeRate, 2)))]
  } else if(change == "decrease"){
    if(toleranceMethod == "both"){
      thedata[size_dif_abs %>=% (toleranceAbs) &
                size_dif_rel %>=% (toleranceRel),
              ':='(pass = FALSE, reason = "break both tolerance",
                   memo = paste0("tol_abs: ", size_dif_abs,
                                 ". tol_rel: ", round(size_dif_rel, 2)))]
    } else if(toleranceMethod == "either"){
      thedata[size_dif_abs %>=% (toleranceAbs) |
                size_dif_rel %>=% (toleranceRel),
              ':='(pass = FALSE, reason = "break either tolerance",
                   memo = paste0("tol_abs: ", size_dif_abs,
                                 ". tol_rel: ", round(size_dif_rel, 2)))]
    } else if(toleranceMethod == "absolute"){
      thedata[size_dif_abs %>=% (toleranceAbs),
              ':='(pass = FALSE, reason = "break absolute tolerance",
                   memo = paste0("tol_abs: ", size_dif_abs))]
    } else {
      thedata[size_dif_rel %>=% (toleranceRel),
              ':='(pass = FALSE, reason = "break relative tolerance",
                   memo = paste0("tol_rel: ", size_dif_rel))]
    }
    thedata[size_changeRate %<<% -maxChangeRate,
            ':='(pass = FALSE, reason = "abnormal change rate",
                 memo = paste0("change rate: ", round(size_changeRate, 2)))]
  }
  thedata[, measTime := Fin_measTime]
  orgdata <- merge(orgdata, thedata[,.(subjectID, measTime, pass, reason, memo)],
                   by = c("subjectID", "measTime"),
                   all.x = TRUE)
  return(orgdata)
}
