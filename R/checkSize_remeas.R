#' Check the size change of a remeasured subject
#'
#' @description This function is to check the size change for a remeasured subject.
#'
#' @param subjectID character, Specifies subject ID, such as a tree id.
#' @param measNo numeric, Measurement number with bigger value indicates a later measurement.
#' @param size numeric, Measurement of an attribute.
#' @param change character, Change direction either from \code{increase} or \code{decrease}.
#'                          Default is \code{increase}.
#' @param tolerance numeric, Tolerance value (exclusive) to allow measurement error, which is a absolute value.
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
                             measNo,
                             size,
                             change = "increase",
                             tolerance = 0){
  if(!change %in% c("increase", "decrease")){
    stop("change must be correctly defined from increase or decrease.")
  }
  if(tolerance %<<% 0){
    stop("tolerance must be defined as a non-negative value.")
  }
  if(length(size[is.na(size)]) > 0){
    stop("size can not have missing value.")
  }
  thedata <- data.table(subjectID = subjectID,
                        measNo = measNo,
                        size = size)
  orgdata <- data.table::copy(thedata)
  if(nrow(thedata) != nrow(unique(thedata, by = c("subjectID", "measNo")))){
    stop("Multiple sizes were recorded for one subject/measurement, please check the duplicates.")
  }
  thedata <- thedata[order(subjectID, measNo),]
  thedata[, obsid := 1:length(measNo), by = "subjectID"]
  thedata[, lastobs := max(obsid), by = "subjectID"]
  thedata[, ':='(Fin_size = shift(size, n = 1L, fill = NA, type = "lead"),
                 Fin_measNo = shift(measNo, n = 1L, fill = NA, type = "lead"))]
  thedata <- thedata[obsid != lastobs,]
  thedata[, size_dif := Fin_size - size]
  thedata[, pass := TRUE]
  if(change == "increase"){
    thedata[size_dif %<=% (-tolerance), pass := FALSE]
  } else if(change == "decrease"){
    thedata[size_dif %>=% tolerance, pass := FALSE]
  }
  thedata[, measNo := Fin_measNo]
  orgdata <- merge(orgdata, thedata[,.(subjectID, measNo, pass)],
                   by = c("subjectID", "measNo"),
                   all.x = TRUE)
  return(orgdata)
}
