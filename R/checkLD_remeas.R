#' Check the live and dead status of a remeasured subject
#'
#' @description This function is to check the live and deas status for a remeasured subject.
#'
#' @param subjectID character, Specifies subject ID, such as a tree id.
#' @param measNo numeric, Measurement number with bigger value indicates a later measurement.
#' @param LDStatus character, Live and dead status for each remeasurement.
#' @param liveCode character, Code for live status.
#' @param deadCode character, Code for dead status.
#'
#' @return A data table that contains pass information. TRUE indicates pass, while FALSE indicates
#'         failure.
#'
#' @importFrom data.table ':=' data.table copy rbindlist
#' @author Yong Luo
#' @export
#' @rdname checkLD_remeas
checkLD_remeas <- function(subjectID, measNo, LDStatus,
                           liveCode, deadCode){
  if(length(unique(LDStatus)[!(unique(LDStatus) %in% c(liveCode, deadCode))])){
    stop("LDStatus contains undefined status.")
  }
  processdata <- data.table(subjectID, measNo, LDStatus)
  if(nrow(processdata) != nrow(unique(processdata, by = c("subjectID", "measNo")))){
    stop("Multiple LDs were recorded for one subject/measurement, please check the duplicates.")
  }
  processdata[, lastmeas := max(measNo),
              by = "subjectID"]
  processdata[LDStatus == deadCode, newld := 1]
  processdata[is.na(newld), newld := 0]
  processdata[, ldtotal := sum(newld), by = "subjectID"]
  processdata_live <- processdata[ldtotal == 0,]
  processdata_live[, pass := TRUE]
  processdata <- processdata[ldtotal != 0,]
  processdata_dead <- processdata[LDStatus == deadCode,]
  processdata_dead[, ':='(firstdead = min(measNo)),
                   by = "subjectID"]
  processdata_firstdead <- processdata_dead[measNo == firstdead,
                                            .(subjectID, firstdead = measNo)]
  processdata <- merge(processdata, processdata_firstdead,
                       by = "subjectID", all.x = TRUE)
  processdata_dead <- processdata[measNo >= firstdead,]
  processdata_dead[, ':='(totaldead = sum(newld),
                          totallength = length(newld)),
                   by = "subjectID"]
  processdata_dead[totaldead != totallength, pass := FALSE]
  processdata_dead[is.na(pass), pass := TRUE]
  results <- unique(rbindlist(list(processdata_live[,.(subjectID, pass)],
                                   processdata_dead[,.(subjectID, pass)])),
                    by = "subjectID")
  return(results)
}
