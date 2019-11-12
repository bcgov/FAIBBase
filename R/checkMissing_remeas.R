#' Check the missing measurements in repeatedly measured subject against intended measurements
#'
#' @description This function is to check missing measurements in repeatedly measured
#'              subject against intended measurements. Note that this function allows the
#'              regeneration, which means there may be missing measurements at the begining of
#'              intended masurements. Additionally, the function may allow the missing measurements
#'              after a subject died, depending on how \code{deadCode} is specified.
#'
#' @param subjectID character, Specifies subject ID, such as a tree id.
#' @param measNo numeric, Measurement number with bigger value indicates a later measurement.
#' @param intendedMeasNo numeric, The measurement number that a subject is intended measured.
#' @param deadCode character, The code indicates the subject is dead. This arguement serves two purposes:
#'                 1) switch the function whether check a subject under dead scenario (i.e., the missing
#'                 measurement before last intended measurement). Setting \code{NULL} will turn off dead mode,
#'                 arguement \code{LDStatus} will not be used in the function.
#'                 2) if the dead mode is turn on (i.e., anything but \code{NULL}),
#'                 this term determines which code will be used for dead. In this case, arguement \code{LSStatus}
#'                 must be specified. By default, this term is set as \code{NULL}.
#' @param LDStatus character, Live or dead status. This arguement is called only when \code{deadCode}
#'                 is not set as \code{NULL}.
#'
#' @return A data table that contains pass information. TRUE indicates pass, while FALSE indicates
#'         failure. The table also contains the missing measurements.
#'
#' @importFrom data.table ':=' data.table copy
#' @author Yong Luo
#' @export
#' @rdname checkMissing_remeas
checkMissing_remeas <- function(subjectID, measNo, intendedMeasNo,
                                deadCode = NULL, LDStatus){
  if(is.null(deadCode)){
    thedata <- data.table(subjectID, measNo, LDStatus = 0)
    if(nrow(thedata) != nrow(unique(thedata, by = c("subjectID", "measNo")))){
      stop("Duplicate measurements were recorded for one subject, please check the duplicates.")
    }
  } else {
    thedata <- data.table(subjectID, measNo, LDStatus)
    if(nrow(thedata) != nrow(unique(thedata, by = c("subjectID", "measNo")))){
      stop("Duplicate measurements were recorded for one subject, please check the duplicates.")
    }
    thedata[LDStatus == deadCode, newld := 1]
    thedata[is.na(newld), newld := 0]
    deadcheck <- checkLD_remeas(subjectID = thedata$subjectID,
                                measNo = thedata$measNo,
                                LDStatus = thedata$newld,
                                liveCode = 0,
                                deadCode = 1)
    if(nrow(deadcheck[pass == FALSE,]) > 0){
      stop("Please check the live and dead status before check missing observations.")
    }
    thedata_firstdead <- thedata[newld == 1,]
    thedata_firstdead <- thedata_firstdead[, .(firstdead = min(measNo)),
                                           by = "subjectID"]
    thedata <- merge(thedata, thedata_firstdead,
                     by = "subjectID", all.x = TRUE)
    thedata <- thedata[is.na(firstdead) | measNo <= firstdead,
                       .(subjectID, measNo, LDStatus = newld)]
  }
  fullmeas <- data.table(expand.grid(subjectID = unique(subjectID),
                                     intendedMeasNo = intendedMeasNo))
  twoends <- thedata[,.(firstMeasNo = min(measNo),
                        lastMeasNo = max(measNo),
                        lastDead = max(LDStatus)),
                     by = "subjectID"]

  twoends[, lastMeasNo_intend := lastMeasNo]
  twoends[lastDead != 1, lastMeasNo_intend := max(intendedMeasNo)]

  fullmeas <- merge(fullmeas, twoends,
                    by = "subjectID", all.x = TRUE)
  fullmeas <- fullmeas[firstMeasNo <= intendedMeasNo &
                         lastMeasNo_intend >= intendedMeasNo,]
  fullmeas[, measNo := intendedMeasNo]
  fullmeas <- merge(fullmeas, thedata[,.(subjectID, measNo, obs = 1, LDStatus)],
                    by = c("subjectID", "measNo"), all.x = TRUE)
  fullmeas[,':='(totaloobs = sum(obs, na.rm = TRUE),
                 intendobs = length(obs)),
           by = "subjectID"]
  fullmeas[is.na(obs) & intendedMeasNo > lastMeasNo, missingReason := "missing tail"]
  fullmeas[is.na(obs) & is.na(missingReason), missingReason := "missing middle"]
  results <- rbind(unique(fullmeas[totaloobs == intendobs,
                                   .(subjectID, pass = TRUE, missingMeasNo = NA, missingReason)],
                          by = "subjectID"),
                   fullmeas[is.na(obs),
                            .(subjectID, pass = FALSE, missingMeasNo = measNo, missingReason)])
  results <- results[order(subjectID, missingMeasNo),]
  return(results)
}
