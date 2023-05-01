#' Calculate volume for trees
#'
#' @description This function is to calculate tree volume using taper equations on a basis of 10 cm slice. As default, the function is to calculate whole tree volume
#'              (\code{VOL_WSV}), total merchantable volume (\code{VOL_BELOW_UTOP}) and non-merchantable volume
#'              (\code{VOL_ABOVE_UTOP}) based on \code{FIZorBEC}, \code{species}, \code{height}, \code{DBH} using Kozak BEC taper equations.
#'              The function also handles broken top trees by specifying \code{BTOPEstimateType}, \code{BTOPHeight} and \code{BTOPDIB}.
#'              Accordingly, \code{VOL_BELOW_BTOP} and \code{VOL_ABOVE_BTOP} are produced.
#'              Lastly, the function derives volume (denoted as \code{LOG_V_X}), merchantable volume
#'              (denoted as \code{LOG_VM_X}) and top inside bark diameter (denoted as \code{LOG_D_X}) for each log when the \code{logLengthMatrix}
#'              is provided. For all the scenarioes, stump height (\code{HT_STUMP}), inside bark diameter at stump height (\code{DIB_STUMP}),
#'              breast height (\code{HT_BH}), inside bark diameter at breast height (\code{DIB_BH}) are generated.
#'
#' @param FIZorBEC character, Specifies which FIZ or BEC (depends on taperEquation) zones the tree located in BC.
#'
#' @param species character, Tree species, must be BC species code.
#'
#' @param height numeric, Total tree height in meter.
#'
#' @param DBH numeric, DBH of the tree in cm.
#'
#' @param taperEquationForm character, Specifies which taper equaiton will be used, currently support KFIZ3 or KBEC.
#'        See function \code{DIB_ICalculator} for details. Default is KBEC, if missing.
#'
#' @param volMultiplier numeric, Volume adjustment multiplier. If missing, 1 (no adjustment) is used.
#'
#' @param stumpHeight numeric, Defines stump height. If missing, 0.3 m is used.
#'
#' @param breastHeight numeric, Defines the breast height. If missing, 1.3 m is used.
#'
#' @param UTOPDIB numeric, Merchantable inside-bark diameter. If missing, UTOP is 10.
#'
#' @param BTOPEstimateType integer, Must among NA, 1, 2, 3. Defines whether a tree has broken top and which field observation (height at broken or DIB at broken )
#'                         is used to define broken point. NA means that tree is not broken top. 1 and 3 means diameter at broken top is not available,
#'                         height at broken top is used to define broken point. 2 means diameter at broken top is available and is used to define broken point.
#'                         Default is NA: tree does not have broken top.
#'
#' @param BTOPHeight numeric, Height at broken top.
#'
#' @param BTOPDIB numeric, Diameter inside bark at height of broken top.
#'
#' @param logLengthMatrix data.table, Log length matrix. If missing, there is no log-level volume returned.
#'
#' @param logMinLength numeric, Minimum log length. This argument is activated when logLengthMatrix is provided.
#'
#' @return A volume table
#'
#'
#' @importFrom data.table ':=' set setnames data.table
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @author Yong Luo
#'
#'
#' @export
#' @docType methods
#' @rdname treeVolCalculator
#'
treeVolCalculator <- function(FIZorBEC, species, height, DBH, taperEquationForm = "KBEC",
                              volMultiplier = 1, stumpHeight = 0.3, breastHeight = 1.3,
                              UTOPDIB = 10, BTOPEstimateType = NA, BTOPHeight = NA,
                              BTOPDIB = NA, logLengthMatrix = data.table(Log1_L = numeric()),
                              logMinLength = 3){
  if(length(unique(BTOPEstimateType)[!(unique(BTOPEstimateType) %in% c(NA, 0, 1, 2, 3))])){
    stop("Invalid input for BTOPEsitmateType, must be among NA, 1, 2, and 3")
  }
  processedData <- data.table(uniObsID = 1:max(length(DBH), length(height)),
                              FIZorBEC = FIZorBEC,
                              SP0 = species,
                              HT = height,
                              DBH = DBH,
                              VOL_MULT = volMultiplier,
                              HT_STUMP = stumpHeight,
                              DIB_STUMP = as.numeric(NA),
                              VOL_STUMP = as.numeric(NA),
                              HT_BH = breastHeight,
                              DIB_BH = as.numeric(NA),
                              HT_UTOP = as.numeric(NA),
                              DIB_UTOP = UTOPDIB)
  vcons <- pi*(1/(2*100)^2)
  if(length(unique(BTOPEstimateType)) == 1){ # default of BTOPEstmateType is NA
    if(is.na(unique(BTOPEstimateType))){
      brokenTopInput <- FALSE ## all trees are non-broken top trees, ignore broken top tree process
    } else {
      brokenTopInput <- TRUE
    }
  } else {
    brokenTopInput <- TRUE
  }
  if(brokenTopInput){
    processedData <- cbind(processedData,
                           data.table(BTOP_ESTIMATE_TYPE = BTOPEstimateType,
                                      HT_BTOP = BTOPHeight, # will be used when btop_estimate_type is 1 or 3
                                      DIB_BTOP = BTOPDIB,
                                      VOL_WSV = 0,
                                      VOL_BELOW_UTOP = 0,
                                      VOL_ABOVE_UTOP = 0,
                                      VOL_BELOW_BTOP = 0,
                                      VOL_ABOVE_BTOP = 0)) # will be used when btop_estimate_type is 2
  } else {
    processedData[,':='(VOL_WSV = 0,
                        VOL_BELOW_UTOP = 0,
                        VOL_ABOVE_UTOP = 0)]
  }
  if(nrow(logLengthMatrix) > 0){ # default is logLenthMatrix is empty
    logMatrixInput <- TRUE
    logsMax <- ncol(logLengthMatrix) # logsMax is determined through the logLengthMatrix
    names(logLengthMatrix) <- paste("LOG_L_", 0:(logsMax-1), sep = "")
    processedData <- cbind(processedData, logLengthMatrix)
    if(nrow(logLengthMatrix[is.na(LOG_L_1),]) > 0){
      warning("Length of the first log was not available, NA was assigned.")
    }
  } else {
    logMatrixInput <- FALSE
  }
  if(taperEquationForm == "KFIZ3"){
    setnames(processedData, "FIZorBEC", "FIZ")
  } else if (taperEquationForm == "KBEC"){
    setnames(processedData, "FIZorBEC", "BEC")
  } else {
    stop("Invalid input for taperEquationForm, must be either KFIZ3 or KBEC")
  }
  ## collect invalid data
  invalidData <- processedData[0,]
  ## tree height checking
  if(nrow(processedData[is.na(HT) | HT %>>% 90 | HT %<=%1.4,]) > 0){
    warning(paste("There were ", nrow(processedData[is.na(HT) | HT %>>% 90 | HT %<=% 1.4,]),
                  " observations had invalid height (NA, >90 or <1.4). Tree volume was assigned as NA.", sep = ""))
  }
  invalidData <- rbindlist(list(invalidData,
                                processedData[is.na(HT) | HT %>>% 90 | HT %<=%1.4,]))
  processedData <- processedData[!(uniObsID %in% invalidData$uniObsID),]

  ## DBH checking
  if(nrow(processedData[is.na(DBH) | DBH %>>% 1000 | DBH %<=% 1,]) > 0){
    warning(paste("There were ", nrow(processedData[is.na(DBH) | DBH %>>% 1000 | DBH %<=% 1,]),
                  " observations had invalid DBH (NA, >1000 or <1). Tree volume was assigned as NA.", sep = ""))
  }
  invalidData <- rbindlist(list(invalidData,
                                processedData[is.na(DBH) | DBH %>>% 1000 | DBH %<=% 1,]))
  processedData <- processedData[!(uniObsID %in% invalidData$uniObsID),]

  if(brokenTopInput){
    ## height at broken top checking
    if(nrow(processedData[!is.na(BTOP_ESTIMATE_TYPE) & HT_BTOP %>>% HT,]) > 0){
      warning("Invalid! Height at broken top was taller than tree height. Tree volume was assigned as NA.")
    }
    invalidData <- rbindlist(list(invalidData,
                                  processedData[!is.na(BTOP_ESTIMATE_TYPE) & HT_BTOP %>>% HT,]))
    processedData <- processedData[!(uniObsID %in% invalidData$uniObsID),]
  }
  invalidData[, c("VOL_WSV", "VOL_BELOW_UTOP", "VOL_ABOVE_UTOP",
                  "VOL_BELOW_BTOP", "VOL_ABOVE_BTOP") := as.numeric(NA)]
  if(nrow(processedData) > 0){
    processedData[, HT_I := stumpHeight]
    processedData[, DIB_I := DIB_ICalculator(taperEquationForm, FIZorBEC = BEC, species = SP0,
                                             height_I = HT_I, heightTotal = HT, DBH = DBH,
                                             volMultiplier = VOL_MULT)]
    ## add constrains to make sure all the DIB below 1.3 m is bigger than or equal to DIB_BH
    ## otherwise, force it to DIB_BH
    processedData[, DIB_BH := DIB_ICalculator(taperEquationForm, FIZorBEC = BEC, species = SP0,
                                             height_I = breastHeight, heightTotal = HT, DBH = DBH,
                                             volMultiplier = VOL_MULT)]
    processedData[DIB_BH > DIB_I,
                  DIB_I := DIB_BH]

    processedData[, ':='(DIB_STUMP = DIB_I,
                         DIB_I_LAST = DIB_I)]
    processedData[, VOL_STUMP := vcons * (HT_I) * (DIB_STUMP^2)]
    processedData[ , ':='(VOL_WSV = VOL_STUMP)]

    if(logMatrixInput){
      processedData[, paste("LOG_V_", 0:(logsMax-1), sep = "") := as.numeric(NA)]
      processedData[, paste("LOG_VM_", 0:(logsMax-1), sep = "") := as.numeric(NA)]
      processedData[, paste("LOG_D_", 0:(logsMax-1), sep = "") := as.numeric(NA)]
      processedData[, ':='(LOG_V_0 = VOL_STUMP, # _VL_LG{0} gross volume of stump
                           LOG_VM_0 = 0, # _VM_LG{0} merch volume of stump
                           LOG_D_0 = DIB_STUMP,
                           LOG_ID = 1L,
                           HT_LOGS = LOG_L_1+stumpHeight,
                           LOG_UTOP = as.integer(NA))] #_DB_LG{0}
      if(brokenTopInput){
        processedData[, LOG_BTOP := as.integer(NA)]
      }
    }
    newdata <- processedData[0, ]
    ## loop for 10 cm slices
    while(nrow(processedData) > 0){
      processedData[, HT_I := HT_I + 0.1]
      processedData[, DIB_I := DIB_ICalculator(taperEquationForm, FIZorBEC = BEC, species = SP0,
                                               height_I = HT_I, heightTotal = HT, DBH = DBH,
                                               volMultiplier = VOL_MULT)]
      ## add constrains to make sure all the DIB below 1.3 m is bigger than or equal to DIB_BH
      ## otherwise, force it to DIB_BH, see email from Rene on April 27, 2023
      processedData[, DIB_BH := DIB_ICalculator(taperEquationForm, FIZorBEC = BEC, species = SP0,
                                                height_I = breastHeight, heightTotal = HT, DBH = DBH,
                                                volMultiplier = VOL_MULT)]
      processedData[DIB_BH > DIB_I & HT_I < breastHeight,
                    DIB_I := DIB_BH]

      processedData[, VOL_I := vcons * (0.1) * (DIB_I_LAST^2 + DIB_I^2)/2]
      processedData[, ':='(VOL_WSV = VOL_WSV + VOL_I,
                           DIB_I_LAST = DIB_I)]
      if(brokenTopInput){
        ## identify below and above broken top by height
        ## btop_estimate_type belongs to 1 or 3
        if(logMatrixInput){
          processedData[BTOP_ESTIMATE_TYPE %in% c(1L, 3L) & HT_I %<<% HT_BTOP,
                        ':='(VOL_BELOW_BTOP = VOL_BELOW_BTOP + VOL_I,
                             LOG_BTOP = LOG_ID)]
        } else {
          processedData[BTOP_ESTIMATE_TYPE %in% c(1L, 3L) & HT_I %<<% HT_BTOP,
                        ':='(VOL_BELOW_BTOP = VOL_BELOW_BTOP + VOL_I)]
        }
        processedData[BTOP_ESTIMATE_TYPE %in% c(1L, 3L) & HT_I %>=% HT_BTOP,
                      VOL_ABOVE_BTOP := VOL_ABOVE_BTOP + VOL_I]
        ## identify below and above broken top by dib at the broken height
        ## btop_estimate_type belongs to 2
        processedData[BTOP_ESTIMATE_TYPE %in% c(2) & DIB_I %>=% DIB_BTOP,
                      ':='(VOL_BELOW_BTOP = VOL_BELOW_BTOP + VOL_I)]
        processedData[BTOP_ESTIMATE_TYPE %in% c(2) & DIB_I %<<% DIB_BTOP,
                      VOL_ABOVE_BTOP := VOL_ABOVE_BTOP + VOL_I]
        if(logMatrixInput){
          processedData[BTOP_ESTIMATE_TYPE %in% c(1, 3) & HT_I %<<% HT_BTOP,
                        ':='(LOG_BTOP = LOG_ID)]
          processedData[BTOP_ESTIMATE_TYPE %in% c(2) & DIB_I %>=% DIB_BTOP,
                        ':='(LOG_BTOP = LOG_ID)]
        }
      }
      ## identify below and above utop/merch by dib_utop
      processedData[DIB_I %>=% DIB_UTOP, ':='(VOL_BELOW_UTOP = VOL_BELOW_UTOP + VOL_I,
                                              HT_UTOP = HT_I)]
      processedData[DIB_I %<<% DIB_UTOP, VOL_ABOVE_UTOP := VOL_ABOVE_UTOP + VOL_I]
      processedData[HT_I %==% HT_BH, DIB_BH := DIB_I]


      ### for the loglength matrix
      if(logMatrixInput){
        processedData[, ':='(LOG_UTOP = LOG_ID)]
        for(indilogid in unique(processedData$LOG_ID)){
          processedData[LOG_ID == indilogid, paste("LOG_D_", indilogid, sep = "") := DIB_I]
          processedData[LOG_ID == indilogid & DIB_I %>=% DIB_UTOP,
                        paste("LOG_VM_", indilogid, sep = "") := rowSums(processedData[LOG_ID == indilogid & DIB_I >= DIB_UTOP,
                                                                                       c(paste("LOG_VM_", indilogid, sep = ""),
                                                                                         "VOL_I"), with = FALSE],
                                                                         na.rm = TRUE)]
          processedData[LOG_ID == indilogid,
                        paste("LOG_V_", indilogid, sep = "") := rowSums(processedData[LOG_ID == indilogid,
                                                                                      c(paste("LOG_V_", indilogid, sep = ""),
                                                                                        "VOL_I"), with = FALSE],
                                                                        na.rm = TRUE)]
        }
        rm(indilogid)
        processedData[HT_LOGS %<<% HT_I, LOG_ID := LOG_ID + 1L]
        for(indilogid in unique(processedData[HT_LOGS %<<% HT_I,]$LOG_ID)){
          processedData[HT_LOGS %<<% HT_I & LOG_ID == indilogid,
                        HT_LOGS := HT_LOGS + processedData[HT_LOGS %<<% HT_I & LOG_ID == indilogid,
                                                           paste("LOG_L_", indilogid, sep = ""), with = FALSE]]
        }
        rm(indilogid)
      }
      newdata <- rbind(newdata,
                       processedData[HT_I %>=% HT, ][, VOL_I := NULL])
      processedData <- processedData[HT_I %<<% HT, ]
    }
    if(logMatrixInput){
      newdata[, VOL_PSP_MERCH := 0]
      for(indilog in 0:(logsMax-1)){
        newdata[, ':='(tempLogLength = unlist(newdata[, paste(c("LOG_L_"), indilog, sep = ""), with = FALSE]),
                       templogmvol = unlist(newdata[, paste("LOG_VM_", indilog, sep = ""), with = FALSE]))]
        newdata[is.na(tempLogLength) | tempLogLength %<<% logMinLength, templogmvol := 0]
        newdata[, VOL_PSP_MERCH := VOL_PSP_MERCH + templogmvol]
        newdata[, ':='(tempLogLength = NULL, templogmvol = NULL)]
      }
      set(newdata, , c(paste("LOG_L_", 0:(logsMax-1), sep = ""), "HT_LOGS", "LOG_ID"), NULL)
    }
    set(newdata, , c("BEC", "SP0", "HT", "DBH", "HT_I",
                     "VOL_MULT",  "DIB_I", "DIB_I_LAST"), NULL)
    if(brokenTopInput){
      set(newdata, , c("BTOP_ESTIMATE_TYPE", "HT_BTOP", "DIB_BTOP"), NULL)
    }
    invalidatacols <- names(newdata)[!(names(newdata) %in% "uniObsID")]
    invalidData <- invalidData[,.(uniObsID)]
    set(invalidData, , invalidatacols, NA)
    alldata <- rbindlist(list(newdata, invalidData))
    alldata[(HT_UTOP %<<% (logMinLength + HT_STUMP)) | is.na(VOL_BELOW_UTOP), VOL_BELOW_UTOP := 0]
  } else {
    alldata <- invalidData
  }
  alldata <- alldata[order(uniObsID),]
  return(alldata[, uniObsID := NULL])
}
