#' Estimate tree height for a broken top tree when projected tree height is available
#'
#'
#' @description This function is to esimate a broken top tree's height based on projected tree
#'              height in the field (\code{heightProjected}). For the broken top trees that have diameter at broken
#'              and broken top trees, total tree height also can be esimated using \code{\link{heightEstimateForBTOP_D}}.
#'
#' @param heightProjected numeric, Projected tree height in the field, must be non-NA value.
#'
#' @return Total tree height
#'
#' @seealso \code{\link{heightEstimateForBTOP_D}}
#' @importFrom data.table ':='
#'
#'
#' @export
#' @docType methods
#' @rdname heightEstimateForBTOP_H
#'
#' @author Yong Luo
heightEstimateForBTOP_H<- function(heightProjected){
  if(length(heightProjected[heightProjected <= 0 & !is.na(heightProjected)]) > 0){
    stop("Projected tree height contains non-positive values.")
  }
  if(length(heightProjected[is.na(heightProjected)]) > 0){
    warning("Projected tree height contains NA values, Estimated tree height is assigned as NA.")
  }
  return(heightProjected)
}




#' Estimate tree height for a broken top tree when DBH, inside bark diamater at broken top height, height at broken
#' are available
#'
#' @description This is the second function to estimate a tree's height for a broken top tree.
#'              A tree's height is esimated using height of the broken top (\code{heightBTOP}), inside bark diameter at broken height (\code{DIBBTOP})
#'              and \code{DBH}. Specifically, this function guesses the tree height,
#'               computes inside bark diameter at broken height (\code{heightBTOP}) using a taper equation,
#'               compares it to an observed inside bark diameter and chooses the tree height that has closest value of inside bark diameter at broken.
#'              For the broken top trees that have field projected height, total tree height also can be estimated using
#'              \code{\link{heightEstimateForBTOP_H}}.
#' @param heightBTOP numeric, Height of the broken top.
#'
#'
#' @param DIBBTOP numeric, Diameter inside bark at the height of the broken top.
#'
#' @param DBH numeric, DBH of the tree, Must be given when BTOP is D.
#'
#' @param taperEquationForm character, Specifies which taper equaiton will be used to estimate
#'                                     tree height, currently supports KBEC, KBECQCI, KFIZ.
#'                                     If missing, the function uses KBEC as default.
#'
#' @param FIZorBEC character, Specifies which FIZ or BEC (depends on taperEquationForm) zones the tree located.
#'
#' @param species character, Tree species.
#'
#' @param volMultiplier numeric, Volume adjustment. If missing, 1 will be used.
#'
#' @param SASOriginal logical, Specifies whether the original sas algrithm will be used for guess tree height
#'                             If missing, FALSE will be used.
#'
#' @return Total tree height
#'
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare %>>% %<<% %==% %>=% %<=%
#' @seealso \code{\link{heightEstimateForBTOP_H}}
#'
#'
#' @export
#' @docType methods
#' @rdname heightEstimateForBTOP_D
#' @author Yong Luo
heightEstimateForBTOP_D <- function(heightBTOP, DIBBTOP, DBH,
                                    taperEquationForm = "KBEC",
                                    FIZorBEC,
                                    species, volMultiplier = 1,
                                    SASOriginal = FALSE){
  BTOPTreeData <- data.table(uniObsID = 1:max(length(heightBTOP), length(DIBBTOP), length(DBH)),
                             HT_BTOP = heightBTOP,
                             DIB_BTOP = DIBBTOP,
                             DBH = DBH,
                             FIZorBEC = FIZorBEC,
                             SP0 = species,
                             VOL_MULT = volMultiplier)

  ## brief check the data
  if(nrow(BTOPTreeData[HT_BTOP <= 0 | is.na(HT_BTOP), ])){
    stop("Height at broken top must be positive values.")
  }
  if(nrow(BTOPTreeData[DIB_BTOP <= 0 | is.na(DIB_BTOP), ])){
    stop("Diameter at broken top height must be positive values.")
  }
  if(nrow(BTOPTreeData[DBH <= 0 | is.na(DBH), ])){
    stop("DBH must be positive values.")
  }

  if(nrow(BTOPTreeData[DIB_BTOP > DBH,]) > 0){
    warning("Diameter inside bark at broken top height is bigger than DBH. Tree height is generated as NA.")
  }
  if(nrow(BTOPTreeData[HT_BTOP > 60,]) > 0){
    warning("Broken top height was greater than 60 m. Tree height is generated as NA")
  }
  if(nrow(BTOPTreeData[HT_BTOP < 1.4,]) > 0){
    warning("Broken top height was smaller than 1.4 m. Tree height is generated as NA")
  }
  BTOPTreeData_NAs <- BTOPTreeData[DIB_BTOP > DBH | HT_BTOP > 60 | HT_BTOP < 1.4 | is.na(DIB_BTOP), ]
  BTOPTreeData_NAs[, estimatedHT := NA]
  BTOPTreeData <- BTOPTreeData[!(uniObsID %in% BTOPTreeData_NAs$uniObsID),]

  if(taperEquationForm == "KFIZ3"){
    setnames(BTOPTreeData, "FIZorBEC", "FIZ")
    setnames(BTOPTreeData_NAs, "FIZorBEC", "FIZ")
  } else if(taperEquationForm %in% c("KBEC")){
    setnames(BTOPTreeData, "FIZorBEC", "BEC")
    setnames(BTOPTreeData_NAs, "FIZorBEC", "BEC")
  } else {
    stop("taperEquationForm is not correctly specified, it must be either KFIZ3 or KBEC")
  }

  if(SASOriginal){
    BTOPTreeData[, ':='(BTOP_ITERATION = 0, BTOP_ACTION_FLAG = "0", HT_I = HT_BTOP,
                        HT_ESTIMATE = HT_BTOP*(DBH/(DBH-DIB_BTOP))+0.3,
                        HT_ESTIMATE_MIN = HT_BTOP+0.1)]

    BTOPTreeData[, ':='(HT_ESTIMATE_MAX = pmin(HT_ESTIMATE*3, 76))]
    BTOPTreeData[, ':='(HT_ESTIMATE = HT_ESTIMATE_MAX)]

    BTOPTreeData[, ':='(HT_EST_LAST = HT_ESTIMATE,
                        HT_EST_INCR = (HT_ESTIMATE_MAX-HT_ESTIMATE_MIN)/10,
                        estimatedHT = as.numeric(NA),
                        DIB_I = as.numeric(NA),
                        DIB_ITERATION_DIFF = as.numeric(NA))]
    newTreeData <- BTOPTreeData[0,]
    for(i in 1:50){ # maximum 50 loops, as indicated in sas codes
      if(nrow(BTOPTreeData) > 0){ # to save the time, the looping is done on condition of
        # BTOPTreeData has rows
        BTOPTreeData[, ':='(estimatedHT = HT_ESTIMATE,
                            BTOP_ITERATION = BTOP_ITERATION + 1)]
        ## for preferred stopping condition, store the completed rows into newTreeData as output
        newTreeData <- rbind(newTreeData,
                             BTOPTreeData[HT_EST_INCR < 0.101, ][, BTOP_ACTION_FLAG := "*"])
        BTOPTreeData <- BTOPTreeData[HT_EST_INCR >= 0.101, ]
        #proceed for rest of them
        if(nrow(BTOPTreeData) > 0){
          ## for emergency stopping condition
          newTreeData <- rbind(newTreeData,
                               BTOPTreeData[estimatedHT < HT_BTOP, ][, BTOP_ACTION_FLAG := "X"])
          BTOPTreeData <- BTOPTreeData[estimatedHT >= HT_BTOP, ]
          BTOPTreeData[, DIB_I := DIB_ICalculator(taperEquationForm = taperEquationForm,
                                                  FIZorBEC = BEC, species = SP0,
                                                  DBH, height_I = HT_I,
                                                  heightTotal = estimatedHT, volMultiplier = VOL_MULT)]

          BTOPTreeData[, DIB_ITERATION_DIFF := DIB_I - DIB_BTOP]
          newTreeData <- rbind(newTreeData,
                               BTOPTreeData[abs(DIB_ITERATION_DIFF) < 0.0005, # in document 0.5
                                            ][, ':='(BTOP_ACTION_FLAG = "E")])

          BTOPTreeData <- BTOPTreeData[abs(DIB_ITERATION_DIFF) >= 0.0005,]
          BTOPTreeData[DIB_ITERATION_DIFF > 0, HT_EST_LAST := HT_ESTIMATE]
          BTOPTreeData[DIB_ITERATION_DIFF > 0, HT_ESTIMATE := HT_ESTIMATE - HT_EST_INCR]

          BTOPTreeData[DIB_ITERATION_DIFF < 0, ':='(HT_ESTIMATE = HT_EST_LAST,
                                                    HT_EST_INCR = HT_EST_INCR/2)]
        }
        ## call volTreeActiveEquation function to calculate volume
        # to calculate DIB_I
      }
    }
    ## for trees that have not obtained Height information after 50 loops
    if(nrow(BTOPTreeData) > 0){
      newTreeData <- rbind(newTreeData, BTOPTreeData)
    }
    rm(BTOPTreeData)
  } else {
    ## the following method is using midpoint to guess the possible height
    BTOPTreeData[, ':='(BTOP_ITERATION = 0, BTOP_ACTION_FLAG = "0", HT_I = HT_BTOP,
                        HT_ESTIMATE_MIN = HT_BTOP,
                        HT_ESTIMATE_MAX = 66.6)]

    BTOPTreeData[, ':='(HT_ESTIMATE = HT_ESTIMATE_MIN + (HT_ESTIMATE_MAX - HT_ESTIMATE_MIN)/2,
                        HT_EST_INCR = (HT_ESTIMATE_MAX - HT_ESTIMATE_MIN)/2,
                        estimatedHT = as.numeric(NA),
                        DIB_I = as.numeric(NA),
                        DIB_ITERATION_DIFF = as.numeric(NA))]

    newTreeData <- BTOPTreeData[0,]
    while(nrow(BTOPTreeData) > 0){
      BTOPTreeData[, ':='(estimatedHT = HT_ESTIMATE,
                          BTOP_ITERATION = BTOP_ITERATION + 1)]

      ## for preferred stopping condition, store the completed rows into newTreeData as output
      newTreeData <- rbind(newTreeData,
                           BTOPTreeData[HT_EST_INCR %<<% 0.1, ][, BTOP_ACTION_FLAG := "*"])
      #proceed for rest of them
      BTOPTreeData <- BTOPTreeData[HT_EST_INCR %>=% 0.1, ]

      if(nrow(BTOPTreeData) > 0){
        BTOPTreeData[, DIB_I := DIB_ICalculator(taperEquationForm = taperEquationForm,
                                                FIZorBEC = BEC, species = as.character(SP0),
                                                DBH, height_I = HT_I,
                                                heightTotal = estimatedHT, volMultiplier = VOL_MULT)]
        BTOPTreeData[, DIB_ITERATION_DIFF := DIB_I - DIB_BTOP]
        newTreeData <- rbind(newTreeData,
                             BTOPTreeData[abs(DIB_ITERATION_DIFF) %<<% 0.01, # in document 0.01
                                          ][, ':='(BTOP_ACTION_FLAG = "E")])

        ## continue with the big different data
        BTOPTreeData <- BTOPTreeData[abs(DIB_ITERATION_DIFF) %>=% 0.01, ]
        ## two situations:
        # 1. with DIB_ITERATION_DIFF > 0, which means DBI_I is bigger than observed,
        # hence, total tree height should be reduced
        BTOPTreeData[,':='(HT_EST_INCR = HT_EST_INCR/2)] # midpoint change

        BTOPTreeData[DIB_ITERATION_DIFF %>>% 0, ':='(HT_ESTIMATE = estimatedHT - HT_EST_INCR)]
        BTOPTreeData[DIB_ITERATION_DIFF %<<% 0, ':='(HT_ESTIMATE = estimatedHT + HT_EST_INCR)]
      }
    }
  }
  newTreeData <- rbind(BTOPTreeData_NAs,
                       newTreeData[, names(BTOPTreeData_NAs), with = FALSE])
  return(newTreeData[order(uniObsID),]$estimatedHT)
}
