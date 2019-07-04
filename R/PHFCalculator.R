#' Calculate tree per ha factor for both fix and variable area plot
#' 
#' @description Calculates tree per ha factor for both fix and variable area plots.
#' 
#' @param sampleType character, Specifies how the plot is sampled among fixed area plot or variable area plot,
#'                               must be either V for variable area plot or F for fixed area plot.
#' 
#' @param blowUp numeric, Specifies the blowup factor. For fixed area plot, it is calculated as 1/plotarea.
#'                         For variable area plot, it is basal area factor (BAF). 
#'                         
#' @param treeWeight numeric, Specifies whether a tree is zero counted (tree is out),
#'                             one time counted (regular count) or two times counted (double counted) 
#'                             in the walk through sampling protocal.
#'                             
#'                              
#' @param plotWeight numeric, Specifies how a plot is measured, i.e., full plot measured (valued as 1),
#'                             half plot measured (valued as 2) or quarter plot measured (valued as 4).
#'                             
#'                             
#' @param treeBasalArea numeric, When plot is measured using variable area plot, this value must be given, 
#'                                otherwise, can be missing
#'                               
#' @return Tree per ha factor
#' 
#' @importFrom data.table ':=' data.table 
#' @author Yong Luo
#' @export
#' @docType methods
#' @rdname PHFCalculator
#' 
setGeneric("PHFCalculator",
           function(sampleType,
                    blowUp,
                    treeWeight,
                    plotWeight,
                    treeBasalArea) {
             standardGeneric("PHFCalculator")
           })

#' @rdname PHFCalculator
setMethod(
  "PHFCalculator",
  signature = c(sampleType = "character",
                blowUp = "numeric",
                treeWeight = "numeric",
                plotWeight = "numeric",
                treeBasalArea = "numeric"),
  definition = function(sampleType,
                        blowUp,
                        treeWeight,
                        plotWeight,
                        treeBasalArea){
    Data <- data.table(obsindex = 1:length(blowUp),
                       SAMP_TYP = sampleType, BLOWUP = blowUp,
                       PLOT_WT = plotWeight, TREE_WT = treeWeight,
                       BA_TREE = treeBasalArea)
    if(nrow(Data[!(SAMP_TYP %in% c("V", "F"))])){
      stop("sampleType must be either V or F. Please check your input.")
    }
    for(i in c("blowUp", "treeWeight", "plotWeight")){
      if(length(get(i)[is.na(get(i))]) > 0){
        stop("Input ", i, " has NA value. Please check your input")
      }
    }
    if(nrow(Data[SAMP_TYP == "V" & is.na(BA_TREE),]) > 0){
      stop("Non-NA TreeBasalArea values must be provided for variable area plot. Please check your input")
    }
    Data[SAMP_TYP == "V", PHF_TREE := TREE_WT*PLOT_WT*BLOWUP/BA_TREE]
    Data[SAMP_TYP == "F", PHF_TREE := TREE_WT*PLOT_WT*BLOWUP]
    return(Data[order(obsindex),]$PHF_TREE)
  })


#' @export
#' @rdname PHFCalculator
setMethod(
  "PHFCalculator",
  signature = c(sampleType = "character",
                blowUp = "numeric",
                treeWeight = "numeric",
                plotWeight = "numeric",
                treeBasalArea = "missing"),
  definition = function(sampleType,
                        blowUp,
                        treeWeight,
                        plotWeight){
    return(PHFCalculator(sampleType, blowUp, treeWeight, 
                         plotWeight, treeBasalArea = as.numeric(NA)))
  })
