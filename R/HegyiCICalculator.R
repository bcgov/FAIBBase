#' the function to calculate intraspecific and interspecific hegyi competition index both distance and size
#'
#' @param objectID character, The unique object identifier. Must be unique.
#'
#' @param species character, Species code to identify intra and inter-specific competition.
#' @param coordX numeric, The x coordinate.
#' @param coordY numeric, The y coordinate.
#' @param size numeric, The size that used for compute competition index.
#'
#' @param maxRadius numeric, The competition index will been calculated within this radius
#' @param distanceWeight numeric, Define how the compeition sensitive to the distance of a neighbours, ie., crowdness.
#'                                Default is 1, which is same as the original Hegyi index.
#' @param sizeWeight numeric, Define how the compeition scales across all the plots. Default is 0, which means there is no scale.
#'
#'
#' @return a data table that has five columns, plotNumber, treeNumber, Year, IntraH and InterH
#'
#' @importFrom data.table data.table ':='
#' @importFrom dplyr left_join '%>%'
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname HegyiCICalculator
#'
#' @author Yong Luo
#'
HegyiCICalculator <- function(objectID,
                              species,
                              coordX,
                              coordY,
                              size,
                              maxRadius,
                              distanceWeight = 1,
                              sizeWeight = 0){
  # calcuate coordination of each tree
  coordinateData <- data.table(objectID, species, coordX, coordX, size)
  maxSize <- max(coordinateData$size)
  minSize <- min(coordinateData$size)
  focalTrees <- coordinateData[,.(k = 1,
                                  focalID = objectID, focalSpecies = species,
                                  focalX = coordX, focalY = coordY, focalSize = size)]
  neighborTrees <- coordinateData[,.(k = 1,
                                     neighborID = objectID, neighborSpecies = species,
                                     neighborX = coordX, neighborY = coordY, neighborSize = size)]
  treeswithneib <- setkey(focalTrees, k)[setkey(neighborTrees, k), nomatch = 0,
                                         allow.cartesian=TRUE]
  treeswithneib[, k := NULL]
  treeswithneib <- treeswithneib[focalID != neighborID,]
  treeswithneib[, XYDistance := (((focalX - neighborX)^2+(focalY-neighborY)^2)^0.5)]
  treeswithneib <- treeswithneib[XYDistance <= maxRadius,]
  treeswithneib[, neighborHegyi := neighborSize/(focalSize * (XYDistance+0.01)^distanceWeight)]
  treeswithneib_intraspecific <- treeswithneib[focalSpecies == neighborSpecies,]
  treeswithneib_interspecific <- treeswithneib[focalSpecies != neighborSpecies,]
  totalHtable <- data.table(focalID = objectID)
  if(nrow(treeswithneib_intraspecific) > 0){
    IntraHtable <- treeswithneib_intraspecific[,.(focalSize = mean(focalSize),
                                                  orgHegyi = sum(neighborHegyi)),
                                               by = c("focalID")]
    IntraHtable[, intraH := orgHegyi * (exp(maxSize - focalSize)/(maxSize - minSize))^sizeWeight]
    totalHtable <- merge(totalHtable, IntraHtable[,.(focalID, intraH)], by = "focalID", all.x = TRUE)
    totalHtable[is.na(intraH), intraH := 0]
  } else {
    totalHtable[,intraH := 0]
  }
  if(nrow(treeswithneib_interspecific) > 0){
    InterHtable <- treeswithneib_interspecific[,.(focalSize = mean(focalSize),
                                                  orgHegyi = sum(neighborHegyi)),
                                               by = c("focalID")]
    InterHtable[, interH := orgHegyi * (exp(maxSize - focalSize)/(maxSize - minSize))^sizeWeight]
    totalHtable <- merge(totalHtable, InterHtable[,.(focalID, interH)], by = "focalID",
                         all.x = TRUE)
    totalHtable[is.na(interH), interH := 0]
  } else {
    totalHtable[,interH := 0]
  }
  totalHtable[, totalH := intraH + interH]
  setnames(totalHtable, "focalID", "objectID")
  return(totalHtable)
}

