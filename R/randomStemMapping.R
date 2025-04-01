#' The function is to generate stem mapping using random spatial distribution of
#' stratified size group for non-stem-mapped trees.
#'
#' @param objectID character, The unique object identifier. Must be unique.
#'
#' @param size numeric, The size that used for compute competition index.
#'
#' @param noofGroup numeric, Defines how many groups to distribute objects.
#'                           Default is 5.
#' @param plotSize numeric, The plot size for the objects.
#'
#' @param mapSize numeric, The map size to distribute the objects.
#'                         Default is 10000 m2.
#'
#' @return a data table that has five columns, plotNumber, treeNumber, Year, IntraH and InterH
#'
#' @importFrom data.table data.table ':='
#' @importFrom  spatstat.random rpoint
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname randomStemMapping
#'
#' @author Yong Luo

randomStemMapping <- function(objectID,
                              size,
                              noofGroup = 5,
                              plotSize,
                              mapSize = 10000){
  onePlotoneCensusdata <- data.table(objectID, size)
  if(nrow(onePlotoneCensusdata) < noofGroup){
    warning(paste0("The number of objects is less than number of group ", noofGroup, ". 1 is used for noofGroup."))
    noofGroup <- 1
  }
  onePlotoneCensusdata <- onePlotoneCensusdata[order(size),]
  onePlotoneCensusdata[,SizeRank:=1:length(size)]
  if(noofGroup != 1){
    onePlotoneCensusdata[, SizeClass:=cut(SizeRank, seq(min(SizeRank), max(SizeRank),
                                                        length = (noofGroup+1)),
                                          labels = paste("Size",
                                                         1:noofGroup, sep = ""),
                                          include.lowest = TRUE)]
  } else {
    onePlotoneCensusdata[, SizeClass := "Size1"]
  }
  for(i in 1:noofGroup){
    indisizeclass <- onePlotoneCensusdata[SizeClass == paste("Size", i, sep = "")]
    NofTree <- round(nrow(indisizeclass)*mapSize/plotSize)
    pts <- data.table(data.frame(rpoint(NofTree), stringsAsFactors = FALSE))

    pts[,':='(x = sqrt(mapSize)*x - sqrt(mapSize)/2, y = sqrt(mapSize)*y - sqrt(mapSize)/2,
              objectID = sample(unique(indisizeclass$objectID),
                                size = length(x),
                                replace = TRUE))]
    if(i == 1){
      alltreemapping <- pts
    } else {
      alltreemapping <- rbind(alltreemapping, pts)
    }
    rm(indisizeclass, NofTree, pts)
  }
  return(alltreemapping)
}
