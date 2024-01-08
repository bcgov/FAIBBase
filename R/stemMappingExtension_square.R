#' Extend stem mapping to a target area and shape.
#'
#' @description The function extends stem mapping in a square plot to a target area and
#'              shape.
#'
#' @param objectID character, A object's ID, e.g., a tree's ID.
#' @param bearing  numeric, Bearing of a object from the north. It should be between 0 to 360.
#' @param distance numeric, Distance between a object and the centre of the circle.
#' @param plotLength numeric, Length of a square plot.
#' @param targetArea  numeric, Defines the area you may want to extend. The unit of this input is \code{ha}.
#'                    Default is 1 ha.
#' @param targetShape character, Defines the shape of the target area. It currently supports \code{circle}
#'                               and \code{square}. The default is \code{square}.
#' @param randomRotate logical, Defines whether need to random rotate the hexagon when merge into
#'                              a \code{targetArea}. The default is \code{FALSE}.
#'
#' @return  A table contains the x and y for all the objects in the extended area.
#'
#' @examples
#' \dontrun{
#' ## randomly generate some trees
#' library(data.table)
#' smallplottrees <- data.table(tree_id = 1:20,
#'                              angle = runif(20, min = 0, max = 360),
#'                              distance = runif(20, min = 0, max = 5.6))
#' ## extend it to 1 ha
#' treelist_smallplot <- stemMappingExtension_square(objectID = smallplottrees$tree_id,
#'                                            bearing = smallplottrees$angle,
#'                                            distance = smallplottrees$distance,
#'                                            radius = 5.64,
#'                                            randomRotate = TRUE)
#'
#'
#' bigplottrees <- data.table(tree_id = 1:20,
#'                            angle = runif(20, min = 0, max = 360),
#'                            distance = runif(20, min = 0, max = 11.28))
#' ## extend it to 1 ha
#' treelist_bigplot <- stemMappingExtension_square(objectID = bigplottrees$tree_id,
#'                                          bearing = bigplottrees$angle,
#'                                          distance = bigplottrees$distance,
#'                                          radius = 11.28)
#' treelist_smallplot[, source := "smallplot"]
#' treelist_bigplot[, source := "bigplot"]
#'
#'
#' alltreelist <- rbind(treelist_bigplot, treelist_smallplot)
#'
#'
#' alltreeplot <- ggplot(data = alltreelist, aes(x, y))+
#'  geom_point(aes(col = factor(source)))
#'
#' }
#'
#' @importFrom data.table data.table shift
#' @importFrom sf st_as_sf st_cast st_combine st_intersection
#' @importFrom dplyr summarise
#'
#' @export
#' @docType methods
#' @rdname stemMappingExtension_square
#'
#' @author Yong Luo
stemMappingExtension_square <- function(objectID, bearing, distance,
                                        plotLength, ## large plot of CMI standard 11.28 m
                                        targetArea = 1, ## unit is ha, defines how big the target extended plot
                                        targetShape = "square",
                                        randomRotate = FALSE){
  if(tolower(targetShape) == "circle"){
    maxRadius <- sqrt(targetArea*10000/pi)
    needDonut <- ceiling(maxRadius/plotLength)
    targetPolygon <- data.table::data.table(angle = seq(0, 359.9, 0.5),
                                            distance = sqrt(targetArea*10000/pi))
    targetPolygon[, ':='(x = sin(angle*pi/180)*distance,
                         y = cos(angle*pi/180)*distance)]
    targetPolygon <- data.frame(targetPolygon) %>%
      st_as_sf(coords = c("x", "y")) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

  } else if(tolower(targetShape) == "square"){
    halfLength <- sqrt(targetArea*10000)/2
    needDonut <- ceiling(halfLength/plotLength)
    targetPolygon <- data.table(matrix(data = c(halfLength, halfLength,
                                                halfLength, -halfLength,
                                                -halfLength, -halfLength,
                                                -halfLength, halfLength),
                                       byrow = TRUE, nrow = 4))
    names(targetPolygon) <- c("x", "y")
    targetPolygon <- data.frame(targetPolygon) %>%
      st_as_sf(coords = c("x", "y")) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
    rm(halfLength)
  }
  initialStemMapping <- data.table::data.table(objectID,
                                               distance,
                                               bearing)
  initialStemMapping[,':='(x = sin(bearing*pi/180)*distance,
                           y = cos(bearing*pi/180)*distance)]


  initialStemMapping <- initialStemMapping[,.(k = 1,
                                              objectID, x, y, distance, bearing)]


  allcenters <- data.table(expand.grid(x_move = seq(-needDonut, needDonut, by = 1)*plotLength,
                                       y_move = seq(-needDonut, needDonut, by = 1)*plotLength))
  allcenters <- allcenters[!(x_move == 0 & y_move == 0), ]


  if(randomRotate){
    allcenters[, ':='(k = 1,
                      squareID = 1:(length(x_move)),
                      rotateAngle = sample(0:3, size = length(x_move),
                                           replace = TRUE))]
    allcenters[, rotateAngle := rotateAngle*90]
  } else {
    allcenters[, ':='(k = 1,
                      squareID = 1:(length(x_move)),
                      rotateAngle = 0)]
  }
  newstemmap <- merge(initialStemMapping, allcenters,
                      by = "k", allow.cartesian = TRUE)
  newstemmap[, bearing := bearing - rotateAngle]
  newstemmap[,':='(x = sin(bearing*pi/180)*distance,
                   y = cos(bearing*pi/180)*distance)]
  newstemmap[,':='(x = x + x_move,
                   y = y + y_move,
                   distance = NULL,
                   bearing = NULL,
                   rotateAngle = NULL)]
  allstemmap <- rbind(initialStemMapping[,.(squareID = 0, objectID, x, y)],
                      newstemmap[,.(squareID, objectID, x, y)])
  allstemmap[,':='(x_temp = x,
                   y_temp = y)]
  allstemmap <- data.frame(allstemmap) %>%
    st_as_sf(coords = c("x", "y"))

  allstemmap <- sf::st_intersection(allstemmap, targetPolygon) %>% data.table
  allstemmap <- allstemmap[,.(squareID, objectID,
                              x = x_temp,
                              y = y_temp)]
  return(allstemmap)
}
