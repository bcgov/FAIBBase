#' Extend stem mapping to a target area and shape.
#'
#' @description The function cuts a circle stem map into a largest hexagon in circle and extends this hexagon to a target area and
#'              shape. This is a generic function.
#'
#' @param objectID character, A object's ID, e.g., a tree's ID.
#' @param bearing  numeric, Azimuth of a object from the north. It should be between 0 to 360.
#' @param distance numeric, Distance between a object and the centre of the circle.
#' @param plotRadius numeric, Radius of the plot circle. If missing, \code{11.28} will be used, as it presents the
#'                        radius of a circle for big trees (i.e., trees DBH >= 9).
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
#' treelist_smallplot <- stemMappingExtension(objectID = smallplottrees$tree_id,
#'                                            bearing = smallplottrees$angle,
#'                                            distance = smallplottrees$distance,
#'                                            plotRadius = 5.64,
#'                                            randomRotate = TRUE)
#'
#'
#' bigplottrees <- data.table(tree_id = 1:20,
#'                            angle = runif(20, min = 0, max = 360),
#'                            distance = runif(20, min = 0, max = 11.28))
#' ## extend it to 1 ha
#' treelist_bigplot <- stemMappingExtension(objectID = bigplottrees$tree_id,
#'                                          bearing = bigplottrees$angle,
#'                                          distance = bigplottrees$distance,
#'                                          plotRadius = 11.28)
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
#' @importFrom sp SpatialPoints SpatialPointsDataFrame SpatialPolygons Polygons Polygon
#' @importFrom data.table data.table shift
#' @importFrom rgeos gArea
#' @importFrom raster intersect
#'
#' @export
#' @docType methods
#' @rdname stemMappingExtension
#'
#' @author Yong Luo

stemMappingExtension <- function(objectID, bearing, distance,
                                 plotRadius = 11.28, ## large plot of CMI standard 11.28 m
                                 targetArea = 1, ## unit is ha, defines how big the target extended plot
                                 targetShape = "square",
                                 randomRotate = FALSE){
  if(tolower(targetShape) == "circle"){
    targetPolygon <- data.table::data.table(angle = seq(0, 359.9, 0.5),
                                            distance = sqrt(targetArea*10000/pi))
    targetPolygon[, ':='(x = sin(angle*pi/180)*distance,
                         y = cos(angle*pi/180)*distance)]
    targetPolygon <- SpatialPolygons(list(Polygons(list(Polygon(targetPolygon[,.(x, y)])),
                                                   ID = 0)))
  } else if(tolower(targetShape) == "square"){
    halfLength <- sqrt(targetArea*10000)/2
    targetPolygon <- data.table(matrix(data = c(halfLength, halfLength,
                                                halfLength, -halfLength,
                                                -halfLength, -halfLength,
                                                -halfLength, halfLength),
                                       byrow = TRUE, nrow = 4))
    names(targetPolygon) <- c("x", "y")
    targetPolygon <- SpatialPolygons(list(Polygons(list(Polygon(targetPolygon[,.(x, y)])),
                                                   ID = 0)))
    rm(halfLength)
  }
  initialStemMapping <- data.table::data.table(objectID,
                                               distance,
                                               bearing)
  initialStemMapping[,':='(x = sin(bearing*pi/180)*distance,
                           y = cos(bearing*pi/180)*distance)]
  initialStemMapping <- rbind(initialStemMapping[, NOTE := NA],
                              data.table(objectID = 0,
                                         distance = 0,
                                         bearing = 0,
                                         x = c(0, plotRadius-1),
                                         y = 0,
                                         NOTE = "DUMMY"))

  initialStemMapping <- SpatialPointsDataFrame(initialStemMapping[,.(x,  y)],
                                               data = initialStemMapping)

  central_hexagon <- data.table(angle = seq(from = 30, to = 360, by = 60),
                                distance = plotRadius)
  central_hexagon[, ':='(x = sin(angle*pi/180)*distance,
                         y = cos(angle*pi/180)*distance)]
  central_hexagon <- Polygons(list(Polygon(data.frame(central_hexagon[,.(x, y)]))), ID = 0)
  central_hexagon <- SpatialPolygons(list(central_hexagon))
  initialStemMapping <- raster::intersect(initialStemMapping, central_hexagon)
  initialStemMapping <- initialStemMapping@data %>% data.table
  initialStemMapping <- initialStemMapping[is.na(NOTE),
                                           .(objectID, distance, bearing, x, y)]
  if(nrow(initialStemMapping) == 0){
    warning("No tree found in the maximum hexagon, empty table is returned.")

    return(data.table(hexagonID = numeric(),
                      objectID = numeric(),
                      x = numeric(),
                      y = numeric()))
  } else {
    initialStemMapping <- initialStemMapping[,.(k = 1,
                                                objectID, x, y, distance, bearing)]
    newcenters <- data.table(hexagonID = numeric(),
                             x_move = numeric(),
                             y_move = numeric())
    i <- 1
    needmore <- TRUE
    ## for the first donut
    while(needmore == TRUE){
      newcenters_donut <- data.table(angle = seq(0, 359, by = 60),
                                     distance = i*2*cos(30*pi/180)*plotRadius)
      newcenters_donut <- newcenters_donut[, .(hexagonID = 1:6,
                                               x_move = sin(angle*pi/180)*distance,
                                               y_move = cos(angle*pi/180)*distance)]
      newcenters <- rbind(newcenters, newcenters_donut)
      if(i == 1){
        newcenters_add <- newcenters[0,]
      } else {
        newcenters_add <- newcenters[0,]
        mediumtable <- newcenters_donut[,.(hexagonID, x_start = x_move,
                                           y_start = y_move)]
        mediumtable[, ':='(x_end = shift(x_start, fill = NA, type = "lead"),
                           y_end = shift(y_start, fill = NA, type = "lead"))]
        mediumtable[hexagonID == 6, ':='(x_end = mediumtable$x_start[1],
                                         y_end = mediumtable$y_start[1])]
        for(j in 1:(i-1)){
          newcenters_add_add <- mediumtable[,':='(x_move = x_start + (x_end - x_start)*j/i,
                                                  y_move = y_start + (y_end - y_start)*j/i)]
          newcenters_add <- rbind(newcenters_add,
                                  newcenters_add_add[,.(hexagonID = NA,
                                                        x_move, y_move)])
          rm(newcenters_add_add)
        }
        rm(mediumtable)
      }
      newcenters <- rbind(newcenters, newcenters_add)
      donutPolygon <- data.table(angle = seq(0, 359, by = 60),
                                 distance = (i*2)*cos(30*pi/180)*plotRadius+0.5*plotRadius)
      donutPolygon <- donutPolygon[, .(x_move = sin(angle*pi/180)*distance,
                                       y_move = cos(angle*pi/180)*distance)]
      donutPolygon <- SpatialPolygons(list(Polygons(list(Polygon(donutPolygon[,.(x_move, y_move)])),
                                                    ID = 0)))
      intersectpolygon <- raster::intersect(donutPolygon, targetPolygon)
      if(gArea(intersectpolygon) == gArea(targetPolygon)){
        needmore <- FALSE
      }
      i <- i+1
    }
    rm(i)
    if(randomRotate){
      newcenters[, ':='(k = 1,
                        hexagonID = 1:(length(x_move)),
                        rotateAngle = sample(0:5, size = length(x_move),
                                             replace = TRUE))]
      newcenters[, rotateAngle := rotateAngle*60]
    } else {
      newcenters[, ':='(k = 1,
                        hexagonID = 1:(length(x_move)),
                        rotateAngle = 0)]
    }
    newstemmap <- merge(initialStemMapping, newcenters,
                        by = "k", allow.cartesian = TRUE)
    newstemmap[, bearing := bearing - rotateAngle]
    newstemmap[,':='(x = sin(bearing*pi/180)*distance,
                     y = cos(bearing*pi/180)*distance)]
    newstemmap[,':='(x = x + x_move,
                     y = y + y_move,
                     distance = NULL,
                     bearing = NULL,
                     rotateAngle = NULL)]
    allstemmap <- rbind(initialStemMapping[,.(hexagonID = 0, objectID, x, y)],
                        newstemmap[,.(hexagonID, objectID, x, y)])
    allstemmap <- SpatialPointsDataFrame(allstemmap[,.(x,  y)],
                                         data = allstemmap)
    allstemmap <- raster::intersect(allstemmap, targetPolygon)
    allstemmap_a <- raster::intersect(allstemmap, targetPolygon)
    allstemmap <- allstemmap@data %>% data.table
    # initial_hexapoints[, k := 1]
    # allhexagons <- merge(initial_hexapoints, newcenters,
    #                      by = "k", allow.cartesian = TRUE)
    # allhexagons <- allhexagons[, .(hexagonID, angle,
    #                                x = x + x_move, y = y + y_move)]
    # all_hexagon_list <- list()
    # all_hexagon_list[[1]] <- Polygons(list(Polygon(data.frame(initial_hexapoints[,.(x, y)]))), ID = 1)
    #
    # for(i in 2:max(allhexagons$hexagonID)){
    #   indihexagon <- allhexagons[hexagonID == i,]
    #   indihexagon <- indihexagon[order(angle),]
    #   indi_hexagon_polygons <- Polygons(list(Polygon(data.frame(indihexagon[,.(x, y)]))),
    #                                     ID = unique(indihexagon$hexagonID))
    #   all_hexagon_list[[unique(indihexagon$hexagonID)]] <- indi_hexagon_polygons
    # }
    # all_hexagons <- SpatialPolygons(all_hexagon_list)
    return(allstemmap)
  }
}

