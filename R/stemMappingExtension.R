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
#' @param randomSeed numeric, Defines random seed for the random number generator.
#'                            This arguement is called when \code{randomRotate} is TRUE.
#'                            If missing, NA is used, suggesting no random seed is
#'                            set.
#'
#' @return  A table contains the x and y for all the objects in the extended area.
#'
#' @examples
#' \dontrun{
#' ## randomly generate some trees
#' library(data.table)
#' smallplottrees <- data.table(expand.grid(angle = seq(0, 360, 1),
#'                               distance = seq(0.1, 5.6, 0.1)))
#' smallplottrees[, tree_id := 1:nrow(smallplottrees)]
#' ## extend it to 1 ha
#' treelist_smallplot <- stemMappingExtension(objectID = smallplottrees$tree_id,
#'                                            bearing = smallplottrees$angle,
#'                                            distance = smallplottrees$distance,
#'                                            plotRadius = 5.64,
#'                                            randomRotate = TRUE)
#' smallplottrees[tree_id %in% treelist_smallplot$objectID,
#'               inHexigon := "Yes"]
#' smallplottrees[is.na(inHexigon),
#'               inHexigon := "No"]
#' smallplottrees[,':='(x = sin(angle*pi/180)*distance,
#'                      y = cos(angle*pi/180)*distance)]
#' library(ggplot2)
#' trees_inplot <- ggplot(data = smallplottrees, aes(x = x, y = y))+
#'  geom_point(aes(col = inHexigon))
#' trees_all <- ggplot(data = treelist_smallplot, aes(x = x, y = y))+
#'  geom_point(aes(col = hexagonID))
#'
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
#'  geom_point(aes(col = factor(source)))+
#'  geom_point(data = alltreelist[hexagonID == 0,],
#'  aes(x, y), col = "red")
#'
#' }
#'
#' @importFrom sf st_as_sf st_area st_cast st_intersection
#' @importFrom data.table data.table shift
#' @importFrom dplyr summarise
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
                                 randomRotate = FALSE,
                                 randomSeed = as.numeric(NA)){
  if(tolower(targetShape) == "circle"){
    targetPolygon <- data.table::data.table(angle = seq(0, 359.9, 0.5),
                                            distance = sqrt(targetArea*10000/pi))
    targetPolygon[, ':='(x = sin(angle*pi/180)*distance,
                         y = cos(angle*pi/180)*distance)]
    # targetPolygon <- SpatialPolygons(list(Polygons(list(Polygon(targetPolygon[,.(x, y)])),
    #                                                ID = 0)))
    targetPolygon <- data.frame(targetPolygon) %>%
      st_as_sf(coords = c("x", "y")) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
  } else if(tolower(targetShape) == "square"){
    halfLength <- sqrt(targetArea*10000)/2
    targetPolygon <- data.table(matrix(data = c(halfLength, halfLength,
                                                halfLength, -halfLength,
                                                -halfLength, -halfLength,
                                                -halfLength, halfLength),
                                       byrow = TRUE, nrow = 4))
    names(targetPolygon) <- c("x", "y")
    # targetPolygon <- SpatialPolygons(list(Polygons(list(Polygon(targetPolygon[,.(x, y)])),
    #                                                ID = 0)))
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
  initialStemMapping <- rbind(initialStemMapping[, NOTE := NA],
                              data.table(objectID = 0,
                                         distance = 0,
                                         bearing = 0,
                                         x = c(0, plotRadius-1),
                                         y = 0,
                                         NOTE = "DUMMY"))
  initialStemMapping[,':='(x_temp = x,
                           y_temp = y)]

  initialStemMapping <- data.frame(initialStemMapping) %>%
    st_as_sf(coords = c("x", "y"))
  central_hexagon <- data.table(angle = seq(from = 30, to = 360, by = 60),
                                distance = plotRadius)
  central_hexagon[, ':='(x = sin(angle*pi/180)*distance,
                         y = cos(angle*pi/180)*distance)]
  central_hexagon <- data.frame(central_hexagon) %>%
    st_as_sf(coords = c("x", "y")) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  initialStemMapping <- suppressWarnings(sf::st_intersection(initialStemMapping, central_hexagon))

  initialStemMapping <- initialStemMapping %>% data.table
  initialStemMapping <- initialStemMapping[is.na(NOTE),
                                           .(objectID, distance, bearing,
                                             x = x_temp,
                                             y = y_temp)]
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
      donutPolygon <- data.frame(donutPolygon) %>%
        st_as_sf(coords = c("x_move", "y_move")) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      intersectpolygon <- sf::st_intersection(donutPolygon, targetPolygon)

      if(st_area(intersectpolygon) == st_area(targetPolygon)){
        needmore <- FALSE
      }
      i <- i+1
    }
    rm(i)
    if(randomRotate){
      if(!is.na(randomSeed)){
        set.seed(randomSeed)
      }
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
    allstemmap[,':='(x_temp = x,
                     y_temp = y)]

    allstemmap <- data.frame(allstemmap) %>%
      st_as_sf(coords = c("x", "y"))
    allstemmap <- suppressWarnings(sf::st_intersection(allstemmap, targetPolygon))

    allstemmap <- allstemmap %>% data.table
    allstemmap <- allstemmap[,.(hexagonID, objectID, x = x_temp, y = y_temp)]
    return(allstemmap)
  }
}

