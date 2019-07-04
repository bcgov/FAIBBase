#' This function is to derive a correction index to account for edge effect
#' @description The correction index is calculated using proportion of overlapped area
#'              to full circular area.
#'
#'
#' @param bearing numeric, The bearing of a tree from a given point of plot (centre or corner).
#' @param distance numeric, The distance of a tree from a given point of plot (centre or corner).
#' @param radius numeric, The radius for a focal subject, which define the circular area around
#'                        a subject tree.
#' @param baseShape character, The shape of the area to be overlapped. Must be either
#'                         \code{circle} or \code{rectangle}. Default is \code{circle}.
#' @param baseRadius numeric, The radius for the base area, if the shape is defined as \code{circle}.
#'                            Default is 10.
#' @param baseCorners list, If the shape is defined as \code{rectangle}, this argument specifies
#'                             upper left, upper right, lower right and lower left corners.
#'                             Default is \code{list(c(-50, 50), c(50, 50), c(50, -50), c(-50, -50))},
#'                             which represent a 10000 m2 base area.
#'
#' @return a data table that has five columns, plotNumber, treeNumber, Year, IntraH and InterH
#'
#' @importFrom sp Polygons SpatialPolygons
#' @importFrom raster intersect
#' @importFrom rgeos gArea
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname areaProportion
#'
#' @author Yong Luo
areaProportion <- function(bearing,
                           distance,
                           radius,
                           baseShape = "circle",
                           baseRadius = 10,
                           baseCorners = list(c(-50, 50), c(50, 50), c(50, -50), c(-50, -50))){
  alldata <- list()
  for(k in 1:length(bearing)){
    alldata[[k]] <- c(bearing[k], distance[k])
  }
  alloutputs <- lapply(alldata, function(i) areaProportion_indi(bearing = i[1],
                                                                distance = i[2],
                                                                radius,
                                                                baseShape,
                                                                baseRadius,
                                                                baseCorners))
  return(unlist(alloutputs))
}

areaProportion_indi <- function(bearing,
                                distance,
                                radius,
                                baseShape,
                                baseRadius,
                                baseCorners){
  focalX <- distance * sin(bearing * pi/180)
  focalY <- distance * cos(bearing * pi/180)
  circlepoints <- seq(0, 2*pi, length.out = 360)
  focalCircle <- Polygons(list(Polygon(cbind(focalX + radius * sin(circlepoints),
                                             focalY + radius * cos(circlepoints)))),
                          ID = 1)
  focalCircle <- SpatialPolygons(list(focalCircle))
  if(baseShape == "circle"){
    baseArea <- Polygons(list(Polygon(cbind(baseRadius * sin(circlepoints),
                                            baseRadius * cos(circlepoints)))),
                         ID = 1)
    baseArea <- SpatialPolygons(list(baseArea))
  } else if (baseShape == "rectangle"){
    for(i in 1:4){
      if(i == 1){
        rectangleTable <- baseCorners[[i]]
      } else {
        rectangleTable <- rbind(rectangleTable, baseCorners[[i]])
      }
    }
    baseArea <- Polygons(list(Polygon(rectangleTable)),
                         ID = 1)
    baseArea <- SpatialPolygons(list(baseArea))
  } else {
    stop("baseShape must be defined as either circle or rectangle.")
  }
  overlapArea <- raster::intersect(focalCircle, baseArea)
  if(is.null(overlapArea)){
    return(as.numeric(NA))
  } else {
    return(rgeos::gArea(overlapArea)/rgeos::gArea(focalCircle))
  }
}
