#' This function is to derive a correction index to account for edge effect when account for
#' competition effect
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
#' @return A ratio of overlapped area to full circular area.
#'
#' @examples
#' \dontrun{
#' # given a tree is located with bearing of 150 degree and distance of 13 m
#' # and in a plot of 16.9 m radius circle, in which all the trees are measured.
#' # assume all the trees within 10m radius have competitive effect on this tree
#' # hence the trees that are in the plot and within 10m radius from focal tree
#' # should be corrected based on proportion
#' # to calculate the proportion
#' proportion <- areaProportion(bearing = 150,
#'                              distance = 13,
#'                              radius = 10,
#'                              baseShape = "circle",
#'                              baseRadius = 16.9)
#'
#'
#'
#' }
#'
#' @importFrom sf st_as_sf st_cast st_area st_intersection
#' @importFrom dplyr summarise
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
  focalCircle <- data.frame(x = focalX + radius * sin(circlepoints),
                            y = focalY + radius * cos(circlepoints)) %>%
    st_as_sf(coords = c("x", "y")) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  if(baseShape == "circle"){
    baseArea <- data.frame(x = baseRadius * sin(circlepoints),
                             y = baseRadius * cos(circlepoints)) %>%
      st_as_sf(coords = c("x", "y")) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

  } else if (baseShape == "rectangle"){
    for(i in 1:4){
      if(i == 1){
        rectangleTable <- baseCorners[[i]]
      } else {
        rectangleTable <- rbind(rectangleTable, baseCorners[[i]])
      }
    }
    rectangleTable <- data.frame(rectangleTable)
    names(rectangleTable) <- c("x", "y")
    baseArea <- rectangleTable %>%
      st_as_sf(coords = c("x", "y")) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

  } else {
    stop("baseShape must be defined as either circle or rectangle.")
  }
  overlapArea <- sf::st_intersection(focalCircle, baseArea)

  if(is.null(overlapArea)){
    return(as.numeric(NA))
  } else {
    return(sf::st_area(overlapArea)/sf::st_area(focalCircle))
  }
}
