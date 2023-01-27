#' Map stems in a plot.
#'
#' @description The function is to map all the stems in a plot based on bearing and distance.
#'
#' @param objectID character, A object's ID, e.g., a tree's ID.
#' @param bearing  numeric, Azimuth of a object from the north. It should be between 0 to 360.
#' @param distance numeric, Distance between a object and the centre of the circle.
#' @param plotShape character, circle, Rectangle or NA. NA means no plot shape information is not available and
#'                             and no plot geometry will be produced.
#' @param plotSize  numeric, Defines the plot geoimetry. If plotShape is circular, a number must be provided as plot radius.
#'                           If plotShape is rectangle, two numbers must be provided to determine length and width of a
#'                           plot.
#' @param distanceUnit character, Defines the unit in the stem mapping. Default is \code{m}.
#' @param outputFormat character, Defines whether a \code{table} or a \code{figure} will be returned
#'                                from this function.
#' @param showID logical, Specifies whether the objectID will be showed in outputed figure. Default is \code{TRUE}.
#'
#' @return  1) A table contains the x and y for all the objects and the plot boundary.
#'          2) a ggplot object will be returned for visualization.
#'
#' @examples
#' \dontrun{
#' ## randomly generate some trees
#' bigplottrees <- data.table(tree_id = 1:20,
#'                            angle = runif(20, min = 0, max = 360),
#'                            distance = runif(20, min = 0, max = 11.28))
#' ## output a table
#' stemmapped_table <- stemMapping(objectID = bigplottrees$tree_id,
#'                                          bearing = bigplottrees$angle,
#'                                          distance = bigplottrees$distance,
#'                                          plotShape = "circle",
#'                                          plotSize = 11.28,
#'                                          distanceUnit = "m",
#'                                          outputFormat = "table")
#'
#' ## output a figure
#' stemmapped_figure <- stemMapping(objectID = bigplottrees$tree_id,
#'                                          bearing = bigplottrees$angle,
#'                                          distance = bigplottrees$distance,
#'                                          plotShape = "circle",
#'                                          plotSize = 11.28,
#'                                          distanceUnit = "m",
#'                                          outputFormat = "figure",
#'                                          showID = TRUE)
#'
#' ## output a figure without a plot
#' stemmapped_figure <- stemMapping(objectID = bigplottrees$tree_id,
#'                                          bearing = bigplottrees$angle,
#'                                          distance = bigplottrees$distance,
#'                                          outputFormat = "figure",
#'                                          showID = TRUE)
#'
#' }
#'
#' @note In the figure, IPC is the integrated plot center with x = 0 and y = 0.
#' @importFrom sp SpatialPoints SpatialPointsDataFrame SpatialPolygons Polygons Polygon
#' @importFrom data.table data.table shift
#' @importFrom rgeos gArea
#' @importFrom raster intersect
#'
#' @export
#' @docType methods
#' @rdname stemMapping
#'
#' @author Yong Luo
stemMapping <- function(objectID,
                        bearing,
                        distance,
                        plotShape = as.character(NA),
                        plotSize,
                        distanceUnit = "m",
                        outputFormat,
                        showID = TRUE){
  if(!is.na(plotShape)){
    if(plotShape == "circle"){
      if(length(plotSize) > 1){stop("plotShape is circle and plotSize must contain one number.")}
    } else if(plotShape == "rectangle") {
      if(length(plotSize) != 2){stop("plotShape is rectangle and plotSize must contain two numbers.")}
    } else {
      stop("plotShape is not correctly specified.")
    }
    if(tolower(plotShape) == "circle"){
      plot_table <- data.table::data.table(angle = seq(0, 359.9, 0.5),
                                           distance = plotSize)
      plot_table[, ':='(x = sin(angle*pi/180)*distance,
                        y = cos(angle*pi/180)*distance)]
    } else {
      plot_table <- data.table(matrix(data = c(plotSize[1]/2, plotSize[2]/2,
                                               plotSize[1]/2, -plotSize[2]/2,
                                               -plotSize[1]/2, -plotSize[2]/2,
                                               -plotSize[1]/2, plotSize[2]/2),
                                      byrow = TRUE, nrow = 4))
      names(plot_table) <- c("x", "y")
    }
    plotboundary <- plot_table[,.(objectID = "plotboundary",
                                  distance = NA,
                                  bearing = NA,
                                  x, y)]

  } else {
    warning("plotShape is not provided and no plot information will be produced.")
  }

  initialStemMapping <- data.table::data.table(objectID,
                                               distance,
                                               bearing)
  initialStemMapping[,':='(x = sin(bearing*pi/180)*distance,
                           y = cos(bearing*pi/180)*distance)]

  if(outputFormat == "table"){
    if(!is.na(plotShape)){
      output <- rbind(initialStemMapping, plotboundary)
    } else {
      output <- initialStemMapping
    }
  } else if(outputFormat == "figure"){
    output <- ggplot2::ggplot()+
      geom_point(data = initialStemMapping, aes(x = x, y = y))+
      geom_point(aes(x = 0, y = 0), col = "red")+
      geom_text(aes(x = 0, y = 0, label = "IPC"),
                col = "red", vjust = 1.5)+
      labs(x = paste0("x (", distanceUnit, ")"), y = paste0("y (", distanceUnit, ")"))+
      theme_bw()
    if(showID){
      output <- output+
        geom_text(data = initialStemMapping,
                  aes(x = x, y = y, label = objectID),
                  vjust = 1.5)
    }
    if(!is.na(plotShape)){
      output <- output+
        geom_polygon(data = plotboundary,
                     aes(x = x, y = y), col = "black", fill = NA)
    }
  } else {
    stop("outputFormat is not correctly specified.")
  }
  return(output)
}

