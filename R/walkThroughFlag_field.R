#' To determine a tree-level flag in a walk-through sample in the field
#'
#' @description This function determines the walk-through area and assign walk-through flag for a tree
#'
#' @param bearingBoundary numeric, The bearing (degree) for a walk-through edge.
#' @param distanceBoundary numeric, The distance (m) between IPC to the walk-through edge.
#' @param radiusYSM numeric, The radius (m) of a fixed area plot, default is 11.28, which is BC's YSM plot radius.
#' @param treeList list, Defines a tree number, bearing and distance of a tree from IPC.
#' @export
#' @docType methods
#' @note This method is adapted from Ducey 2004 paper. The population boundary is a
#'       simple straight line, which is defined by the boundary bearing (\code{bearingBoundary})
#'       and boundary distance (\code{distanceBoundary}).
#'       Purple line (from P1 to P4) is the boundary. P1 to P4 are the four pins. The dark grey area
#'       is the walkthrough area, in which all trees should be marked as \code{W}.
#'       The white area in the circular plot is out of the plot, in which trees should be marked as \code{O}.
#'       No mark should be made for trees in rest of the plot (light grey area).
#'       pin1 (P1 in green) is always the right direction (90 degree on your right) from the distance point (D in red)
#'       with your back to the IPC.
#'       The bearing of boundary is clock-wise angle from the North to the D-P1. The distance
#'       of boundary is between IPC and D.
#' @returns The function returns three objects: 1. a figure shows the plot and boundary and trees with wkthrough codes
#'                                              2. a lookup table in which a field crew can identify a tree's wkthrough codes
#'                                                 based on a tree's bearing.
#'                                              3. assigned wkthrough codes for the trees provided.
#' @examples
#' \dontrun{
#' # for bearing of 45 with distance of 4.5
#' plot_exam_fld <- walkThroughFlag_field(bearingBoundary = 45,
#'                               distanceBoundary = 11.30,
#'                               treeList = list(c(1, 30, 2),
#'                                               c(5, 120, 6),
#'                                               c(6, 160, 10),
#'                                               c(8, 250, 7),
#'                                               c(9, 110, 9),
#'                                               c(10, 340, 9),
#'                                               c(12, 340, 11),
#'                                               c(13, 320, 13)))
#'}
#' @importFrom ggplot2 ggplot geom_segment geom_sf geom_point geom_text theme aes guides guide_legend
#' @importFrom sf st_polygon st_sfc
#' @rdname walkThroughFlag_field
#' @author Yong Luo
walkThroughFlag_field <- function(bearingBoundary,
                                  distanceBoundary,
                                  radiusYSM = 11.28,
                                  treeList){
  pins <- walkThroughPins(bearingBoundary,
                          distanceBoundary,
                          radiusYSM)
  angleBearingRight <- pins$angleBearingRight
  pin1 <- pins$pin1
  pin2 <- pins$pin2
  pin3 <- pins$pin3
  pin4 <- pins$pin4
  ysmplotbearing <- c(seq(0, 359.9, by = 0.1), 0)
  ysmplot <- st_polygon(
    list(
      cbind(sin(ysmplotbearing*pi/180)*radiusYSM,
            cos(ysmplotbearing*pi/180)*radiusYSM)
    )
  )
  ysmplot <- st_sfc(ysmplot)
  distancePoint <- data.frame(x = sin(angleBearingRight*pi/180)*distanceBoundary,
                              y = cos(angleBearingRight*pi/180)*distanceBoundary)
  fourPinPoints <- data.frame(pin = c("P1", "P2", "P3", "P4"),
                              x = sin(c(pin1, pin2, pin3, pin4)*pi/180)*radiusYSM,
                              y = cos(c(pin1, pin2, pin3, pin4)*pi/180)*radiusYSM) %>%
    data.table
  if(distanceBoundary >= radiusYSM){
    fourPinPoints[pin == "P1",
                  ':='(x = sin(bearingBoundary*pi/180)*radiusYSM + distancePoint$x,
                       y = cos(bearingBoundary*pi/180)*radiusYSM + distancePoint$y)]
    fourPinPoints[pin == "P4",
                  ':='(x = sin((bearingBoundary + 180)*pi/180)*radiusYSM + distancePoint$x,
                       y = cos((bearingBoundary + 180)*pi/180)*radiusYSM + distancePoint$y)]
  }


  if(distanceBoundary < radiusYSM){
    ## from P1 to P2
    pin2_new <- if(pin2 < pin1){
      pin2 + 360
    } else {
      pin2
    }
    pin4_new <- if(pin4 < pin3){
      pin4 + 360
    } else {
      pin4
    }
    walkThroughPoly <- data.frame(x = sin(c(seq(pin1, pin2_new, length.out = 100),
                                            seq(pin3, pin4_new, length.out = 100),
                                            pin1)*pi/180)*radiusYSM,
                                  y = cos(c(seq(pin1, pin2_new, length.out = 100),
                                            seq(pin3, pin4_new, length.out = 100),
                                            pin1)*pi/180)*radiusYSM)
    walkThroughPoly <- st_polygon(
      list(
        cbind(walkThroughPoly$x,
              walkThroughPoly$y)
      )
    )
    walkThroughPoly <- st_sfc(walkThroughPoly)
  } else{
    if(pin2 < pin3){
      pin2_new <- pin2 + 360
    } else {
      pin2_new <- pin2
    }
    walkThroughPoly <- data.frame(x = sin(c(seq(pin3, pin2_new, length.out = 200),
                                            pin3)*pi/180)*radiusYSM,
                                  y = cos(c(seq(pin3, pin2_new, length.out = 200),
                                            pin3)*pi/180)*radiusYSM)
    walkThroughPoly <- st_polygon(
      list(
        cbind(walkThroughPoly$x,
              walkThroughPoly$y)
      )
    )
    walkThroughPoly <- st_sfc(walkThroughPoly)
    rm(pin2_new)
  }

  ## prepare the points in the area of fan between P3 and P2
  pin2_new <- if(pin2 < pin3){
    ceiling(pin2+360)
  } else {
    ceiling(pin2)
  }
  pin3_new <- as.integer(pin3)

  bd_tree <- data.frame(expand.grid(bearing = seq(pin3_new, pin2_new, by = 1),
                                    distance = seq(0, radiusYSM, by = 0.05))) %>%
    data.table
  bd_tree$uid <- 1:nrow(bd_tree)
  bd_tree[,':='(x = sin(bearing*pi/180)*distance,
                y = cos(bearing*pi/180)*distance)]

  tree_stem <- st_as_sf(bd_tree, coords = c("x", "y"))

  trees_in_wk <- suppressWarnings(st_intersection(tree_stem, walkThroughPoly)) %>% data.table

  bd_tree <- bd_tree[uid %in% trees_in_wk$uid,]

  bd_tree_smry <- bd_tree[,.(distance_min = min(distance),
                             distance_max = max(distance)),
                          by = "bearing"]
  bd_tree_smry <- bd_tree_smry[order(bearing),]
  bd_tree_smry[bearing >= 360, bearing := bearing - 360]

  treeLocation <- data.table(treeNo = unlist(lapply(treeList, function(x){x[1]})),
                             bearing = unlist(lapply(treeList, function(x){x[2]})),
                             distance = unlist(lapply(treeList, function(x){x[3]})))
  treeLocation[, treedup := length(bearing), by = "treeNo"]
  treeDup <- treeLocation[treedup > 1]
  if(nrow(treeDup) > 0){
    stop(paste0("Tree number is duplicated: ", unique(treeDup$treeNo)))
  }
  rm(treeDup)
  treeLocation[, treedup := NULL]
  treeLocation[,':='(x = sin(bearing*pi/180)*distance,
                     y = cos(bearing*pi/180)*distance)]

  treeLocation <- merge(treeLocation,
                        bd_tree_smry,
                        by = "bearing",
                        all.x = TRUE)
  treeLocation[, wkcodes := "NA"]
  treeLocation[distance >= distance_min & distance <= distance_max,
               wkcodes := "W"]
  treeLocation[distance > distance_max,
               wkcodes := "O"]
  treeLocation[distance > radiusYSM,
               wkcodes := "Error"]

  if(distanceBoundary < radiusYSM){
    pin1_new <- if(pin1 < pin4){
      pin1 + 360
    } else {
      pin1
    }
    outPoly <- data.frame(x = sin(c(seq(pin4, pin1_new, by = 1),
                                    pin4)*pi/180)*radiusYSM,
                          y = cos(c(seq(pin4, pin1_new, by = 1),
                                    pin4)*pi/180)*radiusYSM)

    outPoly <- st_polygon(
      list(
        cbind(outPoly$x,
              outPoly$y)
      )
    )
    outPoly <- st_sfc(outPoly)
    out_size <- round(st_area(outPoly))
  }
  plot_size <- round(st_area(ysmplot))
  wk_size <- round(st_area(walkThroughPoly))
  if(distanceBoundary < radiusYSM){
    plotfigure <- ggplot()+
      geom_sf(data = ysmplot, col = "grey", fill = "grey", alpha = 0.5)+
      geom_sf(data = walkThroughPoly, fill = "grey", col = "grey")+
      geom_sf(data = outPoly, fill = "white", col = "grey")+
      geom_segment(aes(x = 0, xend = distancePoint$x, y = 0, yend = distancePoint$y))+
      geom_segment(aes(x = fourPinPoints$x[1], xend = fourPinPoints$x[4],
                       y = fourPinPoints$y[1], yend = fourPinPoints$y[4]),
                   col = "purple", size = 2)+
      geom_point(data = distancePoint, aes(x = x, y = y), col = "red")+
      geom_text(aes(x = distancePoint$x, y = distancePoint$y, label = "D"),
                col = "red", hjust = -0.5)+
      geom_point(data = fourPinPoints, aes(x = x, y = y), col = "red")+
      geom_text(aes(x = fourPinPoints$x, y = fourPinPoints$y, label = fourPinPoints$pin),
                hjust = -0.5,
                col = "red")+
      geom_point(aes(x = 0, y = 0), col = "red")+
      geom_text(aes(x = 0, y = 0, label = "IPC"), col = "red", hjust = -0.5)+
      geom_point(data = treeLocation, aes(x = x, y = y, colour = as.factor(wkcodes)),
                 shape = 10)+
      geom_text(aes(x = treeLocation$x, y = treeLocation$y,
                    label = treeLocation$treeNo),
                hjust = 0)+
      guides(colour = guide_legend(title = "WK codes"))+
      geom_segment(aes(x = distancePoint$x, xend = distancePoint$x,
                       y = distancePoint$y, yend = distancePoint$y + 5),
                   arrow = arrow(),
                   col = "black",
                   alpha = 0.3)+
      geom_text(aes(x = distancePoint$x,
                    y = distancePoint$y + 5,
                    label = "N"), col = "black", vjust = -0.5)+
      geom_text(aes(x = c(-12, -12, -12),
                    y = c(-9, -10, -11),
                    label = c(paste0("Plot size: ", plot_size),
                              paste0("Out size:  ", out_size),
                              paste0("WK size:  ", wk_size))),
                hjust = 0)+
      geom_text(aes(x = c(-12, -12),
                    y = c(11, 12),
                    label = c(paste0("B_bearing:  ", bearingBoundary),
                              paste0("B_distance:", distanceBoundary))),
                hjust = 0)+
      geom_text(aes(x = c(7, 7, 7, 7),
                    y = seq(12, 9.5, length.out = 4),
                    label = c(paste0("Pin1: ", round(pin1, 1)),
                              paste0("Pin2: ", round(pin2, 1)),
                              paste0("Pin3: ", round(pin3, 1)),
                              paste0("Pin4: ", round(pin4, 1)))),
                hjust = 0)+
      theme_bw()+
      theme(axis.title = element_blank(),
            axis.text = element_text(size = 10),
            panel.grid = element_blank(),
            legend.background = element_rect(colour = "black"),
            legend.direction = "vertical",
            legend.position = c(0.92, 0.12),
            legend.title = element_blank())

  } else {
    plotfigure <- ggplot()+
      geom_sf(data = ysmplot, col = "grey", fill = "grey", alpha = 0.5)+
      geom_sf(data = walkThroughPoly, fill = "grey", col = "grey")+
      geom_segment(aes(x = 0, xend = distancePoint$x, y = 0, yend = distancePoint$y))+
      geom_segment(aes(x = fourPinPoints$x[1], xend = fourPinPoints$x[4],
                       y = fourPinPoints$y[1], yend = fourPinPoints$y[4]),
                   col = "purple", size = 2)+
      geom_point(data = distancePoint, aes(x = x, y = y), col = "red")+
      geom_text(aes(x = distancePoint$x, y = distancePoint$y, label = "D"),
                col = "red", hjust = -0.5)+
      geom_point(data = fourPinPoints, aes(x = x, y = y), col = "red")+
      geom_text(aes(x = fourPinPoints$x, y = fourPinPoints$y, label = fourPinPoints$pin),
                hjust = -0.5,
                col = "red")+
      geom_point(aes(x = 0, y = 0), col = "red")+
      geom_text(aes(x = 0, y = 0, label = "IPC"), col = "red", hjust = -0.5)+
      geom_point(data = treeLocation, aes(x = x, y = y, colour = as.factor(wkcodes)),
                 shape = 10)+
      geom_text(aes(x = treeLocation$x, y = treeLocation$y,
                    label = treeLocation$treeNo),
                hjust = 0)+
      guides(colour = guide_legend(title = "WK codes"))+
      geom_segment(aes(x = distancePoint$x, xend = distancePoint$x,
                       y = distancePoint$y, yend = distancePoint$y + 5),
                   arrow = arrow(),
                   col = "black",
                   alpha = 0.3)+
      geom_text(aes(x = distancePoint$x,
                    y = distancePoint$y + 5,
                    label = "N"), col = "black", vjust = -0.5)+
      geom_text(aes(x = c(-12, -12, -12),
                    y = c(-9, -10, -11),
                    label = c(paste0("Plot size: ", plot_size),
                              paste0("Out size:  NA"),
                              paste0("WK size:  ", wk_size))),
                hjust = 0)+
      geom_text(aes(x = c(-12, -12),
                    y = c(11, 12),
                    label = c(paste0("B_bearing:  ", bearingBoundary),
                              paste0("B_distance:", distanceBoundary))),
                hjust = 0)+
      geom_text(aes(x = c(7, 7),
                    y = c(12, 10),
                    label = c(paste0("Pin2: ", round(pin2, 1)),
                              paste0("Pin3: ", round(pin3, 1)))),
                hjust = 0)+
      theme_bw()+
      theme(axis.title = element_blank(),
            axis.text = element_text(size = 10),
            panel.grid = element_blank(),
            legend.background = element_rect(colour = "black"),
            legend.direction = "vertical",
            legend.position = c(0.92, 0.12),
            legend.title = element_blank())
  }

  return(list(figure = plotfigure,
              wkRange = bd_tree_smry,
              wkCode = treeLocation[,.(treeNo, bearing, distance, wkcodes)]))
}
