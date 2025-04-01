#' To determine a tree-level flag in a walk-through sample in office
#'
#' @description This function determines the walk-through area and assign walk-through flag for a tree
#'
#' @param boundaryList list, A list of boundaries,
#'                           The first element is bearing (degree) for a walk-through edge.
#'                           The second element is distance (m) between IPC to the walk-through edge.
#' @param radiusYSM numeric, The radius (m) of a YSM plot, default is 11.28.
#' @param treeNo numeric, Defines a tree number.
#' @param bearingTree numeric, Defines bearing of a tree number from IPC.
#' @param distanceTree numeric, Defines distance of a tree number from IPC.
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
#' treelist <- data.frame(expand.grid(bearing_tree = seq(0, 360, by = 20),
#'                                    distance_tree = seq(3, 11, by = 2)))
#' treelist <- rbind(treelist, data.frame(bearing_tree = 50,
#'                                        distance_tree = 13)) # this tree is beyound 11.28 m
#' treelist$tree_no <- 1:nrow(treelist)
#' # example 1, one boundary in plot
#' plot_exam1 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 6)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 2, one boundary out of plot
#' plot_exam2 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 13)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 3, two boundaries and out-of-plot zones are overlapped
#' plot_exam3 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 6),
#'                                                           c(150, 7)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 4, two boundaries and out-of-plot zones are not overlapped
#' plot_exam4 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 6),
#'                                                           c(250, 7)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 5, two boundaries with one of them has out-of-plot zone and the other one is out of plot
#' plot_exam5 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 6),
#'                                                           c(150, 13)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 6, two boundaries are out of plot
#' plot_exam6 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 15),
#'                                                           c(150, 13)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 7, three boundaries
#' plot_exam7 <- boundaryTreeFlag_walkthrough(boundaryList <- list(c(100, 7),
#'                                                           c(150, 3),
#'                                                           c(300, 15)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#'}
#'
#' @importFrom ggplot2 ggplot geom_segment geom_sf geom_point geom_text theme aes guides guide_legend
#' @importFrom sf st_polygon st_sfc st_area st_combine st_sf st_cast st_union st_difference st_intersection
#' @rdname boundaryTreeFlag_walkthrough
#' @author Yong Luo
boundaryTreeFlag_walkthrough <- function(boundaryList,
                                         radiusYSM = 11.28,
                                         treeNo,
                                         bearingTree,
                                         distanceTree){
  ysmplotbearing <- c(seq(0, 359.9, by = 0.1), 0)
  ysmplot <- st_polygon(
    list(
      cbind(sin(ysmplotbearing*pi/180)*radiusYSM,
            cos(ysmplotbearing*pi/180)*radiusYSM)
    )
  )
  ysmplot <- st_sfc(ysmplot)
  plot_size <- round(st_area(ysmplot))
  allpins <- lapply(boundaryList,
                    function(x){walkThroughPins(bearingBoundary = x[1],
                                                distanceBoundary = x[2],
                                                radiusYSM)})
  allpins_tb <- NULL
  for (i in 1:length(allpins)) {
    indibounary <- allpins[[i]]
    indipin_tb <- data.frame(boundary = i,
                             pin = names(unlist(indibounary)),
                             bearing = round(unlist(indibounary), 1),
                             bearingBoundary = boundaryList[[i]][1],
                             distanceBoundary = boundaryList[[i]][2],
                             radiusYSM = radiusYSM) %>%
      data.table
    allpins_tb <- rbind(allpins_tb, indipin_tb)
    rm(indipin_tb, indibounary)
  }
  rm(i, allpins)

  allpins_tb[pin == "angleBearingRight",
             ':='(x = sin(bearing*pi/180)*distanceBoundary,
                  y = cos(bearing*pi/180)*distanceBoundary)]

  allpins_tb[pin %in% c(paste0("pin", 1:4)),
             ':='(x = sin(bearing*pi/180)*radiusYSM,
                  y = cos(bearing*pi/180)*radiusYSM)]
  allpins_tb <- merge(allpins_tb,
                      allpins_tb[pin == "angleBearingRight",
                                 .(boundary, distance_x = x, distance_y = y)],
                      by = "boundary",
                      all.x = TRUE)
  allpins_tb[distanceBoundary >= radiusYSM & pin == "pin1",
             ':='(x = sin(bearingBoundary*pi/180)*radiusYSM + distance_x,
                  y = cos(bearingBoundary*pi/180)*radiusYSM + distance_y)]
  allpins_tb[distanceBoundary >= radiusYSM & pin == "pin4",
             ':='(x = sin((bearingBoundary + 180)*pi/180)*radiusYSM + distance_x,
                  y = cos((bearingBoundary + 180)*pi/180)*radiusYSM + distance_y)]
  allpins_tb[,':='(distance_x = NULL,
                   distance_y = NULL)]

  allpins_tb_wide <- reshape(data = allpins_tb[pin %in% c("pin1", "pin2", "pin3", "pin4"),
                                               .(boundary, pin, x, y, bearing, distanceBoundary,
                                                 bearingBoundary,
                                                 radiusYSM)],
                             direction = "wide",
                             idvar = "boundary",
                             timevar = "pin",
                             v.names = c("bearing", "x", "y"),
                             sep = "_")

  allpins_tb_wide[, ':='(x_lab = -12,
                         y_lab = seq(13+nrow(allpins_tb_wide)*1.2, 13,
                                     length.out = nrow(allpins_tb_wide)),
                         B_lab = paste0("B", boundary,
                                        ": bearing=", bearingBoundary,
                                        "; distance=", distanceBoundary))]

  allpins_tb_wide[,':='(pin_y = seq(12+nrow(allpins_tb_wide)*1.2, 12,
                                    length.out = nrow(allpins_tb_wide)),
                        pin_lab = paste0("       pin1=", bearing_pin1,
                                         "; pin2=", bearing_pin2,
                                         "; pin3=", bearing_pin3,
                                         "; pin4=", bearing_pin4))]
  allpins_tb_wide[, boundaryName := paste0("B", boundary)]

  allpins_tb_wide[distanceBoundary < radiusYSM &
                    bearing_pin2 < bearing_pin1,
                  ':='(bearing_pin2 = bearing_pin2 + 360)]
  allpins_tb_wide[distanceBoundary < radiusYSM &
                    bearing_pin4 < bearing_pin3,
                  ':='(bearing_pin4 = bearing_pin4 + 360)]
  allpins_tb_wide[distanceBoundary >= radiusYSM &
                    bearing_pin2 < bearing_pin3,
                  bearing_pin2 := bearing_pin2 + 360]
  j <- 0

  for (i in 1:nrow(allpins_tb_wide)) {
    distanceBoundary <- allpins_tb_wide$distanceBoundary[i]
    pin1 <- allpins_tb_wide$bearing_pin1[i]
    pin2 <- allpins_tb_wide$bearing_pin2[i]
    pin3 <- allpins_tb_wide$bearing_pin3[i]
    pin4 <- allpins_tb_wide$bearing_pin4[i]
    if(distanceBoundary < radiusYSM){
      walkThroughPoly_i <- data.frame(x = sin(c(seq(pin1,
                                                    pin2, by = 0.1),
                                                seq(pin3,
                                                    pin4, by = 0.1),
                                                pin1)*pi/180)*radiusYSM,
                                      y = cos(c(seq(pin1,
                                                    pin2, by = 0.1),
                                                seq(pin3,
                                                    pin4, by = 0.1),
                                                pin1)*pi/180)*radiusYSM)
      pin1_new <- if(pin1 < pin4){
        pin1 + 360
      } else {
        pin1
      }
      outPoly_i <- data.frame(x = sin(c(seq(pin4, pin1_new, by = 0.1),
                                        pin4)*pi/180)*radiusYSM,
                              y = cos(c(seq(pin4, pin1_new, by = 0.1),
                                        pin4)*pi/180)*radiusYSM)

      outPoly_i <- st_polygon(
        list(
          cbind(outPoly_i$x,
                outPoly_i$y)
        )
      )
      outPoly_i <- st_sfc(outPoly_i)
      j <- j + 1
      if(j == 1){
        outPoly <- outPoly_i
      } else {
        outPoly <- st_combine(c(outPoly, outPoly_i))
      }

    } else{
      walkThroughPoly_i <- data.frame(x = sin(c(seq(pin3,
                                                    pin2, length.out = 200),
                                                pin3)*pi/180)*radiusYSM,
                                      y = cos(c(seq(pin3,
                                                    pin2, length.out = 200),
                                                pin3)*pi/180)*radiusYSM)
    }
    walkThroughPoly_i <- st_polygon(
      list(
        cbind(walkThroughPoly_i$x,
              walkThroughPoly_i$y)
      )
    )
    walkThroughPoly_i <- st_sfc(walkThroughPoly_i)

    if(i == 1){
      walkThroughPoly <- walkThroughPoly_i
    } else {
      walkThroughPoly <- st_combine(c(walkThroughPoly, walkThroughPoly_i))
    }
    rm(distanceBoundary, pin1, pin2, pin3, pin4, walkThroughPoly_i)
  }

  walkThroughPoly <- st_cast(st_sf(walkThroughPoly), "POLYGON")
  walkThroughPoly <- st_cast(st_union(walkThroughPoly), "POLYGON")

  if(j == 0){
    outPoly <- NULL
    out_size <- 0
  } else {
    outPoly <- st_cast(st_sf(outPoly), "POLYGON")
    outPoly <- st_cast(st_union(outPoly), "POLYGON")
    out_size <- round(sum(st_area(outPoly)))
    ## remove walkthrough zone in the out plot polygon
    walkThroughPoly <- st_cast(st_sf(st_difference(walkThroughPoly, outPoly)))
  }
  wk_size <- round(sum(st_area(walkThroughPoly)))
  rm(i, j)
  treeLocation <- data.table(treeNo = treeNo,
                             bearing = bearingTree,
                             distance = distanceTree)
  treeLocation[, treedup := length(bearing), by = "treeNo"]
  treeDup <- treeLocation[treedup > 1]
  if(nrow(treeDup) > 0){
    stop(paste0("Tree number is duplicated: ", unique(treeDup$treeNo)))
  }
  rm(treeDup)
  treeLocation[, treedup := NULL]
  treeLocation[,':='(x = sin(bearing*pi/180)*distance,
                     y = cos(bearing*pi/180)*distance)]
  tree_stem <- st_as_sf(treeLocation, coords = c("x", "y"))

  trees_in_wk <- suppressWarnings(st_intersection(tree_stem, walkThroughPoly)) %>% data.table
  treeLocation[, wkcodes := "NA"]
  treeLocation[treeNo %in% trees_in_wk$treeNo,
               wkcodes := "W"]
  treeLocation[distance > radiusYSM,
               wkcodes := "Error"]
  plotfigure <- ggplot()+
    geom_sf(data = ysmplot, col = "grey", fill = "grey", alpha = 0.5)+
    geom_sf(data = walkThroughPoly, fill = "grey", col = "grey")
  if(!is.null(outPoly)){
    trees_in_out <- suppressWarnings(st_intersection(tree_stem, outPoly)) %>% data.table
    treeLocation[treeNo %in% trees_in_out$treeNo,
                 wkcodes := "O"]
    plotfigure <- plotfigure+
      geom_sf(data = outPoly, fill = "white", col = "grey")
  }

  distancePoint <- allpins_tb[pin == "angleBearingRight",
                              .(x, y, B_lab = paste0("B", boundary))]
  plotfigure <- plotfigure +
    geom_segment(aes(x = 0, xend = distancePoint$x, y = 0, yend = distancePoint$y))+
    geom_segment(data = allpins_tb_wide,
                 aes(x = x_pin1, xend = x_pin4,
                     y = y_pin1, yend = y_pin4),
                 linewidth = 2,
                 col = "purple")+
    geom_point(data = distancePoint, aes(x = x, y = y), col = "red")+
    geom_text(data = distancePoint,
              aes(x = x, y = y, label = B_lab),
              col = "red",
              size = 7,
              hjust = -0.5)+
    geom_point(data = allpins_tb[pin %in% c("pin1", "pin2", "pin3", "pin4")],
               aes(x = x, y = y), col = "red")+
    geom_text(data = allpins_tb[pin %in% c("pin1", "pin2", "pin3", "pin4")],
              aes(x = x, y = y, label = pin),
              hjust = -0.5,
              col = "red")+
    geom_point(aes(x = 0, y = 0), col = "red")+
    geom_text(aes(x = 0, y = 0, label = "IPC"), col = "red", hjust = -0.5)+
    geom_point(data = treeLocation, aes(x = x, y = y, colour = as.factor(wkcodes)),
               shape = 10)+
    geom_text(data = treeLocation,
              aes(x = x, y = y,
                  label = treeNo),
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
    geom_text(data = allpins_tb_wide,
              aes(x = x_lab,
                  y = y_lab,
                  label = B_lab),
              hjust = 0)+
    geom_text(data = allpins_tb_wide,
              aes(x = x_lab,
                  y = pin_y,
                  label = pin_lab),
              hjust = 0)+
    theme_bw()+
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 10),
          panel.grid = element_blank(),
          legend.background = element_rect(colour = "black"),
          legend.direction = "vertical",
          legend.position = c(0.92, 0.12),
          legend.title = element_blank())

  return(list(figure = plotfigure,
              wkCode = treeLocation[,.(treeNo, bearing, distance, wkcodes)]))
}

#' To determine a tree-level flag in a walk-through sample using mirage method
#'
#' @description This function determines the walk-through zone and assign walk-through flag for a tree
#'
#' @param boundaryList list, A list of boundaries,
#'                           The first element is bearing (degree) for a walk-through edge.
#'                           The second element is distance (m) between IPC to the walk-through edge.
#' @param radiusYSM numeric, The radius (m) of a YSM plot, default is 11.28.
#' @param treeNo numeric, Defines a tree number.
#' @param bearingTree numeric, Defines bearing of a tree number from IPC.
#' @param distanceTree numeric, Defines distance of a tree number from IPC.
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
#' treelist <- data.frame(expand.grid(bearing_tree = seq(0, 360, by = 20),
#'                                    distance_tree = seq(3, 11, by = 2)))
#' treelist <- rbind(treelist, data.frame(bearing_tree = 50,
#'                                        distance_tree = 13)) # this tree is beyound 11.28 m
#' treelist$tree_no <- 1:nrow(treelist)
#' # example 1, one boundary in plot
#' plot_exam1 <- boundaryTreeFlag_mirage(boundaryList <- list(c(100, 6)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 2, two boundaries and out-of-plot zones are overlapped
#' plot_exam2 <- boundaryTreeFlag_mirage(boundaryList <- list(c(100, 6),
#'                                                           c(150, 7)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 3, two boundaries and out-of-plot zones are not overlapped
#' plot_exam3 <- boundaryTreeFlag_mirage(boundaryList <- list(c(100, 6),
#'                                                           c(250, 7)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#' # example 4, extreme case when a boundary is close to IPC
#' plot_exam4 <- boundaryTreeFlag_mirage(boundaryList <- list(c(300, 1)),
#'                                      treeNo = treelist$tree_no,
#'                                      bearingTree = treelist$bearing_tree,
#'                                      distanceTree = treelist$distance_tree)
#'}
#'
#' @importFrom ggplot2 ggplot geom_segment geom_sf geom_point geom_text theme aes guides guide_legend
#' @importFrom sf st_polygon st_sfc st_area st_combine st_sf st_cast st_union st_difference st_intersection
#' @rdname boundaryTreeFlag_mirage
#' @author Yong Luo
boundaryTreeFlag_mirage <- function(boundaryList,
                                    radiusYSM = 11.28,
                                    treeNo,
                                    bearingTree,
                                    distanceTree){
  ysmplotbearing <- c(seq(0, 359.9, by = 0.1), 0)
  ysmplot <- st_polygon(
    list(
      cbind(sin(ysmplotbearing*pi/180)*radiusYSM,
            cos(ysmplotbearing*pi/180)*radiusYSM)
    )
  )
  ysmplot <- st_sfc(ysmplot)
  plot_size <- round(st_area(ysmplot))

  allpins <- lapply(boundaryList,
                    function(x){walkThroughPins(bearingBoundary = x[1],
                                                distanceBoundary = x[2],
                                                radiusYSM)})
  allpins_tb <- NULL
  mirage_plot <- NULL
  for (i in 1:length(allpins)) {
    indibounary <- allpins[[i]]
    indipin_tb <- data.frame(boundary = i,
                             pin = names(unlist(indibounary)),
                             bearing = round(unlist(indibounary), 1),
                             bearingBoundary = boundaryList[[i]][1],
                             distanceBoundary = boundaryList[[i]][2],
                             radiusYSM = radiusYSM) %>%
      data.table
    allpins_tb <- rbind(allpins_tb, indipin_tb)
    mirage_plot_i <- data.table(boundary = i,
                                x = sin(ysmplotbearing*pi/180)*radiusYSM,
                                y = cos(ysmplotbearing*pi/180)*radiusYSM)
    mirage_plot <- rbind(mirage_plot, mirage_plot_i)
    rm(indipin_tb, indibounary, mirage_plot_i)
  }
  rm(i, allpins)
  new_center <- allpins_tb[pin == "angleBearingRight",
                           .(boundary,
                             x_center = sin(bearing*pi/180)*distanceBoundary*2,
                             y_center = cos(bearing*pi/180)*distanceBoundary*2)]
  mirage_plot <- merge(mirage_plot, new_center,
                       by = "boundary",
                       all.x = TRUE)
  mirage_plot[, ':='(x = x + x_center,
                     y = y + y_center)]



  allpins_tb[pin == "angleBearingRight",
             ':='(x = sin(bearing*pi/180)*distanceBoundary,
                  y = cos(bearing*pi/180)*distanceBoundary)]

  allpins_tb[pin %in% c(paste0("pin", 1:4)),
             ':='(x = sin(bearing*pi/180)*radiusYSM,
                  y = cos(bearing*pi/180)*radiusYSM)]
  allpins_tb <- merge(allpins_tb,
                      allpins_tb[pin == "angleBearingRight",
                                 .(boundary, distance_x = x, distance_y = y)],
                      by = "boundary",
                      all.x = TRUE)
  allpins_tb[distanceBoundary >= radiusYSM & pin == "pin1",
             ':='(x = sin(bearingBoundary*pi/180)*radiusYSM + distance_x,
                  y = cos(bearingBoundary*pi/180)*radiusYSM + distance_y)]
  allpins_tb[distanceBoundary >= radiusYSM & pin == "pin4",
             ':='(x = sin((bearingBoundary + 180)*pi/180)*radiusYSM + distance_x,
                  y = cos((bearingBoundary + 180)*pi/180)*radiusYSM + distance_y)]
  allpins_tb[,':='(distance_x = NULL,
                   distance_y = NULL)]

  allpins_tb_wide <- reshape(data = allpins_tb[pin %in% c("pin1", "pin4"),
                                               .(boundary, pin, x, y, bearing, distanceBoundary,
                                                 bearingBoundary,
                                                 radiusYSM)],
                             direction = "wide",
                             idvar = "boundary",
                             timevar = "pin",
                             v.names = c("bearing", "x", "y"),
                             sep = "_")

  allpins_tb_wide[, ':='(x_lab = -12,
                         y_lab = seq(13+nrow(allpins_tb_wide)*1.2, 13,
                                     length.out = nrow(allpins_tb_wide)),
                         B_lab = paste0("B", boundary,
                                        ": bearing=", bearingBoundary,
                                        "; distance=", distanceBoundary))]

  allpins_tb_wide[,':='(pin_y = seq(12+nrow(allpins_tb_wide)*1.2, 12,
                                    length.out = nrow(allpins_tb_wide)),
                        pin_lab = paste0("       pin1=", bearing_pin1,
                                         "; pin4=", bearing_pin4))]
  allpins_tb_wide[, boundaryName := paste0("B", boundary)]

  allpins_tb_wide[bearing_pin4 < bearing_pin1,
                  ':='(bearing_pin4 = bearing_pin4 + 360)]
  for (i in 1:nrow(allpins_tb_wide)) {
    boundarid <- allpins_tb_wide$boundary[i]
    pin1 <- allpins_tb_wide$bearing_pin1[i]
    pin4 <- allpins_tb_wide$bearing_pin4[i]
    polyInPlot_i <- data.frame(x = sin(c(seq(pin1, pin4, by = 0.1),
                                         pin1)*pi/180)*radiusYSM,
                               y = cos(c(seq(pin1, pin4, by = 0.1),
                                         pin1)*pi/180)*radiusYSM)
    polyInPlot_i <- st_polygon(
      list(
        cbind(polyInPlot_i$x,
              polyInPlot_i$y)
      )
    )
    polyInPlot_i <- st_sfc(polyInPlot_i)

    ## mirage plot polygons
    mirage_plot_i <- mirage_plot[boundary == boundarid,
                                 .(x, y)]
    mirage_plot_i <- st_polygon(
      list(
        cbind(mirage_plot_i$x,
              mirage_plot_i$y)
      )
    )
    mirage_plot_i <- st_sfc(mirage_plot_i)

    mirage_plot_i <- st_intersection(polyInPlot_i,
                                     mirage_plot_i)

    if(i == 1){
      walkThroughPoly <- mirage_plot_i
    } else {
      walkThroughPoly <- st_combine(c(walkThroughPoly, mirage_plot_i))
    }
    pin1_new <- if(pin1 < pin4){
      pin1 + 360
    } else {
      pin1
    }
    outPoly_i <- data.frame(x = sin(c(seq(pin4, pin1_new, by = 0.1),
                                      pin4)*pi/180)*radiusYSM,
                            y = cos(c(seq(pin4, pin1_new, by = 0.1),
                                      pin4)*pi/180)*radiusYSM)

    outPoly_i <- st_polygon(
      list(
        cbind(outPoly_i$x,
              outPoly_i$y)
      )
    )
    outPoly_i <- st_sfc(outPoly_i)

    if(i == 1){
      outPoly <- outPoly_i
    } else {
      outPoly <- st_combine(c(outPoly, outPoly_i))
    }
    rm(boundarid, pin1, polyInPlot_i, outPoly_i,
       mirage_plot_i)
  }
  rm(i)
  walkThroughPoly <- st_cast(st_sf(walkThroughPoly), "POLYGON")
  walkThroughPoly <- st_cast(st_union(walkThroughPoly), "POLYGON")

  outPoly <- st_cast(st_sf(outPoly), "POLYGON")
  outPoly <- st_cast(st_union(outPoly), "POLYGON")
  out_size <- round(sum(st_area(outPoly)))
  ## remove walkthrough zone in the out plot polygon
  walkThroughPoly <- st_cast(st_sf(st_difference(walkThroughPoly, outPoly)))
  walkThroughPoly <- st_cast(st_union(walkThroughPoly), "POLYGON")

  wk_size <- round(sum(st_area(walkThroughPoly)))
  treeLocation <- data.table(treeNo = treeNo,
                             bearing = bearingTree,
                             distance = distanceTree)
  treeLocation[, treedup := length(bearing), by = "treeNo"]
  treeDup <- treeLocation[treedup > 1]
  if(nrow(treeDup) > 0){
    stop(paste0("Tree number is duplicated: ", unique(treeDup$treeNo)))
  }
  rm(treeDup)
  treeLocation[, treedup := NULL]
  treeLocation[,':='(x = sin(bearing*pi/180)*distance,
                     y = cos(bearing*pi/180)*distance)]
  tree_stem <- st_as_sf(treeLocation, coords = c("x", "y"))

  trees_in_wk <- suppressWarnings(st_intersection(tree_stem, walkThroughPoly)) %>% data.table
  treeLocation[, wkcodes := "NA"]
  treeLocation[treeNo %in% trees_in_wk$treeNo,
               wkcodes := "W"]
  treeLocation[distance > radiusYSM,
               wkcodes := "Error"]

  trees_in_out <- suppressWarnings(st_intersection(tree_stem, outPoly)) %>% data.table
  treeLocation[treeNo %in% trees_in_out$treeNo,
               wkcodes := "O"]
  distancePoint <- allpins_tb[pin == "angleBearingRight",
                              .(x, y, B_lab = paste0("B", boundary))]

  plotfigure <- ggplot()+
    geom_sf(data = ysmplot, col = "grey", fill = "grey", alpha = 0.5)+
    geom_sf(data = walkThroughPoly, fill = "grey", col = "grey") +
    geom_sf(data = outPoly, fill = "white", col = "grey") +
    geom_segment(aes(x = 0, xend = distancePoint$x, y = 0, yend = distancePoint$y))+
    geom_segment(data = allpins_tb_wide,
                 aes(x = x_pin1, xend = x_pin4,
                     y = y_pin1, yend = y_pin4),
                 linewidth = 2,
                 col = "purple")+
    geom_point(data = distancePoint, aes(x = x, y = y), col = "red")+
    geom_text(data = distancePoint,
              aes(x = x, y = y, label = B_lab),
              col = "red",
              size = 7,
              hjust = -0.5)+
    geom_point(data = allpins_tb[pin %in% c("pin1", "pin4")],
               aes(x = x, y = y), col = "red")+
    geom_text(data = allpins_tb[pin %in% c("pin1", "pin4")],
              aes(x = x, y = y, label = pin),
              hjust = -0.5,
              col = "red")+
    geom_point(aes(x = 0, y = 0), col = "red")+
    geom_text(aes(x = 0, y = 0, label = "IPC"), col = "red", hjust = -0.5)+
    geom_point(data = treeLocation, aes(x = x, y = y, colour = as.factor(wkcodes)),
               shape = 10)+
    geom_text(data = treeLocation,
              aes(x = x, y = y,
                  label = treeNo),
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
    geom_text(data = allpins_tb_wide,
              aes(x = x_lab,
                  y = y_lab,
                  label = B_lab),
              hjust = 0)+
    geom_text(data = allpins_tb_wide,
              aes(x = x_lab,
                  y = pin_y,
                  label = pin_lab),
              hjust = 0)+
    theme_bw()+
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 10),
          panel.grid = element_blank(),
          legend.background = element_rect(colour = "black"),
          legend.direction = "vertical",
          legend.position = c(0.92, 0.12),
          legend.title = element_blank())
  return(list(figure = plotfigure,
              wkCode = treeLocation[,.(treeNo, bearing, distance, wkcodes)]))
}




#' To calculate 4 pins for a a walk-through sample
#'
#' @description This function is to calculate 4 pins for a walk-through sample
#'
#' @param bearingBoundary numeric, The bearing (degree) for a walk-through edge.
#' @param distanceBoundary numeric, The distance (m) between IPC to the walk-through edge.
#' @param radiusYSM numeric, The radius (m) of a YSM plot, default is 11.28.
#' @export
#' @docType methods
#' @note This function is equivalent to the excel provided by Chris, which is used in
#'       handheld.
#'
#' @rdname walkThroughPins
#' @author Yong Luo
walkThroughPins <- function(bearingBoundary,
                            distanceBoundary,
                            radiusYSM = 11.28){

  # determine 4 pins
  angleBearingRight <- if(bearingBoundary - 90 < 0){
    bearingBoundary - 90 + 360
  } else {bearingBoundary - 90}

  if(distanceBoundary >= radiusYSM){
    pin1 <- as.numeric(NA)
    pin4 <- as.numeric(NA)
  } else {
    pin1 <- if(acos(distanceBoundary/11.28)*180/pi+angleBearingRight>360){
      acos(distanceBoundary/11.28)*180/pi+angleBearingRight-360
    } else if(acos(distanceBoundary/11.28)*180/pi+angleBearingRight<0){
      acos(distanceBoundary/11.28)*180/pi+angleBearingRight+360
    } else {acos(distanceBoundary/11.28)*180/pi+angleBearingRight}
    pin4 <- if(angleBearingRight-acos(distanceBoundary/11.28)*180/pi>360){
      angleBearingRight-acos(distanceBoundary/11.28)*180/pi-360
    } else if(angleBearingRight-acos(distanceBoundary/11.28)*180/pi<0){
      angleBearingRight-acos(distanceBoundary/11.28)*180/pi+360
    } else {angleBearingRight-acos(distanceBoundary/11.28)*180/pi}
  }
  pin2 <- if(acos(distanceBoundary/2/11.28)*180/pi+angleBearingRight>360){
    acos(distanceBoundary/2/11.28)*180/pi+angleBearingRight-360
  } else if(acos(distanceBoundary/2/11.28)*180/pi+angleBearingRight<0){
    acos(distanceBoundary/2/11.28)*180/pi+angleBearingRight+360
  } else {
    acos(distanceBoundary/2/11.28)*180/pi+angleBearingRight
  }

  pin3 <- if(angleBearingRight-acos(distanceBoundary/2/11.28)*180/pi>360){
    angleBearingRight-acos(distanceBoundary/2/11.28)*180/pi-360
  } else if(angleBearingRight-acos(distanceBoundary/2/11.28)*180/pi<0){
    angleBearingRight-acos(distanceBoundary/2/11.28)*180/pi+360
  } else {
    angleBearingRight-acos(distanceBoundary/2/11.28)*180/pi
  }

  return(list(angleBearingRight = angleBearingRight,
              pin1 = pin1, pin2 = pin2, pin3 = pin3, pin4 = pin4))
}



