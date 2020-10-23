#' Generic function to derive BEC, TSA and FIZ for given locations
#'
#' @description This function is to derive \code{BEC}, \code{TSA} or {FIZ} based on an UTM location and BEC map.
#'
#' @param pointID character, Data point ID.
#' @param zone  integer, UTM zone.
#' @param northing integer, UTM northing.
#' @param easting integer, UTM easting.
#' @param spatialMap SpatialPolygonsDataFrame, Spatial map.
#' @param spatialAttribute character, Specifies which spatial attribute to be obtained.
#'                                    Must be one of "BEC", "TSA", "FIZ", "TFL" and "OWNERSHIP",
#'                                    regardless of lower or upper cases. Must be consistent with
#'                                    \code{spatialMap} arguement.
#'
#' @return Depends on what spatial attribute a function derives.
#'         For \code{BEC}, a table that contains:
#'         \itemize{
#'         \item{bec_zone} {bec zone.}
#'         \item{bec_sbz} {bec subzone.}
#'         \item{bec_var} {bec variant}
#'         }
#'         For \code{TSA}, a table that contains:
#'         \itemize{
#'         \item{tsa} {tsa.}
#'         \item{tsa_desc} {tsa descriptions.}
#'         }
#'         For \code{FIZ}, a table that contains:
#'         \itemize{
#'         \item{fiz} {fiz forest inventory zone.}
#'         }
#'         For \code{TFL}, a table that contains:
#'         \itemize{
#'         \item{tfl} {tfl timber farm licenses.}
#'         }
#'         For \code{OWNERSHIP}, a table that contains:
#'         \itemize{
#'         \item{owner} {owner of land.}
#'         \item{schedule} {schedule.}
#'         }
#' @importFrom data.table data.table ':=' set
#' @importFrom dplyr '%>%'
#' @importFrom sp spTransform identicalCRS
#' @importFrom raster intersect
#' @importFrom sf st_as_sf st_intersection st_crs
#' @examples
#' \dontrun{
#'  ## for Prince Rupert, Fort Nelson, Prince George, Victoria, Kelowna
#'  citylocs <- data.frame(point_ID = c("Prince Rupert", "Prince George", "Victoria", "Kelowna"),
#'                         zone = c(9, 10, 10, 11),
#'                         northing = c(6019079.41, 5974323.27, 5361626.96, 5528467),
#'                         easting = c(415075.83, 516441.65, 475594.70, 321996.76))
#'  tsamap <- bcmaps::tsa(class = "sp")
#'  city_tsa <- getSpatial(pointID = citylocs$point_ID,
#'                         zone = citylocs$zone,
#'                         northing = citylocs$northing,
#'                         easting = citylocs$easting,
#'                         spatialMap = tsamap,
#'                         spatialAttribute = "TSA")
#'  print(city_tsa)
#'  #         pointID tsa          tsa_desc
#'  #   Prince Rupert  46     GBR North TSA
#'  #   Prince George  24 Prince George TSA
#'  #        Victoria  38    Arrowsmith TSA
#'  #         Kelowna  22      Okanagan TSA
#'
#'  becmap <- bcmaps::bec(class = "sp")
#'  city_bec <- getSpatial(pointID = citylocs$point_ID,
#'                         zone = citylocs$zone,
#'                         northing = citylocs$northing,
#'                         easting = citylocs$easting,
#'                         spatialMap = becmap,
#'                         spatialAttribute = "bec")
#'  print(city_bec)
#'  #        pointID bec_zone bec_sbz bec_var
#'  #  Prince Rupert      CWH      vh       2
#'  #  Prince George      SBS      mh    <NA>
#'  #       Victoria      CDF      mm    <NA>
#'  #        Kelowna       PP      xh       1
#' }
#' @export
#' @rdname getSpatial
#'
#' @author Yong Luo
getSpatial <- function(pointID, zone, northing, easting,
                       spatialMap, spatialAttribute){
  if(missing(pointID)){
    pointID <- as.character(1:(max(length(zone), length(northing), length(easting))))
  }
  connectionTable <- data.table(tempID = as.character(1:length(pointID)),
                                pointID)
  pointmap <- UTM_Convertor(point_ID = connectionTable$tempID,
                            zone = zone,
                            northing = northing,
                            easting = easting,
                            class = "sp")

  spatialAttribute <- toupper(spatialAttribute)
  if(spatialAttribute == "OWNERSHIP"){
    pointmap <- sf::st_as_sf(pointmap)
    spatialMap <- sf::st_as_sf(spatialMap)
    if(st_crs(spatialMap) != st_crs(pointmap)){
      pointmap <- suppressWarnings(sf::st_transform(pointmap, sf::st_crs(spatialMap)))
    }
    pointmap_data <- suppressWarnings(sf::st_intersection(pointmap, spatialMap)) %>%
      data.table
  } else {
    if(!identicalCRS(pointmap, spatialMap)){
      pointmap <- suppressWarnings(sp::spTransform(pointmap, crs(spatialMap)))
    }
    pointmap_tmp <- suppressWarnings(raster::intersect(pointmap, spatialMap))
    coordata <- pointmap_tmp@coords %>% data.frame
    coordata$newid <- row.names(pointmap_tmp@coords)
    pointmap_data <- cbind(coordata,
                           pointmap_tmp@data) %>% data.table
    pointmap_data[, ':='(point_ID = newid,
                         Longitude = coords.x1,
                         Latitude = coords.x2)]
    pointmap_data[,':='(newid = NULL,
                        coords.x1 = NULL,
                        coords.x2 = NULL)]
  }


  if (spatialAttribute == "BEC"){
    pointmap_data <- pointmap_data[,.(tempID = point_ID,
                                      bec_zone = ZONE,
                                      bec_sbz = SUBZONE,
                                      bec_var = VARIANT)]
    pointmap_data <- merge(connectionTable, pointmap_data,
                           by = "tempID", all.x = TRUE)
    pointmap_data <- pointmap_data[!duplicated(pointmap_data),]
    pointmap_data[, tempID := as.numeric(tempID)]
    pointmap_data <- pointmap_data[order(tempID), .(pointID, bec_zone, bec_sbz, bec_var)]
  } else if (spatialAttribute == "TSA"){
    pointmap_data <- pointmap_data[is.na(RETIREMENT_DATE)] # remove the tsa that retired
    pointmap_data[, WHEN_UPDATED := as.Date(WHEN_UPDATED)]
    pointmap_data[, last_UPDATED := max(WHEN_UPDATED), by = "point_ID"] # select the most recent updates
    pointmap_data <- pointmap_data[WHEN_UPDATED == last_UPDATED,]
    pointmap_data <- pointmap_data[,.(tempID = point_ID,
                                      tsa = TSA_NUMBER,
                                      tsa_desc = TSA_NUMBER_DESCRIPTION)]
    pointmap_data <- merge(connectionTable, pointmap_data,
                           by = "tempID", all.x = TRUE)
    pointmap_data <- pointmap_data[!duplicated(pointmap_data),]
    pointmap_data[, tempID := as.numeric(tempID)]
    pointmap_data <- pointmap_data[order(tempID), .(pointID, tsa, tsa_desc)]
  } else if (spatialAttribute == "FIZ"){
    pointmap_data <- pointmap_data[,.(tempID = point_ID,
                                      FIZ)]
    pointmap_data <- merge(connectionTable, pointmap_data,
                           by = "tempID", all.x = TRUE)
    pointmap_data <- pointmap_data[!duplicated(pointmap_data),]
    pointmap_data[, tempID := as.numeric(tempID)]
    pointmap_data <- pointmap_data[order(tempID), .(pointID, FIZ)]
  } else if (spatialAttribute == "TFL"){
    pointmap_data <- pointmap_data[,.(tempID = point_ID,
                                      TFL = FOREST_FIL)]
    pointmap_data <- merge(connectionTable, pointmap_data,
                           by = "tempID", all.x = TRUE)
    pointmap_data <- pointmap_data[!duplicated(pointmap_data),]
    pointmap_data[, tempID := as.numeric(tempID)]
    pointmap_data <- pointmap_data[order(tempID), .(pointID, TFL)]
  } else if (spatialAttribute == "OWNERSHIP"){
    names(pointmap_data) <- toupper(names(pointmap_data))
    pointmap_data <- pointmap_data[,.(tempID = POINT_ID,
                                      OWNER = OWN,
                                      SCHEDULE)]
    pointmap_data <- merge(connectionTable, pointmap_data,
                           by = "tempID", all.x = TRUE)
    pointmap_data <- pointmap_data[!duplicated(pointmap_data),]
    pointmap_data[, tempID := as.numeric(tempID)]
    pointmap_data <- pointmap_data[order(tempID), .(pointID, OWNER, SCHEDULE)]
  } else {
    stop("spatialAttribute is not correctly specified.")
  }
  return(pointmap_data)
}

