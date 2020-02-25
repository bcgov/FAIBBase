#' Generic function to derive BEC, TSA and FIZ for given locations
#'
#' @description This function is to derive \code{BEC}, \code{TSA} or {FIZ} based on an UTM location and BEC map.
#'
#'
#' @param pointID character, Data point ID.
#' @param zone  integer, UTM zone.
#' @param northing integer, UTM northing.
#' @param easting integer, UTM easting.
#' @param spatialAttribute character, specifies which spatial attribute to be obtained. Must be one of
#'                         "BEC", "TSA", "FIZ", "TFL" and "OWNERSHIP", regardless of lower or upper cases.
#'                         The spatial attribute \code{BEC} and \code{TSA} are
#'                         available at \code{\link[bcmaps]{bec}} and \code{\link[bcmaps]{tsa}}.
#'                         Therefore, for these attributes, \code{mapPath}, \code{mapName} and
#'                         \code{mapFormat} should not be specified. Since \code{FIZ}, \code{TFL} and
#'                         \code{OWNERSHIP} map is not available from any R package.
#'                         The below arguements must be specified.
#' @param mapPath character, Path to map.
#' @param mapName character, Map name of the map.
#' @param mapFormat character, Map format of the map.
#'
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
#'
#'
#'
#'
#' @importFrom data.table data.table ':=' set
#' @importFrom dplyr '%>%'
#' @importFrom bcmaps bec
#' @importFrom sp spTransform identicalCRS
#' @importFrom raster intersect
#' @importFrom rgdal readOGR
#'
#' @export
#' @rdname getSpatial
#'
#' @author Yong Luo
#'
getSpatial <- function(pointID, zone, northing, easting,
                       spatialAttribute = "all", mapPath = as.character(NA),
                       mapName = as.character(NA), mapFormat = "gdb"){
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
  if(spatialAttribute %in% c("BEC", "TSA", "FIZ", "TFL", "OWNERSHIP")){
    if(spatialAttribute == "BEC"){
      spatialMap <- bcmaps::bec(class = "sp")
    } else if (spatialAttribute == "TSA") {
      spatialMap <- bcmaps::tsa(class = "sp")
    } else if (spatialAttribute == "FIZ"){
      if(is.na(mapPath) | is.na(mapName) | !(mapFormat %in% c("shp", "gdb"))){
        stop("mapPath, mapName and mapFormat must be correctly specified to derive FIZ.")
      }
      gdbpath <- file.path(mapPath, paste(mapName, ".", mapFormat, sep = ""))
      spatialMap <- rgdal::readOGR(dsn = gdbpath, layer = "forest_inventory_zone")
    } else if (spatialAttribute == "TFL"){
      gdbpath <- file.path(mapPath, paste(mapName, ".", mapFormat, sep = ""))
      spatialMap <- rgdal::readOGR(dsn = gdbpath, layer = "TFL_BusinessView_March2018")
    } else if (spatialAttribute == "OWNERSHIP"){
      gdbpath <- file.path(mapPath, paste(mapName, ".", mapFormat, sep = ""))
      spatialMap <- rgdal::readOGR(dsn = gdbpath, layer = "dissolve_multi")
    }
    if(!sp::identicalCRS(spatialMap, pointmap)){
      pointmap <- suppressWarnings(sp::spTransform(pointmap, crs(spatialMap)))
    }

    pointmap_new <- raster::intersect(pointmap, spatialMap)
    pointmap_data <- pointmap_new@data %>% data.table
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
      pointmap_data <- pointmap_data[,.(tempID = point_ID,
                                        OWNER = OWN,
                                        SCHEDULE)]
      pointmap_data <- merge(connectionTable, pointmap_data,
                             by = "tempID", all.x = TRUE)
      pointmap_data <- pointmap_data[!duplicated(pointmap_data),]
      pointmap_data[, tempID := as.numeric(tempID)]
      pointmap_data <- pointmap_data[order(tempID), .(pointID, OWNER, SCHEDULE)]
    }
    return(pointmap_data)
  } else {
    stop("spatialAttribute is not correctly specified.")
  }
}
