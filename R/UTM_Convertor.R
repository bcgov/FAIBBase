#' Convert UTM to other coordinate reference system.
#'
#' @description Converts UTM coordinates to the other coordinate reference system.
#'
#' @param point_ID character, Data point ID.
#' @param zone  integer, UTM zone.
#' @param northing integer, UTM northing.
#' @param easting integer, UTM easting.
#' @param CRS_To  character, Defines the spatial coordination reference that you wish to transform.
#'        Default is BC Albers reference system.
#' @param class character, Define the class of returned objective. Currently this function supports
#'              either \code{table} or \code{sf} class. Default is \code{table}.
#'
#' @return  Reprojected objective.
#'
#' @examples
#' \dontrun{
#' ## for Prince Rupert, Fort Nelson, Prince George, Victoria, Kelowna
#' citylocs <- UTM_Convertor(point_ID = c("Prince Rupert", "Prince George",
#'                                        "Victoria", "Kelowna"),
#'                           zone = c(9, 10, 10, 11),
#'                           northing = c(6019079.41, 5974323.27, 5361626.96, 5528467.98),
#'                           easting = c(415075.83, 516441.65, 475594.70, 321996.76),
#'                           class = "sf")
#' bcbdry <- bcmaps::bc_bound(class = "sp")
#' plot(bcbdry)
#' plot(citylocs, col = "red", size = 10, add = TRUE)
#' }
#'
#' @importFrom sf st_crs st_transform st_as_sf st_coordinates
#' @importFrom data.table data.table
#'
#' @export
#' @docType methods
#' @rdname UTM_Convertor
#'
#' @author Yong Luo
UTM_Convertor <-  function(point_ID, zone, northing,
                           easting,
                           CRS_To = "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs",
                           class = "sf") {
  CRS_To <- st_crs(CRS_To)
  UTMTable <- data.table::data.table(point_ID = point_ID,
                                     Zone = zone, Northing = northing,
                                     Easting = easting)
  fullUTMInfor <- UTMTable[!is.na(Zone) & !is.na(Easting) & !is.na(Northing),]
  output <- UTMTable[!(point_ID %in% fullUTMInfor$point_ID),.(point_ID,
                                                              Longitude = as.numeric(NA),
                                                              Latitude = as.numeric(NA))]
  utmZones <- unique(fullUTMInfor$Zone)
  for(utmZone in utmZones){
    outputAdded <- fullUTMInfor[Zone == utmZone, ]
    crsUTMstring <- st_crs(paste("+proj=utm +zone=", utmZone, sep=""))
    utmcoor <- st_as_sf(outputAdded,
                        coords = c("Easting",
                                   "Northing"),
                        crs = crsUTMstring)
    longlatcoor <- st_transform(utmcoor, CRS_To)

    transformed <- data.table(st_coordinates(longlatcoor))
    names(transformed) <- c("Longitude", "Latitude")
    outputAdded <- cbind(data.table(longlatcoor),
                         transformed)
    output <- rbindlist(list(output, outputAdded[,.(point_ID, Longitude, Latitude)]))
  }

  if(class == "table"){
    return(output[,.(point_ID, Longitude, Latitude)])
  } else if (class == "sf"){
    output_valid <- output[!is.na(Longitude) & !is.na(Latitude),]
    output_invalid <- output[!(point_ID %in% unique(output_valid$point_ID)),]
    row.names(output_valid) <- output_valid$point_ID
    utmcoor <- st_as_sf(output_valid,
                        coords = c("Longitude",
                                   "Latitude"),
                        crs = CRS_To)
    if(nrow(output_invalid) > 0){
      warning(paste("The below point(s) can not be converted due to missing UTM information:\n",
                    data.frame(output_invalid[,.(point_ID)]),
                    sep = ""))
    }
    return(utmcoor)
  } else {
    stop("class is not properly defined.")
  }
}
