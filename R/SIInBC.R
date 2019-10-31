#' Derive site index for a given spatial coverage or a spatial point
#'
#'
#' @description This function is to derive species' site index for a given spatial coverage or
#'              spatial points based on BC provincial species productivity maps.
#'
#' @param SIMapPath character, Specifies folder location of species index maps. Please request
#'                       all the maps from author and save them into your target folder.
#'                       The function only supports \code{TIFF} format. Currently those
#'                       maps were converted from BC Data catalogue.
#' @param spatialCoverage spatialPolygons or spatialPoints, Specifies spatial polygons or spatial points that need to
#'                        intersect.
#' @param species character, Must be one or some of 22 species.
#' @param returnClass character, Specifies the class you intended to return from either
#'                    \code{sp} or \code{table}. If missing, \code{table} will be used.
#'
#' @return the returned value depends on \code{returnClass} arguement and class of
#'         \code{spatialCoverage} arguement. If \code{returnClass} is set as \code{table},
#'         a table will be returned. If \code{returnClass} is set as \code{sp}, a raster layer
#'         will be returned for SpatialPolygons* objects, while a SpatialPointDataframe will be
#'         returned for SpatialPoints* objects.
#'
#' @importFrom data.table ':=' data.table
#' @importFrom dplyr '%>%'
#' @importFrom raster raster mask crop stack extract getValues crs
#' @importFrom sp spTransform SpatialPointsDataFrame coordinates
#' @export
#'
#' @rdname SIInBC
#' @author Yong Luo
SIInBC <- function(SIMapPath, spatialCoverage,
                   species = "all", returnClass = "table"){
  allspecies <- c("At", "Ba", "Bg", "Bl", "Cw", "Dr", "Ep",
                  "Fd", "Hm", "Hw", "Lt", "Lw", "Pa", "Pl",
                  "Pw", "Py", "Sb", "Se", "Ss", "Sw", "Sx",
                  "Yc") ## should have 22 species
  if(length(species) == 1){
    if(toupper(species) == "ALL"){
      species <- allspecies
    }
  }

  if(length(species) > 1){
    speciesValid <- species[toupper(species) %in% toupper(allspecies)]
    if(length(species[!(toupper(species) %in% toupper(allspecies))]) > 0){
      warning("Species: ",
              paste0(species[!(toupper(species) %in% toupper(allspecies))], collapse = ", "),
              " are not valid species.")
    }

    for(indispecies in speciesValid){
      indispeciesmap <- raster::raster(file.path(SIMapPath,
                                                 paste0("Site_Prod_", indispecies, ".tif")))
      if(indispecies == speciesValid[1]){
        speciesmaps <- stack(indispeciesmap)
      } else {
        speciesmaps <- stack(speciesmaps, indispeciesmap)
      }
    }
  } else {
    speciesmaps <- raster::raster(file.path(SIMapPath,
                                            paste0("Site_Prod_", species, ".tif")))
  }
  spatialCoverage <- sp::spTransform(spatialCoverage,
                                     CRSobj = raster::crs(speciesmaps))
  if(class(spatialCoverage) %in% c("SpatialPolygons",
                                   "SpatialPolygonsDataFrame")){
    speciesSpatialMaps <- raster::crop(speciesmaps,
                                       spatialCoverage)
    speciesSpatialMaps <- raster::mask(speciesSpatialMaps,
                                       spatialCoverage)
    if(returnClass == "table"){
      refraster <- raster(nrows = nrow(speciesSpatialMaps),
                          ncols = ncol(speciesSpatialMaps),
                          xmn = xmin(speciesSpatialMaps),
                          xmx = xmax(speciesSpatialMaps),
                          ymn = ymin(speciesSpatialMaps),
                          ymx = ymax(speciesSpatialMaps),
                          crs = raster::crs(speciesSpatialMaps),
                          vals = rep(1, ncell(speciesSpatialMaps)))
      refraster <- raster::mask(refraster, spatialCoverage)
      gc()
      outputtable <- data.table::data.table(data.frame(coordinates(speciesSpatialMaps)))
      outputtable[, pointID := 1:nrow(outputtable)]
      newspatialpoints <- SpatialPointsDataFrame(coords = outputtable[,.(x, y)],
                                                 data = outputtable,
                                                 proj4string = raster::crs(speciesSpatialMaps))

      newspatialpoints <- spTransform(newspatialpoints,
                                      CRSobj = raster::crs("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      gc()
      outputtable <- data.table::data.table(data.frame(coordinates(newspatialpoints)))
      names(outputtable) <- c("ALBERS_X", "ALBERS_Y")
      rm(newspatialpoints)
      gc()
      siteindextable <- data.table(getValues(speciesSpatialMaps))
      names(siteindextable) <- paste0(species, "_SI")
      siteindex <- cbind(outputtable,
                         siteindextable)
      siteindex[, reference := getValues(refraster)]
      rm(outputtable, refraster)
      siteindex <- siteindex[!is.na(reference),]
      gc()
      siteindex[, reference := NULL]
      return(siteindex)
    } else if(returnClass == "sp"){
      return(speciesSpatialMaps)
    } else {
      stop("Please specify returnClass correctly.")
    }
  } else if (class(spatialCoverage) %in% c("SpatialPoints",
                                           "SpatialPointsDataFrame",
                                           "SpatialLines")){
    siteindex <- data.table(data.frame(raster::extract(speciesmaps, spatialCoverage)))
    names(siteindex) <- paste0(speciesValid, "_SI")
    spatialCoverage <- spTransform(spatialCoverage,
                                   CRSobj = raster::crs("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    locations <- data.table(data.frame(coordinates(spatialCoverage)))
    names(locations) <- c("ALBERS_X", "ALBERS_Y")
    if(class(spatialCoverage) %in% c("SpatialPoints",
                                     "SpatialLines")){
      siteindex <- cbind(data.table(pointID = 1:nrow(siteindex)),
                         locations,
                         siteindex)
    } else {
      siteindex <- cbind(data.table(spatialCoverage@data),
                         locations,
                         siteindex)
    }

    if(returnClass == "table"){
      return(siteindex)
    } else if (returnClass == "sp"){
      finalmap <- SpatialPointsDataFrame(coords = siteindex[,.(ALBERS_X, ALBERS_Y)],
                                         data = siteindex,
                                         proj4string = raster::crs("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      return(finalmap)
    } else {
      stop("Please specify returnClass correctly.")
    }
  }
}
