################################################################################
#' Standardize species name from different forest inventory data, this function to make all
#' the species compatible to biomassCalculation function
#'
#'
#' @param speciesTable  data table. It must at least have one column species
#'
#'
#' @param forestInventorySource,  Character string. Give the forest inventory data source
#'        Currently support MBPSP, MBTSP, ABPSP, BCPSP, SKPSP, SKTSP and NFIPSP
#'
#'
#' @return  a data tables, the first one contains successfully standardized species.
#'          the newSpeciesName is the standardized name, unknown means the species in
#'          the original species table can not be found according to manual
#'
#' @importFrom data.table ':='
#'
#' @note no note
#'
#' @seealso no
#'
#' @docType methods
#' @rdname standardizeSpeciesName
#'
#' @author Yong Luo
#'
setGeneric("standardizeSpeciesName", function(speciesTable, forestInventorySource) {
  standardGeneric("standardizeSpeciesName")
})
#' @export
#' @rdname standardizeSpeciesName
setMethod(
  "standardizeSpeciesName",
  signature = c(speciesTable = "data.table",
                forestInventorySource = "character"),
  definition = function(speciesTable, forestInventorySource) {
    if(forestInventorySource == "MBPSP" | forestInventorySource == "MBTSP"){
      speciesTable[Species == "AE" | Species == "E" | Species == "WE",
                   newSpeciesName := "white elm"] # assume WE is white elm in MB PSP
      speciesTable[Species == "GA", newSpeciesName := "red ash"] # green ash = red ash
      speciesTable[Species == "WP", newSpeciesName := "white pine"]
      speciesTable[Species == "RP", newSpeciesName := "red pine"]
      speciesTable[Species == "JP" | Species == "jp", newSpeciesName := "jack pine"]
      speciesTable[Species == "SP", newSpeciesName := "scots pine"]
      speciesTable[Species == "BS" | Species == "bs" | Species == " BS",
                   newSpeciesName := "black spruce"]
      speciesTable[Species == "WS" | Species == "ws", newSpeciesName := "white spruce"]
      speciesTable[Species == "BF" | Species == "bf", newSpeciesName := "balsam fir"]
      speciesTable[Species == "TL" | Species == "tl", newSpeciesName := "tamarack larch"]
      speciesTable[Species == "EC", newSpeciesName := "eastern redcedar"]
      # assume cedar in original document is eastern redcedar
      speciesTable[Species == "TA" | Species == "ta", newSpeciesName := "trembling aspen"]
      speciesTable[Species == "LA", newSpeciesName := "largetooth aspen"]
      speciesTable[Species == "BA" | Species == "ba", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "CO", newSpeciesName := "eastern cottonwood"]
      speciesTable[Species == "W", newSpeciesName := "willow"]
      speciesTable[Species == "WB" | Species == "wb", newSpeciesName := "white birch"]
      speciesTable[Species == "B" | Species == "BW", newSpeciesName := "basswood"]
      speciesTable[Species == "HB", newSpeciesName := "hackberry"]
      speciesTable[Species == "MM", newSpeciesName := "manitoba maple"]
      speciesTable[Species == "AS", newSpeciesName := "black ash"]
      # assume ash in original document is black ash
      speciesTable[Species == "HH", newSpeciesName := "hop-hornbeam"]
      speciesTable[Species == "BO", newSpeciesName := "white oak"]
      # assume bur oak in original document is white oak
    } else if (forestInventorySource == "BCPSP"){
      speciesTable[Species == "SB", newSpeciesName := "black spruce"]
      speciesTable[Species == "LT", newSpeciesName := "tamarack larch"]
      speciesTable[Species == "SW", newSpeciesName := "white spruce"]
      speciesTable[Species == "EP", newSpeciesName := "white birch"]
      speciesTable[Species == "PLI", newSpeciesName := "lodgepole pine"]
      speciesTable[Species == "ACB", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "S", newSpeciesName := "black spruce"]
      # S for spruce in original doc. assume S is black spruce
      speciesTable[Species == "PL", newSpeciesName := "lodgepole pine"]
      speciesTable[Species == "ACT", newSpeciesName := "black cottonwood"]
      speciesTable[Species == "AT", newSpeciesName := "trembling aspen"]
      speciesTable[Species == "E", newSpeciesName := "white birch"]
      # E for birch in original doc. assume S is white birch
      # speciesTable[Species == "XC", newSpeciesName := "unknow conifer"]
      speciesTable[Species == "BL", newSpeciesName := "alpine fir"]
      speciesTable[Species == "EA", newSpeciesName := "white birch"]
      # EA is alaka paper birch, assume it is white birch here
      speciesTable[Species == "AC", newSpeciesName := "balsam poplar"]
      # speciesTable[Species == "X", newSpeciesName := "unknown"]
      speciesTable[Species == "B", newSpeciesName := "fir"]
      speciesTable[Species == "W", newSpeciesName := "willow"]
      speciesTable[Species == "DR", newSpeciesName := "red alder"]
      speciesTable[Species == "DM", newSpeciesName := "mountain alder"]
      speciesTable[Species == "PJ", newSpeciesName := "jack pine"]
      speciesTable[Species == "ZH", newSpeciesName := "hardwood"]
      speciesTable[Species == "SXW", newSpeciesName := "white spruce"]
      # SXW is hybrid between engelmann and white spruce
      speciesTable[Species == "XH", newSpeciesName := "hardwood"]
      speciesTable[Species == "SX", newSpeciesName := "white spruce"]
      # SX is hybrid
      speciesTable[Species == "A", newSpeciesName := "trembling aspen"]
      # A is aspen cottonwood and poplar, assume it is trembling aspen
      speciesTable[Species == "P", newSpeciesName := "lodgepole pine"]
      # P is pine Species, assume it is lodgepole pine
      speciesTable[Species == "MR", newSpeciesName := "red maple"]
      # MR could not find, assume it is red maple
      speciesTable[Species == "SE", newSpeciesName := "engelmann spruce"]
      speciesTable[Species == "FD", newSpeciesName := "douglas-fir"]
      speciesTable[Species == "BA", newSpeciesName := "amabalis fir"]
      speciesTable[Species == "CW", newSpeciesName := "western redcedar"]
      speciesTable[Species == "HW", newSpeciesName := "western hemlock"]
      speciesTable[Species == "FDI", newSpeciesName := "douglas-fir"]
      # speciesTable[Species == "SXE", newSpeciesName := "unknown"]
      speciesTable[Species == "D", newSpeciesName := "red alder"]
      # D is alder, assume red alder
      speciesTable[Species == "H", newSpeciesName := "western hemlock"]
      speciesTable[Species == "MV", newSpeciesName := "vine maple"]
      speciesTable[Species == "HM", newSpeciesName := "mountain hemlock"]
      speciesTable[Species == "EXP", newSpeciesName := "white birch"]
      speciesTable[Species == "WS", newSpeciesName := "scoulers willow"]
      speciesTable[Species == "AX", newSpeciesName := "trembling aspen"]
      # hybrid poplars for AX, assume trembling aspen
    } else if(forestInventorySource == "ABPSP"){
      # speciesTable[Species == "  ", newSpeciesName := "unknown"]
      speciesTable[Species == "AW", newSpeciesName := "trembling aspen"]
      speciesTable[Species == "BW", newSpeciesName := "white birch"]
      speciesTable[Species == "FA", newSpeciesName := "alpine fir"]
      speciesTable[Species == "FB", newSpeciesName := "balsam fir"]
      speciesTable[Species == "FD", newSpeciesName := "douglas-fir"]
      speciesTable[Species == "LA", newSpeciesName := "alpine larch"]
      speciesTable[Species == "LT", newSpeciesName := "tamarack larch"]
      # speciesTable[Species == "P ", newSpeciesName := "unknown"]
      speciesTable[Species == "PB", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "PF", newSpeciesName := "limber pine"]
      speciesTable[Species == "PJ", newSpeciesName := "jack pine"]
      speciesTable[Species == "PL", newSpeciesName := "lodgepole pine"]
      speciesTable[Species == "PW", newSpeciesName := "whitebark pine"]
      speciesTable[Species == "SB", newSpeciesName := "black spruce"]
      speciesTable[Species == "SE", newSpeciesName := "engelmann spruce"]
      speciesTable[Species == "SW", newSpeciesName := "white spruce"]
    } else if(forestInventorySource == "SKPSP" | forestInventorySource == "SKTSP"){
      # speciesTable[Species == "", newSpeciesName := "unknown"]
      speciesTable[Species == "BF", newSpeciesName := "balsam fir"]
      speciesTable[Species == "BP", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "BS", newSpeciesName := "black spruce"]
      # speciesTable[Species == "DC", newSpeciesName := "unknown"] # what is this
      # speciesTable[Species == "DD", newSpeciesName := "unknown"] # what is this
      # speciesTable[Species == "DU", newSpeciesName := "unknown"] # what is this
      speciesTable[Species == "GA", newSpeciesName := "green ash"]
      speciesTable[Species == "JP", newSpeciesName := "jack pine"]
      speciesTable[Species == "MM", newSpeciesName := "manitoba maple"]
      # speciesTable[Species == "PC", newSpeciesName := "unknown"] # what is this
      speciesTable[Species == "TA", newSpeciesName := "trembling aspen"]
      speciesTable[Species == "TL", newSpeciesName := "tamarack larch"]
      # speciesTable[Species == "UI", newSpeciesName := "unknown"] # what is this
      speciesTable[Species == "WB", newSpeciesName := "white birch"]
      speciesTable[Species == "WE", newSpeciesName := "white elm"]
      speciesTable[Species == "WS", newSpeciesName := "white spruce"]
      # speciesTable[Species == "XX", newSpeciesName := "unknown"] # what is this
    } else if(forestInventorySource == "NWTTSP"){
      speciesTable[Species == "A", newSpeciesName := "trembling aspen"]
      speciesTable[Species == "BW", newSpeciesName := "white birch"]
      speciesTable[Species == "Bw", newSpeciesName := "white birch"]
      speciesTable[Species == "F", newSpeciesName := "balsam fir"]
      speciesTable[Species == "FB", newSpeciesName := "balsam fir"]
      speciesTable[Species == "L", newSpeciesName := "tamarack larch"]
      speciesTable[Species == "LT", newSpeciesName := "tamarack larch"]
      speciesTable[Species == "PB", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "Pj", newSpeciesName := "jack pine"]
      speciesTable[Species == "PJ", newSpeciesName := "jack pine"]
      speciesTable[Species == "Pl", newSpeciesName := "lodgepole pine"]
      speciesTable[Species == "PL", newSpeciesName := "lodgepole pine"]
      speciesTable[Species == "Po", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "PO", newSpeciesName := "balsam poplar"]
      speciesTable[Species == "Sb", newSpeciesName := "black spruce"]
      speciesTable[Species == "SB", newSpeciesName := "black spruce"]
      speciesTable[Species == "sw", newSpeciesName := "white spruce"]
      speciesTable[Species == "Sw", newSpeciesName := "white spruce"]
      speciesTable[Species == "SW", newSpeciesName := "white spruce"]
      speciesTable[Species == "W", newSpeciesName := "willow"]
    } else if(forestInventorySource == "NFIPSP"){
      # the Species code system in NFI is different from provincial systems
      # the speciesTable for NFIPSP must have Genus and Species column

      speciesTable[Genus == "ABIE" & Species == "BAL",
                   newSpeciesName := "balsam fir"]
      speciesTable[Genus == "ABIE" & Species == "LAS",
                   newSpeciesName := "subalpine fir"]
      speciesTable[Genus == "ACER" & Species == "SPI",
                   newSpeciesName := "mountain maple"]
      speciesTable[Genus == "ALNU" & Species == "SPP",
                   newSpeciesName := "alder"]
      speciesTable[Genus == "ALNU" & Species == "INC",
                   newSpeciesName := "gray alder"]
      speciesTable[Genus == "ALNU" & Species == "RUB",
                   newSpeciesName := "red alder"]
      speciesTable[Genus == "ALNU" & Species == "CRI",
                   newSpeciesName := "alder"]
      speciesTable[Genus == "ALNU" & Species == "VIR",
                   newSpeciesName := "sitka alder"]
      speciesTable[Genus == "AMEL" & Species == "ALN",
                   newSpeciesName := "saskatoon-berry"]
      speciesTable[Genus == "BETU" & Species == "GLA",
                   newSpeciesName := "birch"]

      speciesTable[Genus == "BETU" & Species == "NAN",
                   newSpeciesName := "birch"] # not found
      speciesTable[Genus == "BETU" & Species == "GLA",
                   newSpeciesName := "birch"] # not found
      speciesTable[Genus == "BETU" & Species == "NEO",
                   newSpeciesName := "white birch"] # alaska paper birch
      speciesTable[Genus == "BETU" & Species == "OCC",
                   newSpeciesName := "white birch"] # water birch
      speciesTable[Genus == "BETU" & Species == "PAP",
                   newSpeciesName := "white birch"]
      speciesTable[Genus == "BETU" & Species == "PUM",
                   newSpeciesName := "birch"] # not found
      speciesTable[Genus == "BETU" & Species == "SPP",
                   newSpeciesName := "birch"] # water birch
      speciesTable[Genus == "CORN" & Species == "STO",
                   newSpeciesName := "redosier dogwood"] # water birch
      speciesTable[Genus == "CORY" & Species == "COR",
                   newSpeciesName := "unknown"] # water birch
      speciesTable[Genus == "FRAX" & Species == "PEN",
                   newSpeciesName := "red ash"]
      speciesTable[Genus == "GENC" & Species == "SPP",
                   newSpeciesName := "softwood"]
      speciesTable[Genus == "GENH" & Species == "SPP",
                   newSpeciesName := "hardwood"]
      speciesTable[Genus == "LARI" & Species == "LAR",
                   newSpeciesName := "tamarack larch"]
      speciesTable[Genus == "LARI" & Species == "OCC",
                   newSpeciesName := "western larch"]
      speciesTable[Genus == "PICE" & Species == "ENG",
                   newSpeciesName := "engelmann spruce"]
      speciesTable[Genus == "PICE" & Species == "GLA",
                   newSpeciesName := "white spruce"]
      speciesTable[Genus == "PICE" & Species == "MAR",
                   newSpeciesName := "black spruce"]
      speciesTable[Genus == "PICE" & Species == "SPP",
                   newSpeciesName := "spruce"]
      speciesTable[Genus == "PINU" & Species == "BAN",
                   newSpeciesName := "jack pine"]
      speciesTable[Genus == "PINU" & Species == "CON",
                   newSpeciesName := "lodgepole pine"]
      speciesTable[Genus == "POPU" & Species == "BAL",
                   newSpeciesName := "balsam poplar"]
      speciesTable[Genus == "POPU" & Species == "SPP",
                   newSpeciesName := "poplar"]
      speciesTable[Genus == "POPU" & Species == "TRE",
                   newSpeciesName := "trembling aspen"]
      speciesTable[Genus == "PRUN" & Species == "PEN",
                   newSpeciesName := "pin cherry"]
      speciesTable[Genus == "PRUN" & Species == "VIR",
                   newSpeciesName := "choke cherry"]
      speciesTable[Genus == "QUER" & Species == "MAC",
                   newSpeciesName := "bur oak"]
      speciesTable[Genus == "SALI" & Species == "PED",
                   newSpeciesName := "willow"] # not found assume willow
      speciesTable[Genus == "SALI" & Species == "PLA",
                   newSpeciesName := "willow"] # not found assume willow
      speciesTable[Genus == "SALI" & Species == "SCO",
                   newSpeciesName := "scouler willow"]
      speciesTable[Genus == "SALI" & Species == "SPP",
                   newSpeciesName := "willow"]
      speciesTable[Genus == "SALI" & Species == "SPP",
                   newSpeciesName := "willow"]
      speciesTable[Genus == "SHEP" & Species == "CAN",
                   newSpeciesName := "berry"] # not found assume berry
      speciesTable[Genus == "UNKN" & Species == "SPP",
                   newSpeciesName := "unknown"] #
    } else {
      stop("Please define the correct forestInventorySource among ")
    }
    speciesTable[is.na(newSpeciesName), newSpeciesName := "unknown"]
    return(speciesTable)
  })
