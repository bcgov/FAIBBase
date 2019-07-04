#' Group BEC zones into interior and coastal region
#' 
#' @description It groups the BC BEC zone into two regions: coastal region \code{C} and interior region \code{I}.
#'
#' @param BEC character, BC BEC zone(s)
#' 
#' 
#' @return grouped region by bec zone, in which \code{C} stands for coastal region, \code{I} stands for interior region and 
#'         \code{?} stands for unknown region. 
#'
#' 
#' @export
#' @docType methods
#' @rdname BEC2IC
#'
#' @author Yong Luo
#'
setGeneric("BEC2IC",
           function(BEC) {
             standardGeneric("BEC2IC")
           })

#' @rdname BEC2IC
setMethod(
  "BEC2IC",
  signature = c(BEC = "character"),
  definition = function(BEC){
    BEC_I_C <- rep("?", length(BEC))
    BEC_I_C[BEC %in% c("CDF", "CWH", "MH", "CMA")] <- "C"
    ## add IMA and BAFA to I based on Rene's suggestion
    # Both ‘IMA’ and ‘BAFA’ were previously classes in the ‘AT’ (alpine tundra) BEC zone, as part of the BEC classification update.
    # So it makes sense to assign these missing values to ‘I’ (interior)
    
    BEC_I_C[BEC %in% c("AT" , "PP", "BWBS", "ESSF" , "ICH", "BG",
                       "IDF", "MS", "SBPS", "SBS"  , "SWB", "IMA", "BAFA")] <- "I"
    return(BEC_I_C)
  })