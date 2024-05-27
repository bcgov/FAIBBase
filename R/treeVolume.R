#' Calculate volume for trees
#'
#' @description This function is to produce a tree trunk profile (i.e., inside bark diameter (\code{DIB})).
#'              And summarize the whole stem volume (\code{VOL_WSV}) and merchantable volume (\code{VOL_MER}).
#'
#' @param taperEquationForm character, Specifies which taper equations will be used, currently support \code{KBEC} or \code{KFIZ3}.
#'                                     \code{KBEC} is the Kozak's equations (2002 version) based on BEC zone, tree sizes and species.
#'                                     \code{KFIZ3} is the equations based on forest inventory zone (FIZ), tree sizes and species.
#'                                     Default is KBEC, if missing.
#'
#' @param FIZorBEC character, Specifies which FIZ or BEC (depends on \code{taperEquationForm}) zones the tree located in BC.
#'
#' @param species character, Tree species, must be BC species code.
#'
#' @param DBH numeric, DBH of the tree in cm.
#' @param height numeric, Total tree height in meter.
#' @param BTOPHeight numeric, Height at broken top in meter. \code{NA} suggests no broken top.
#'                            If missing, the default is \code{NA}.
#'
#' @param volumeName character, Indicates which volume you want to derive. It supports the
#'                              whole stem volume (\code{WSV}), merchantable volume (\code{MER})
#'                              or stump volume (\code{STUMP}).
#'                              The merchantable volume is the whole stem volume minus volume of
#'                              stump and volume less than minimum utility diameter (\code{UTOPDIB}).
#'                              If missing, the default is \code{WSV}.
#' @param stumpHeight numeric, Defines stump height. If missing, 0.3 m is used. It will be called to
#'                             calculate the merchantable volume.
#'
#' @param UTOPDIB numeric, Merchantable inside-bark diameter. If missing, UTOP is 10. It will be called to
#'                             calculate the merchantable volume.
#'
#'
#' @return volume
#' @note For the volume between 0 and 0.3, also known as stump volume,
#'       the function calculates the volume as cylinder with the diameter of stump
#'        height. In the case of the diameter at stump height is less than diameter at breast height,
#'        the diameter at breast height is used as stump height. It calculates tree volume based on a 10 cm
#'        slices starting from 0.3 m tall using Smalian’s formula.
#' @examples
#' \dontrun{
#' treeA_wsv <- treeVolume(FIZorBEC = "CWH",
#'                         species = "H",
#'                         DBH = 30.7,
#'                         height = 27.4,
#'                         BTOPHeight = 5.6)
#' trees_wsv <- treeVolume(FIZorBEC = "CWH",
#'                         species = c("H", "S", "H"),
#'                         DBH = c(30.7, 42.3, 11.2),
#'                         height = c(27.4, 37.3, 11.6),
#'                         BTOPHeight = c(5.6, NA, NA))
#' trees_mer <- treeVolume(FIZorBEC = "CWH",
#'                         volumeName = "MER",
#'                         species = c("H", "S", "H"),
#'                         DBH = c(30.7, 42.3, 11.2),
#'                         height = c(27.4, 37.3, 11.6),
#'                         BTOPHeight = c(5.6, NA, NA))
#' }
#'
#' @importFrom data.table ':=' set setnames data.table shift
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @author Yong Luo
#'
#' @seealso \code{\link{treeVolCalculator}} and \code{\link{treeProfile}}
#' @export
#' @docType methods
#' @rdname treeVolume
treeVolume <- function(taperEquationForm = "KBEC",
                       FIZorBEC, species, DBH,
                       height, BTOPHeight = NA,
                       volumeName = "WSV",
                       stumpHeight = 0.3,
                       UTOPDIB = 10){
  processtable <- data.table(FIZorBEC = FIZorBEC,
                             species = species,
                             DBH = DBH,
                             height = height,
                             BTOPHeight = BTOPHeight)
  volume_out <- NULL
  for (indirow in 1:nrow(processtable)) {
    tree_profile <- treeProfile(taperEquationForm = taperEquationForm,
                                FIZorBEC = processtable$FIZorBEC[indirow],
                                species = processtable$species[indirow],
                                height = processtable$height[indirow],
                                DBH = processtable$DBH[indirow],
                                stumpHeight = stumpHeight,
                                UTOPDIB = UTOPDIB,
                                BTOPHeight = processtable$BTOPHeight[indirow])
    if(volumeName == "WSV"){
      volume_out <- c(volume_out, tree_profile$volume_summary[Name == "VOL_WSV"]$Value)
    } else if (volumeName == "MER"){
      volume_out <- c(volume_out, tree_profile$volume_summary[Name == "VOL_MER"]$Value)
    } else if (volumeName == "STUMP"){
      volume_out <- c(volume_out, tree_profile$volume_summary[Name == "VOL_STUMP"]$Value)
    } else {
      stop("volumeName is not correctly specified.")
    }
  }
  return(volume_out)
}
