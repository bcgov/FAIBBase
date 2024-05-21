#' Calculate volume for trees
#'
#' @description This function is to produce a tree trunk profile (i.e., inside bark diameter (\code{DIB})).
#'              And summarize the whole stem volume (\code{VOL_WSV}) and merchantable volume (\code{VOL_MER}).
#'
#' @param taperEquationForm character, Specifies which taper equaiton will be used, currently support KFIZ3 or KBEC.
#'        See function \code{DIB_ICalculator} for details. Default is KBEC, if missing.
#'
#' @param FIZorBEC character, Specifies which FIZ or BEC (depends on taperEquation) zones the tree located in BC.
#'
#' @param species character, Tree species, must be BC species code.
#'
#' @param height numeric, Total tree height in meter.
#'
#' @param DBH numeric, DBH of the tree in cm.
#'
#' @param stumpHeight numeric, Defines stump height. If missing, 0.3 m is used.
#'
#' @param breastHeight numeric, Defines the breast height. If missing, 1.3 m is used.
#'
#' @param UTOPDIB numeric, Merchantable inside-bark diameter. If missing, UTOP is 10.
#'
#' @param BTOPHeight numeric, Height at broken top.
#'
#' @return A volume table
#' @note For the volume between 0 and 0.3, also known as stump volume,
#'       the compiler calculates the volume as cylinder with the diameter of stump
#'        height. In the case of the diameter at stump height is less than diameter at breast height,
#'        the diameter at breast height is used as stump height. It calculates tree volume based on a 10 cm
#'        slices starting from 0.3 m tall using Smalian’s formula.
#' @examples
#' \dontrun{
#' treeprofile_a <- treeProfile(FIZorBEC = "CWH",
#'                              species = "H",
#'                              height = 27.4,
#'                              DBH = 30.7,
#'                              BTOPHeight = 5.6)
#' treeprofile_b <- treeProfile(FIZorBEC = "CWH",
#'                              species = "S",
#'                              height = 37.3,
#'                              DBH = 42.3)
#' treeprofile_c <- treeProfile(FIZorBEC = "CWH",
#'                              species = "H",
#'                              height = 11.6,
#'                              DBH = 11.2)
#'
#' }
#'
#' @importFrom data.table ':=' set setnames data.table shift
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#' @author Yong Luo
#'
#' @seealso \code{\link{treeVolCalculator}}
#' @export
#' @docType methods
#' @rdname treeProfile
treeProfile <- function(taperEquationForm = "KBEC",
                        FIZorBEC, species, height, DBH,
                        stumpHeight = 0.3, breastHeight = 1.3,
                        UTOPDIB = 10, BTOPHeight = NA){
  vcons <- pi*(1/(2*100)^2)
  ## collect invalid data
  ## tree height checking
  if(height %>>% 90 | height %<=%1.4){
    stop("Invalid height (>90 or <1.4).")
  }
  ## DBH checking
  if(DBH %>>% 1000 | DBH %<=% 1){
    stop("Invalid DBH (>1000 or <1).")
  }
  ## height at broken top checking
  if(!is.na(BTOPHeight)){
    if(BTOPHeight %>>% height){
      warning("Height at broken top was taller than tree height. Assumping no break top.")
      BTOPHeight <- NA
    }
  }
  DIB_stump <- DIB_ICalculator(taperEquationForm,
                               FIZorBEC = FIZorBEC,
                               species = species,
                               height_I = stumpHeight,
                               heightTotal = height,
                               DBH = DBH,
                               volMultiplier = 1)
  DIB_BH <- DIB_ICalculator(taperEquationForm,
                            FIZorBEC = FIZorBEC,
                            species = species,
                            height_I = breastHeight,
                            heightTotal = height,
                            DBH = DBH,
                            volMultiplier = 1)
  if(DIB_BH > DIB_stump){
    DIB_stump <- DIB_BH}
  treevolsmry <- data.table(Name = "VOL_STUMP",
                            Value = vcons * (stumpHeight) * (DIB_stump^2))
  treeprofiledata <- data.table(HT_I = seq(stumpHeight,
                                           height, by = 0.1))
  treeprofiledata$DIB_I <- DIB_ICalculator(taperEquationForm,
                                           FIZorBEC = FIZorBEC,
                                           species = species,
                                           height_I = treeprofiledata$HT_I,
                                           heightTotal = height,
                                           DBH = DBH,
                                           volMultiplier = 1)
  treeprofiledata[HT_I < breastHeight & DIB_I < DIB_BH,
                  DIB_I := DIB_BH]
  treeprofiledata[, ':='(HT_I_next = shift(HT_I, type = "lead"),
                         DIB_I_next = shift(DIB_I, type = "lead"))]
  treeprofiledata[, VOL_I := vcons * (0.1) * (DIB_I_next^2 + DIB_I^2)/2]
  treeprofiledata <- rbind(data.table(HT_I = 0,
                                      DIB_I = 0,
                                      HT_I_next = stumpHeight,
                                      DIB_I_next = DIB_stump,
                                      VOL_I = vcons * (stumpHeight) * (DIB_stump^2)),
                           treeprofiledata)
  treeprofiledata[HT_I %==% stumpHeight,
                  Comment := "stump height"]
  treeprofiledata[HT_I %==% breastHeight,
                  Comment := "breast height"]
  merchant <- max(treeprofiledata[DIB_I_next %>=% UTOPDIB]$HT_I)
  treeprofiledata[HT_I %==% merchant,
                  Comment := "max merchantable height"]
  if(!is.na(BTOPHeight)){
    # treat non broken top trees
    treeprofiledata[HT_I %==% (BTOPHeight-0.1),
                    Comment := "break height"]
    treevolsmry <- rbind(treevolsmry,
                         treeprofiledata[HT_I_next %<<% BTOPHeight,
                                         .(Name = "VOL_WSV",
                                           Value = sum(VOL_I))],
                         treeprofiledata[HT_I %>=% stumpHeight &
                                           DIB_I_next %>=% UTOPDIB &
                                           HT_I_next %<<% BTOPHeight,
                                         .(Name = "VOL_MER",
                                           Value = sum(VOL_I))])
  } else {
    treevolsmry <- rbind(treevolsmry,
                         treeprofiledata[,.(Name = "VOL_WSV",
                                            Value = sum(VOL_I, na.rm = TRUE))],
                         treeprofiledata[HT_I %>=% stumpHeight &
                                           DIB_I_next %>=% UTOPDIB,
                                         .(Name = "VOL_MER",
                                           Value = sum(VOL_I, na.rm = TRUE))])
  }
  return(list(tree_profile = treeprofiledata,
              volume_summary = treevolsmry))
}
