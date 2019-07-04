#' Calculate the inside-bark diameter at a given height
#'
#' @description This function uses taper equation to calculate diameter inside bark at a given height.
#'              It is equivalent to the subroutine of vol_tree_active_equation in vol_setup macro
#'
#' @param taperEquationForm character, Specifies a taper equation form one of KBEC, KBECQCI, KFIZ3.
#'
#' @param FIZorBEC character, Specifies FIZ or BEC.
#' @param species character, Species code.
#' @param height_I numeric, Height from ground.
#' @param heightTotal numeric, Total height of a tree.
#' @param DBH numeric, Diameter at breast height.
#' @param volMultiplier numeric, Volume adjustment multiplier.
#'
#'
#'
#'
#' @return Diameter inside bark
#'
#'
#' @importFrom data.table ':=' setnames
#'
#'
#' @export
#' @docType methods
#' @rdname DIB_ICalculator
#'
#' @author Yong Luo
#'
setGeneric("DIB_ICalculator",
           function(taperEquationForm, FIZorBEC,
                    species, height_I, heightTotal,
                    DBH, volMultiplier) {
             standardGeneric("DIB_ICalculator")
           })

#' @rdname DIB_ICalculator
setMethod(
  "DIB_ICalculator",
  signature = c(taperEquationForm = "character",
                FIZorBEC = "character",
                species = "character",
                height_I = "numeric",
                heightTotal = "numeric",
                DBH = "numeric",
                volMultiplier = "numeric"),
  definition = function(taperEquationForm, FIZorBEC,
                        species, height_I, heightTotal,
                        DBH, volMultiplier){
    ## handle excess of limits
    indiTreeData <- data.table(uniObsID = 1:max(length(height_I), length(heightTotal), length(DBH)),
                               FIZorBEC, SP0 = species,
                               HT_I = height_I,
                               HT = heightTotal, DBH,
                               VOL_MULT = volMultiplier)
    if(taperEquationForm == "KFIZ3"){
      setnames(indiTreeData, "FIZorBEC", "FIZ")
    } else {
      setnames(indiTreeData, "FIZorBEC", "BEC")
    }
    ## for invalid tree height, assign DIB_I with NA
    outputData <- indiTreeData[HT %in% c(NA, 0), ][, DIB_I := as.numeric(NA)]
    if(nrow(indiTreeData[HT %in% c(NA, 0), ]) > 0){
      warning("NA is assigned to DIB_I, as total tree height is not available.")
    }
    indiTreeData <- indiTreeData[!(HT %in% c(NA, 0)),]

    ## for invalid tree DBH, assign DIB_I with NA
    outputData <- rbind(outputData,
                        indiTreeData[DBH %in% c(NA, 0), ][, DIB_I := as.numeric(NA)])
    if(nrow(indiTreeData[DBH %in% c(NA, 0), ]) > 0){
      warning("NA is assigned to DIB_I, as DBH is not available.")
    }
    indiTreeData <- indiTreeData[!(DBH %in% c(NA, 0)),]


    # downsize as much as possible
    # clean up later
    outputData <- rbind(outputData,
                        indiTreeData[abs(HT_I - HT) %<<% 0.1,][, DIB_I := 0])

    indiTreeData <- indiTreeData[abs(HT_I - HT) %>=% 0.1,]

    ## need to remove rows that contain extreme values
    # for invalid max height increment, assign DIB_I with NA
    outputData <- rbind(outputData,
                        indiTreeData[HT_I %>>% (HT+0.1), ][, DIB_I := as.numeric(NA)])

    if(nrow(indiTreeData[HT_I %>>% (HT+0.1), ]) > 0){
      warning("NA is assigned to DIB_I, as heigh_I is bigger than heighTotal")
    }
    indiTreeData <- indiTreeData[HT_I %<=% (HT+0.1),]

    ## for invalid min height increment, assign DIB_I with NA
    # outputData <- rbind(outputData,
    #                     indiTreeData[HT_I %<<% 0.1, ][, DIB_I := as.numeric(NA)])
    # if(nrow(indiTreeData[HT_I < 0.1, ]) > 0){
    #   warning("NA is assigned to DIB_I, as heigh_I is too small, i.e., less than 0.1m.")
    # }
    # indiTreeData <- indiTreeData[HT_I %>=% 0.1,]

    ## to generate coefficients for specific taper equation
    taperCoeffs <- taperCoeffsGenerator(taperEquationForm = taperEquationForm)
    if(nrow(indiTreeData) > 0){
    outputData <- rbind(outputData,
                        indiTreeData[, DIB_I := taperImplementor(taperEquationForm = taperEquationForm,
                                                                 taperCoeffs = taperCoeffs,
                                                                 FIZorBEC = BEC,
                                                                 species = SP0,
                                                                 height_I = HT_I,
                                                                 heightTotal = HT,
                                                                 DBH = DBH,
                                                                 volMultiplier = VOL_MULT)])
    }

    DIB_I <- outputData[order(uniObsID),]$DIB_I
    return(DIB_I)
  })


#' Implement taper equation for a given tree
#'
#'
#' @param taperEquationForm character, Specifies a taper equation form one of KBEC, KBECQCI, KFIZ3.
#' @param taperCoeffs data.table, Table that stores the coefficients that match the taper equation.
#' @param FIZorBEC character, FIZ or BEC.
#' @param species character, Species code.
#' @param height_I numeric, Height from ground.
#' @param heightTotal numeric, Total height of a tree.
#' @param DBH numeric, Diameter at breast height.
#' @param volMultiplier, Volume multiplier adjustment.
#'
#' @return DIB_I diameter inside bark at height_I
#'
#' @importFrom data.table ':=' setnames setkey
#'
#' @note This function is inside of the VRIVolTree function
#'
#'
#' @docType methods
#' @rdname taperImplementor
#'
#' @author Yong Luo
#'
setGeneric("taperImplementor",
           function(taperEquationForm, taperCoeffs, FIZorBEC,
                    species, height_I, heightTotal, DBH, volMultiplier) {
             standardGeneric("taperImplementor")
           })

#' @rdname taperImplementor
setMethod(
  "taperImplementor",
  signature = c(taperEquationForm = "character",
                taperCoeffs = "data.table",
                FIZorBEC = "character",
                species = "character",
                height_I = "numeric",
                heightTotal = "numeric",
                DBH = "numeric",
                volMultiplier = "numeric"),
  definition = function(taperEquationForm, taperCoeffs, FIZorBEC,
                        species, height_I, heightTotal, DBH,
                        volMultiplier){
    if(taperEquationForm == "KFIZ3"){
      tempTable <- data.table(obsIndex = 1:length(height_I),
                              FIZorBEC, SP0 = species,
                              HT_I = height_I,
                              HT = heightTotal, DBH)
      setnames(tempTable, "FIZorBEC", "FIZ")
      tempTable1 <- setkey(tempTable, FIZ, SP0)[setkey(taperCoeffs, FIZ, SP0),
                                                nomatch = 0]
      if(nrow(tempTable) != nrow(tempTable1)){
        print(unique(tempTable[!(obsIndex %in% tempTable1$obsIndex), .(FIZ, species = SP0)],
                     by = c("FIZ", "species")))
        stop("The above combinations of FIZ and species do not have taper equations.")
      } else {
        tempTable <- tempTable1
        rm(tempTable1)
        tempTable[, Z := HT_I/HT]
        tempTable[, X := (1-sqrt(Z))/(1-sqrt(P))]
        tempTable[, EX := A3 * (Z^2)+ A4*log(Z+.001)
                  + A5 * sqrt(X)
                  + A6 * (DBH/HT )
                  + A7 * exp(Z)]
        tempTable[, DIB_I := A0  * (DBH^A1)
                  * (A2^DBH)
                  * (X^EX)]
        DIB_I <- tempTable[order(obsIndex), ]$DIB_I
      }
    } else {
      tempTable <- data.table(obsIndex = 1:length(height_I),
                              FIZorBEC, SP0 = species,
                              HT_I = height_I,
                              HT = heightTotal, DBH,
                              VOL_MULT = volMultiplier)
      setnames(tempTable, "FIZorBEC", "BEC")
      tempTable1 <- setkey(tempTable, BEC, SP0)[setkey(taperCoeffs, BEC, SP0),
                                                nomatch = 0]
      if(nrow(tempTable) != nrow(tempTable1)){
        print(unique(tempTable[!(obsIndex %in% tempTable1$obsIndex), .(BEC, species = SP0)],
                     by = c("BEC", "species")))
        stop("The above combinations of BEC and species do not have taper equations.")
      } else {
        tempTable <- tempTable1
        rm(tempTable1)
        if(taperEquationForm %in% c("KBEC", "KBECQCI")){ # for 2002 equations
          tempTable[, ':='(F1 = B1 * (DBH^B2),
                           F2 = HT^B3,
                           ZI = HT_I/HT,
                           DH = DBH/HT,
                           P = 1.3/HT)]
          tempTable[, ':='(QI = 1-(ZI^(1/3)))]
          tempTable[, XI := QI/(1-P^(1/3))]
          tempTable[, ':='(EX1 = B4 * (ZI^4),
                           EX2 = B5/exp(DH),
                           EX3 = B6 * (XI^0.1),
                           EX4 = B7/DBH,
                           EX5 = B8 * (HT^QI),
                           EX6 = B9 * XI)]
          tempTable[, EX := EX1 + EX2 + EX3 + EX4+
                      EX5 + EX6]
          tempTable[, DIB_I := ERR * F1 * F2 * (XI^EX)]

          ## current without vol_mult adjustment
          tempTable[, DIB_I := DIB_I * VOL_MULT]


          DIB_I <- tempTable[order(obsIndex), ]$DIB_I
        } else if (taperEquationForm %in% c("KBEC_OLD", "KBECQCI_OLD")){ # for old equations
          tempTable[, ':='(FF = B1 * (DBH^B2) * B3^DBH,
                           DH = DBH/HT,
                           XQ = HT_I/HT)]
          tempTable[, XL := 1 - sqrt(XQ)]
          tempTable[, XX := XL/0.9]
          tempTable[XX < 0, XX := 0]
          tempTable[, XL := asin(XL) ] # for arcsine XL
          tempTable[, XB := 1/(XQ + DH)]
          tempTable[, EX := B4
                    +  B5 * XQ^0.25
                    +  B6 * XQ^(1.0 / 3.0)
                    +  B7 * XQ^0.5
                    +  B8 * XL
                    +  B9 * XB
                    +  B10 * HT]
          tempTable[, DIB_I := ERR * FF * (XX^EX)]

          ## current without vol_mult adjustment
          tempTable[, DIB_I := DIB_I * VOL_MULT]


          DIB_I <- tempTable[order(obsIndex), ]$DIB_I

        }

      }
    }

    return(DIB_I)

  })





#### define taperCoeffsGenerator
#' Generate the coefficients table of taper equations
#'
#' @description  Generates the coefficients of the taper equations for based on specific
#'               taper equation form (\code{taperEquationForm})
#'
#'
#' @param taperEquationForm character, Specifies a taper equation form one of KBEC, KBECQCI, KFIZ3.
#'
#'
#'
#'
#' @return A coeffients table
#'
#' @importFrom data.table ':=' setkey data.table
#'
#'
#'
#' @docType methods
#' @rdname taperCoeffsGenerator
#'
#' @author Yong Luo
#'
setGeneric("taperCoeffsGenerator",
           function(taperEquationForm) {
             standardGeneric("taperCoeffsGenerator")
           })

#' @rdname taperCoeffsGenerator
setMethod(
  "taperCoeffsGenerator",
  signature = c(taperEquationForm = "character"),
  definition = function(taperEquationForm){
    if(taperEquationForm == "KFIZ3"){
      ## hardcoded for the FIZ head table
      headtable <- data.table(expand.grid(FIZ = c("A", "B", "C",
                                                  "D", "E", "F", "G", "H", "I", "J",
                                                  "K", "L"),
                                          SP0 = c("F", "C", "H", "B", # same order as original sas compiler
                                                  "S", "Y", "PW", "PL",
                                                  "PY", "L", "AC", "D",
                                                  "MB", "E", "AT", "PA")),
                              stringsAsFactors = FALSE)

      headtable[FIZ %in% c("A", "B", "C"), FIZ_EQN_NO := 1]
      headtable[FIZ %in% c("D", "E", "F", "G", "H", "I", "J"), FIZ_EQN_NO := 2]
      headtable[FIZ %in% c("K", "L"), FIZ_EQN_NO := 3]

      ### forget the species order in original sas codes, the following R codes directly join the coefficients
      ### into the table by fiz equation no and species codes
      ## know how the coefficients been orgnized in the coefficient table in the sas codes
      ## currently, the below table is hard coded, could be moved to other place make it more
      ## flexiable
      fizequ <- data.table(FIZ_EQN_NO = c(rep(1, 16),
                                          rep(2, 16),
                                          rep(3, 16)),
                           SP0 = rep(c("F", "C", "H", "B", # same order as original sas compiler
                                       "S", "Y", "PW", "PL",
                                       "PY", "L", "AC", "D",
                                       "MB", "E", "AT", "PA"),
                                     3),
                           EQN_NO = 1:48)

      headtable <- setkey(headtable, FIZ_EQN_NO, SP0)[setkey(fizequ, FIZ_EQN_NO, SP0),
                                                      nomatch = 0]

      TaperCoeffs <- data.table(EQN_NO = 1:48,
                                P = c(0.25, 0.25, 0.20, 0.25, # for the inflection
                                      0.25, 0.30, 0.25, 0.25,  # which is corresponding to array inflect in
                                      0.25, 0.30, 0.25, 0.30,  # original sas compiler
                                      0.25, 0.25, 0.20, 0.25,

                                      0.25, 0.30, 0.25, 0.30,
                                      0.30, 0.30, 0.25, 0.25,
                                      0.25, 0.30, 0.25, 0.30,
                                      0.25, 0.25, 0.20, 0.25,

                                      0.25, 0.30, 0.25, 0.30,
                                      0.30, 0.30, 0.25, 0.25,
                                      0.25, 0.30, 0.25, 0.30,
                                      0.25, 0.25, 0.20, 0.25),
                                # the following are the coefficients for fiz equations
                                # which is corresponding to array A_FIZ in sas code
                                A0 = c(1.012675,  1.218296,  0.830874,  0.988964,
                                       0.924126,  0.928138,  0.868943,  0.774601,
                                       0.856592,  0.746827,  0.802839,  0.719188,
                                       1.097880,  0.648830,  0.855966,  1.078961,

                                       0.920840,  1.033575,  0.752027,  1.008741,
                                       0.897311,  0.928138,  0.984019,  0.774601,
                                       0.856592,  0.746827,  0.802839,  0.719188,
                                       1.097880,  0.648830,  0.855966,  1.078961,

                                       0.920840,  1.033575,  0.752027,  0.764353,
                                       0.897595,  0.928138,  0.984019,  0.793793,
                                       0.856592,  1.164819,  0.852579,  0.719188,
                                       1.097880,  0.633306,  0.891641,  1.078961),

                                A1 = c(0.899136,  0.855983,  1.005210,  0.951803,
                                       0.950707,  0.945293,  0.976312,  1.040320,
                                       0.936402,  1.003900,  0.993776,  1.052190,
                                       0.840504,  1.121390,  0.987014,  0.894083,

                                       0.923867,  0.896971,  1.028970,  0.916357,
                                       0.957090,  0.945293,  0.941322,  1.040320,
                                       0.936402,  1.003900,  0.993776,  1.052190,
                                       0.840504,  1.121390,  0.987014,  0.894083,

                                       0.923867,  0.896971,  1.028970,  1.053220,
                                       0.957499,  0.945293,  0.941322,  1.049320,
                                       0.936402,  0.831995,  0.952969,  1.052190,
                                       0.840504,  1.110510,  0.957835,  0.894083),

                                A2 = c(1.000123,  0.999921,  0.999142,  0.999789,
                                       0.999518,  0.999206,  0.999773,  0.996984,
                                       1.002104,  0.997233,  0.998974,  0.997551,
                                       1.006569,  0.992077,  0.999828,  1.001749,

                                       1.000568,  0.999079,  0.998660,  1.001159,
                                       0.999370,  0.999206,  0.999700,  0.996984,
                                       1.002104,  0.997233,  0.998974,  0.997551,
                                       1.006569,  0.992077,  0.999828,  1.001749,

                                       1.000568,  0.999079,  0.998660,  0.994711,
                                       0.998952,  0.999206,  0.999700,  0.995709,
                                       1.002104,  1.003909,  1.000477,  0.997551,
                                       1.006569,  0.994733,  1.001450,  1.001749),

                                A3 = c(0.968978,  2.037620,  1.770670,  2.336270,
                                       1.750510,  0.301423,  1.676930,  0.745750,
                                       0.566217,  0.747048,  0.706093,  0.599235,
                                       0.981297,  0.865974,  0.424473,  1.377540,

                                       1.095560,  1.598260,  1.174800,  1.415990,
                                       1.532270,  0.301423,  1.571030,  0.745750,
                                       0.566217,  0.747048,  0.706093,  0.599235,
                                       0.981297,  0.865974,  0.424473,  1.377540,

                                       1.095560,  1.598260,  1.174800,  1.381630,
                                       1.110150,  0.301423,  1.571030,  0.583403,
                                       0.566217,  1.880250,  0.731911,  0.599235,
                                       0.981297,  1.021680,  0.695143,  1.377540),

                                A4 = c(-0.190913, -0.486492, -0.329190, -0.502311,
                                       -0.408021, -0.040792, -0.372195, -0.130177,
                                       -0.087141, -0.133729, -0.096789, -0.033036,
                                       -0.222619, -0.106757, -0.037553, -0.286807,

                                       -0.202191, -0.411541, -0.263576, -0.325671,
                                       -0.364679, -0.040792, -0.369344, -0.130177,
                                       -0.087141, -0.133729, -0.096789, -0.033036,
                                       -0.222619, -0.106757, -0.037553, -0.286807,

                                       -0.202191, -0.411541, -0.263576, -0.306534,
                                       -0.281544, -0.040792, -0.369344, -0.077654,
                                       -0.087141, -0.401856, -0.084192, -0.033036,
                                       -0.222619, -0.141481, -0.039652, -0.286807),

                                A5 = c(0.825961,  2.632080,  2.185610,  4.154490,
                                       2.659000, -1.235630,  2.567510,  0.558818,
                                       -0.063450,  0.397110,  0.312724, -0.261339,
                                       1.428220,  0.257139, -0.517540,  1.038780,

                                       0.967329,  2.402420,  2.233330,  2.793270,
                                       2.741210, -1.235630,  2.703200,  0.558818,
                                       -0.063450,  0.397110,  0.312724, -0.261339,
                                       1.428220,  0.257139, -0.517540,  1.038780,

                                       0.967329,  2.402420,  2.233330,  2.637080,
                                       2.125100, -1.235630,  2.703200, -0.036267,
                                       -0.063450,  3.082780,  0.196339, -0.261339,
                                       1.428220,  0.641499, -0.603404,  1.038780),

                                A6 = c(0.048766,  0.109094,  0.105050,  0.086560,
                                       0.092651,  0.030743,  0.071928,  0.198687,
                                       0.071720,  0.078345,  0.119634,  0.215536,
                                       0.228560,  0.254574,  0.102211,  0.072537,

                                       0.081696,  0.094283,  0.045184,  0.108427,
                                       0.117756,  0.030743,  0.049628,  0.198687,
                                       0.071720,  0.078345,  0.119634,  0.215536,
                                       0.228560,  0.254574,  0.102211,  0.072537,

                                       0.081696,  0.094283,  0.045184,  0.163996,
                                       0.148340,  0.030743,  0.049628,  0.142531,
                                       0.071720,  0.276300,  0.148285,  0.215536,
                                       0.228560,  0.258921,  0.193706,  0.072537),

                                A7 = c(-0.426214, -1.486550, -1.192970, -2.186240,
                                       -1.396760,  0.672879, -1.340830, -0.324178,
                                       0.051415, -0.183542, -0.080057,  0.123059,
                                       -0.654456, -0.149926,  0.303931, -0.647372,

                                       -0.514604, -1.252170, -1.002020, -1.326790,
                                       -1.362760,  0.672879, -1.334700, -0.324178,
                                       0.051415, -0.183542, -0.080057,  0.123059,
                                       -0.654456, -0.149926,  0.303931, -0.647372,

                                       -0.514604, -1.252170, -1.002020, -1.292080,
                                       -1.005560,  0.672879, -1.334700, -0.022523,
                                       0.051415, -1.661550, -0.069852,  0.123059,
                                       -0.654456, -0.359508,  0.209159, -0.647372))
      taperCoeffs <- setkey(headtable, EQN_NO)[setkey(TaperCoeffs, EQN_NO),
                                               nomatch = 0]
      taperCoeffs[, ':='(FIZ_EQN_NO = NULL, EQN_NO = NULL) ]
    } else {
      ## the following codes do not follow the original sas complier, for which it introduces some unnessessary index
      ## these codes will join the coefficients into tree data by BEC zone and species code
      ## produce the coefficient table
      ## for 16 species and 13 species, totally 208 combinations,
      ## however, 74 equations have been used, as some equatioins are same

      headtable <- data.table(BEC = rep(c("AT", "BWBS", "CDF", "CWH", "ESSF", "ICH",
                                          "IDF", "MH", "MS", "PP", "SBPS", "SBS", "SWB"), 16),
                              SP0 = unlist(lapply(c("AC", "AT", "B", "C", # 16 species following same order as original codes
                                                    "D", "E", "F", "H",
                                                    "L", "MB", "PA", "PL",
                                                    "PW", "PY", "S", "Y"), function(s) rep(s, 13))),

                              EQN_NO = c(1,1,2,2,1,3,4,2,4,4,4,5,1, # from the original matrix, each row represents a species and each column represents a bec zone
                                         6,6,7,7,8,7,8,6,6,8,9,9,6,
                                         10,11,12,12,13,14,15,16,17,15,17,18,19,
                                         21,21,20,20,21,22,23,20,24,23,23,23,21,
                                         25,25,25,25,25,25,25,25,25,25,25,25,25,
                                         26,26,27,27,26,27,28,26,29,28,29,29,26,
                                         31,31,30,30,31,32,33,30,34,35,36,36,31,
                                         39,39,37,37,38,39,40,41,38,40,38,42,39,
                                         43,43,45,45,44,45,46,44,47,46,47,47,43,
                                         48,48,48,48,48,48,48,48,48,48,48,48,48,
                                         49,49,49,49,49,49,49,49,49,49,49,49,49,
                                         52,50,51,51,52,53,54,52,55,54,56,57,58,
                                         60,60,59,59,60,60,61,60,61,61,61,61,60,
                                         63,62,62,62,62,62,63,62,63,64,63,62,63,
                                         67,65,66,66,67,68,69,67,70,69,71,72,73,
                                         74,74,74,74,74,74,74,74,74,74,74,74,74))
      ## the following prepare coefficients based on taper equation form (taperEquationForm)
      if(taperEquationForm == "KBEC"){ # using Kozak 2002 coefficients
        TaperCoeffs <- data.table(EQN_NO = 1:74,
                                  # B1-B10 is from _CBEC_2002
                                  B1 = c(0.697074,	0.980777,	0.470279,	0.832203,	0.855187,
                                         0.692238,	0.587918,   0.763041,	0.774001,	0.702051,
                                         0.946160,	0.888629,   0.891464,   0.912164,	0.963706,
                                         0.929690,	0.967745,	0.958944,	0.862635,   0.974922,
                                         1.013822,	1.001957,	0.854996,	0.888422,	0.917640,
                                         0.790946,	1.044910,	0.921736,	0.782832,	0.865189,
                                         0.846834,   0.824203,	0.784898,	0.791505,	0.874374,
                                         0.753585,	0.883473,   0.822588,	0.925665,	0.703487,
                                         1.096978,	0.918348,	0.750708,   1.133297,	0.856489,
                                         0.909755,	1.065324,	0.625287,	0.828758,   0.925039,
                                         0.936312,	0.866315,	0.924016,	0.823282,	0.869486,
                                         0.912291,	0.954651,	0.781048,	0.912068,	0.894384,
                                         0.675913,   0.693763,	0.587359,	0.678661,	0.792409,
                                         0.861077,	0.873000,   0.878697,	0.827157,	0.829601,
                                         0.856953,	0.843142,	0.832931,   1.036053),

                                  B2 = c(0.984191,	0.984880,	0.825508,	1.031080,	1.004580,
                                         1.025710,	0.990603,	0.987319,	0.977164,	0.930470,
                                         0.970066,	1.018170,	0.970117,	0.987020,	1.002770,
                                         1.007040,	1.000920,	0.998840,	0.983681,	0.947060,
                                         0.986204,	0.970817,	0.953731,	1.020540,	1.043920,
                                         1.059030,	1.085390,	1.007230,	1.060500,	0.918710,
                                         1.007570,	0.958215,	0.922923,	0.899732,	0.943590,
                                         0.930390,	0.977059,	1.028930,	0.996021,	0.940797,
                                         0.954388,	1.012930,	1.004220,	0.920441,	1.000540,
                                         0.978524,	0.994625,	0.998921,	1.045750,	1.041100,
                                         1.004120,	1.008480,	1.008580,	1.008530,	0.981791,
                                         1.034900,	1.039110,	1.065120,	1.015880,	0.946690,
                                         0.988752,	1.063850,	1.006390,	1.027590,	0.989184,
                                         0.974794,	0.986406,	0.982530,	1.046070,	0.972121,
                                         0.980573,	1.008130,	0.998840,	0.945233),

                                  B3 = c(0.092048,	0.000000,	0.408597,	0.000000,	0.000000,
                                         0.065998,	0.167431,	0.088551,	0.078353,	0.169054,
                                         0.036379,	0.000000,	0.057080,	0.028816,	0.000000,
                                         0.000000,	0.000000,	0.000000,	0.051593,	0.058082,
                                         0.000000,	0.017320,	0.089223,	0.000000,	-0.030260,
                                         0.000000,	-0.117568,	0.000000,	0.000000,	0.083304,
                                         0.000000,	0.064930,	0.114861,	0.138371,	0.052367,
                                         0.118792,	0.040378,	0.000000,	0.000000,	0.161374,
                                         0.000000,	0.000000,	0.079550,	0.000000,	0.000000,
                                         0.000000,	-0.067298,	0.154001,	0.000000,	-0.031225,
                                         0.000000,	0.022381,	0.000000,	0.037550,	0.045308,
                                         -0.026858,	-0.040248,	0.000000,	0.000000,	0.078050,
                                         0.113469,	0.000000,	0.127014,	0.055666,	0.075818,
                                         0.062424,	0.048413,	0.047642,	0.000000,	0.079652,
                                         0.057827,	0.031402,	0.045616,	0.041717),

                                  B4 = c(0.571823,	0.425990,	0.518996,	0.555893,	0.616982, # B1
                                         0.617946,	0.561594,	0.567474,	0.562123,	0.368305,
                                         0.267499,	0.281762,	0.329031,	0.280213,	0.316520,
                                         0.322973,	0.229936,	0.338360,	0.300719,	0.422469,
                                         0.365871,	0.350235,	0.509025,	0.535892,	0.551160,
                                         0.535476,	0.519758,	0.585822,	0.531431,	0.408556,
                                         0.439966,	0.466737,	0.496895,	0.467790,	0.464744,
                                         0.492167,	0.371598,	0.223070,	0.336874,	0.445750,
                                         0.292216,	0.265524,	0.391716,	0.406280,	0.416442,
                                         0.458164,	0.409168,	0.496851,	0.575368,	0.400319,
                                         0.323048,	0.341563,	0.382689,	0.381356,	0.414707,
                                         0.145265,	0.335144,	0.406749,	0.401949,	0.285474,
                                         0.329021,	0.541075,	0.520076,	0.567757,	0.318595,
                                         0.365018,	0.358853,	0.349046,	0.346671,	0.365467,
                                         0.260685,	0.319773,	0.261189,	0.513777),

                                  B5 = c(-0.532732,	-0.408586,	 0.000000,	-0.819495,	-0.600928, #B2
                                         -0.732773,	 0.000000,	-0.445677,	-0.406941,	-0.871972,
                                         -0.758032,	-0.513748,	-0.535456,	-0.430046,	-0.855607,
                                         -0.893384,	-0.632784,	-0.673099,	-0.609452,	-0.754983,
                                         -0.933513,	-0.922568,	-0.154652,	-0.929744,	-1.087550,
                                         -1.477960,	-1.631900,	-0.311250,	-1.393960,	-0.255448,
                                         -0.857115,	-0.728184,	-0.398502,	-0.596136,	-0.837250,
                                         -0.483977,	-0.555001,	-0.506310,	-0.673516,	-0.614371,
                                         -0.828280,	-0.651588,	-0.779147,	-0.379927,	-0.606263,
                                         -0.624715,	-0.562042,	-0.557429,	-0.755372,	-0.628840,
                                         -0.941452,	-0.975034,	-0.580517,	-0.637805,	-0.786627,
                                         -0.642841,	-0.625804,	 0.000000,	-0.619283,	-0.262013,
                                         -0.622173,	-0.920213,	-0.510024,	-0.810195,	-0.497244,
                                         -0.684348,	-0.577954,	-0.584707,	-0.553554,	-0.617612,
                                         -0.469767,	-0.620014,	-0.503513,	-0.694438),

                                  B6 = c(0.585361,	0.599333,	0.586112,	0.684864,	0.506335, # /  B3 /
                                         0.511722,	0.422530,	0.573964,	0.429284,	0.744146,
                                         0.658246,	0.540385,	0.605273,	0.615161,	0.716103,
                                         0.614989,	0.736691,	0.646087,	0.701826,	0.567453,
                                         0.600301,	0.582394,	0.498134,	0.490758,	0.632185,
                                         0.764237,	0.752113,	0.518927,	0.823577,	0.381950,
                                         0.495484,	0.468689,	0.407022,	0.426090,	0.447888,
                                         0.376916,	0.534113,	0.609010,	0.519966,	0.684340,
                                         0.564943,	0.776771,	0.742984,	0.346425,	0.493724,
                                         0.475407,	0.428397,	0.807706,	0.315451,	0.455121,
                                         0.608207,	0.577124,	0.483719,	0.442884,	0.492388,
                                         0.582900,	0.527018,	0.366657,	0.435591,	0.518040,
                                         0.538638,	0.379487,	0.423550,	0.342971,	0.650584,
                                         0.592759,	0.590280,	0.583692,	0.626023,	0.566992,
                                         0.594807,	0.607053,	0.738097,	0.357880),

                                  B7 = c(1.031500,	0.000000,	-1.139980,	2.764040,	0.000000, # / B4/
                                         1.581340,  -1.042440,	0.604079,	0.932614,	0.000000,
                                         2.158570,	0.961118,	1.351210,	1.337750,	1.520660,
                                         1.957730,	2.241810,	1.313000,	0.000000,	0.605054,
                                         2.154230,	1.924880,	-0.954664,	1.802300,	3.073710,
                                         5.437050,	6.194730,	0.739471,	4.189560,	0.279214,
                                         2.657920,	2.374190,	1.239400,	1.298160,	3.543260,
                                         1.156240,	2.501250,	2.750420,	4.268100,	2.192590,
                                         5.902140,	0.000000,	0.000000,	1.620100,	1.904660,
                                         1.862690,	1.677760,	0.000000,	1.794500,	0.894543,
                                         2.169960,	2.121340,	1.025690,	1.968660,	1.210020,
                                         0.000000,	1.076270,	-2.012740,	2.539620,	0.641732,
                                         1.616470,	2.702480,	0.702013,	2.541040,	0.000000,
                                         2.055230,	1.065000,	1.085990,	0.660229,	1.672490,
                                         1.803020,	1.403530,	-0.491759,	3.264120),

                                  B8 = c(0.018622,	0.027804,	0.000000,	0.036282,	0.079865, # / B5	 /
                                         0.040625,	0.000000,	0.000000,	0.036028,	0.091529,
                                         0.090207,	0.071218,	0.037795,	0.070896,	0.057347,
                                         0.056401,	0.070556,	0.054767,	0.065195,	0.094522,
                                         0.130682,	0.113347,	0.086719,	0.110408,	0.057013,
                                         0.084584,	0.099053,	0.075925,	0.102576,	0.043055,
                                         0.059184,	0.057825,	0.043678,	0.075816,	0.042725,
                                         0.040249,	0.067055,	0.067192,	0.072168,	0.000000,
                                         0.107168,	-0.019603,	0.091586,	0.071055,	0.045593,
                                         0.055198,	0.061444,	0.000000,	0.075022,	0.058064,
                                         0.060198,	0.067085,	0.058644,	0.058989,	0.065964,
                                         0.070425,	0.050757,	0.055123,	0.063571,	0.084762,
                                         0.088421,	0.031049,	0.000000,	0.019376,	0.050670,
                                         0.066461,	0.058842,	0.078816,	0.077040,	0.069729,
                                         0.060404,	0.067863,	0.035516,	0.093162),

                                  B9 = c(-0.225146,	-0.401537,	0.000000,	-0.491508,	-0.531571, # /  b6	 /
                                         -0.209009,	0.000000,	-0.182052,	-0.169494,	-0.743705,
                                         -0.749478,	-0.654727,	-0.360031,	-0.639182,	-0.480415,
                                         -0.552446,	-0.720140,	-0.519713,	-0.556178,	-0.281363,
                                         -0.556858,	-0.383764,	-0.189895,	-0.275766,	-0.375225,
                                         -0.559917,	-0.637429,	-0.408469,	-0.711948,	-0.204499,
                                         -0.232023,	-0.271688,	-0.222667,	-0.270822,	-0.318190,
                                         -0.057124,	-0.628608,	-0.793186,	-0.692041,	-0.316217,
                                         -1.014470,	-0.194663,	-0.466741,	-0.247628,	-0.289813,
                                         -0.304118,	-0.283161,	-0.109494,	0.000000,	-0.258598,
                                         -0.422735,	-0.355627,	-0.292710,	-0.350828,	-0.332804,
                                         -0.524672,	-0.297841,	-0.271375,	-0.316662,	-0.643002,
                                         -0.612866,	0.000000,	0.106217,	0.000000,	-0.351825,
                                         -0.409440,	-0.329744,	-0.463912,	-0.504864,	-0.385494,
                                         -0.442514,	-0.438919,	-0.356541,	-0.224877),

                                  ## error term here, _ERB_2002 in original sas codes
                                  ERR = c(1.004456,   1.007678,   1.009519,   1.006134,
                                          1.008825,   1.004719,   1.003938,   1.007869,   1.005168,
                                          1.002143,   1.003150,   1.005004,   1.005722,   1.005777,
                                          1.005358,   1.003793,   1.005464,   1.005911,   1.003682,
                                          1.008718,   1.006380,   1.007075,   1.007603,   1.007159,
                                          1.007517,   1.007491,   1.005790,   1.009798,   1.010023,
                                          1.006131,   1.005544,   1.005038,   1.006373,   1.004945,
                                          1.006178,   1.004648,   1.009289,   1.004987,   1.007584,
                                          1.009550,   1.007599,   1.002855,   1.003156,   1.004631,
                                          1.005360,   1.006390,   1.006045,   1.007925,   1.004216,
                                          1.002821,   1.006966,   1.004492,   1.004334,   1.004274,
                                          1.004354,   1.002498,   1.004025,   1.003508,   1.004830,
                                          1.003882,   1.003722,   1.005371,   1.006804,   1.009481,
                                          1.003595,   1.006564,   1.004711,   1.004692,   1.004600,
                                          1.004587,   1.002349,   1.004949,   1.002927,   1.007684))
      } else if (taperEquationForm == "KBEC_OLD") { # using the older kozak taper equation model, it has 10 coeffs
        TaperCoeffs <- data.table(EQN_NO = 1:74,
                                  # B0-B9 from _CBEC array in original sas codes
                                  B1 = c(0.926472,   1.101792,    .926472,    .672378, # /A0 /
                                         0.866667,    .984285,   1.135583,   1.173494,   1.144134,
                                         0.842384,    .704739,   1.192696,   1.582557,   1.372364,
                                         1.583827,   1.185559,   1.609732,   1.226561,    .865989,
                                         1.843373,   2.332067,   1.735756,   2.006167,   2.496071,
                                         0.990422,    .992562,    .649193,   1.115784,    .340047,
                                         1.207477,   1.253020,   1.361797,   1.312899,   1.400177,
                                         1.217588,   1.117082,   1.072655,   1.109871,    .987946,
                                         1.054211,    .792637,   1.073364,   1.579174,    .986780,
                                         1.082503,   1.112471,   1.032756,   1.106740,   1.675228,
                                         0.974504,   1.222762,   1.112247,   1.225558,   1.134229,
                                         1.316970,    .841285,   1.011506,   1.461466,   1.341984,
                                         1.294196,   1.352042,   1.048176,   1.300887,   1.344632,
                                         1.146634,   1.178316,   1.517568,   1.356492,   1.372405,
                                         1.384212,   1.234876,   1.231969,   1.335956,   1.899499),

                                  B2 = c(1.023400,    .969435,   1.023400,   1.149360, # / A1 /
                                         1.049010,   1.026200,    .995001,    .985894,    .993345,
                                         1.123420,   1.208190,    .981241,    .892999,    .946051,
                                         0.892642,    .983256,    .870578,    .978191,   1.111810,
                                         0.911766,    .844348,    .940212,    .868523,    .821778,
                                         1.068810,   1.026020,   1.239080,   1.023700,   1.525200,
                                         0.945081,    .956041,    .921515,    .926191,    .934137,
                                         0.942922,    .986962,   1.015510,    .967993,   1.042700,
                                         0.997259,   1.146680,   1.015280,    .881474,   1.027500,
                                         0.978596,    .995964,   1.005460,    .995549,    .912974,
                                         1.077140,    .986634,   1.033920,    .983522,   1.021980,
                                         0.933305,   1.054190,   1.055710,    .874587,    .963313,
                                         0.976095,    .955550,    .986993,    .911281,    .922500,
                                         1.015090,   1.012020,    .938039,    .980958,    .969735,
                                         0.977568,    .997534,    .998729,    .922608,    .897550),

                                  B3 = c(0.999683,   1.000429,    .999683,    .997229, # / A2 /
                                         0.999110,   1.000662,    .999269,    .999232,    .999225,
                                         0.996431,    .992804,    .999920,   1.001111,    .999789,
                                         1.001128,    .999864,   1.002709,    .998981,    .996946,
                                         0.999877,   1.000894,    .999218,   1.001385,   1.001576,
                                         0.996624,   1.004024,    .993890,    .998966,    .982712,
                                         0.999882,    .999780,   1.000689,   1.000310,    .999642,
                                         0.999602,    .999152,    .998825,   1.001018,    .997706,
                                         0.999812,    .995253,    .998820,   1.006172,    .997511,
                                         0.999494,    .997311,    .998124,   1.003166,   1.001646,
                                         0.996946,    .998821,    .997325,    .999177,    .996523,
                                         1.001822,   1.000479,    .996782,   1.006517,    .999675,
                                         0.999388,   1.000253,   1.000590,   1.002578,   1.001281,
                                         0.999170,    .999175,    .999824,    .998925,    .999491,
                                         0.998811,    .999003,    .998974,   1.003277,    .999514),

                                  B4 = c(121.365000, -47.728900, 121.365000,  73.711200, # / A3 /
                                         121.991000, 244.333000, 252.892000, 251.359000, 257.022000,
                                         57.109400,  29.736900,  61.717800,  25.666500,  14.968800,
                                         24.158500,  61.047100, -51.683300,  16.972100,  60.571100,
                                         72.650900,  44.702800,  28.652900,  53.192400, 201.623000,
                                         146.127000, 151.666000, 174.433000, 163.160000, 131.079000,
                                         87.286900, 100.448000, 125.413000, 114.078000, 132.142000,
                                         112.689000, 168.309000,  81.647200, -71.755900, 101.902000,
                                         -22.873000,  89.070600,  83.363300,  25.673500,   7.225950,
                                         4.206740,  95.228000,   8.792230,   4.073990,  78.621200,
                                         169.168000, 138.851000, 120.901000, 146.438000, 153.783000,
                                         128.699000,-156.170000,  93.935600, 162.475000,  27.513400,
                                         17.939300,  39.495800, 143.388000,  63.486100,  86.179000,
                                         50.551900,  47.985600,  69.195100,  53.450600,  85.176500,
                                         88.050800,  62.893100,  64.890800,  30.638600, 137.737000),

                                  B5 = c(-272.245000, 198.234000,-272.245000,-133.272000, # / A4 /
                                         -272.272000,-634.980000,-692.263000,-691.926000,-704.738000,
                                         -187.239000,-113.372000,-219.227000,-101.253000, -79.652700,
                                         -96.925900,-215.498000, 110.779000, -48.983200,-203.473000,
                                         -194.055000,-149.679000, -91.178600,-138.478000,-579.877000,
                                         -354.998000,-393.699000,-466.130000,-425.603000,-321.202000,
                                         -211.746000,-248.130000,-325.419000,-299.577000,-355.819000,
                                         -291.096000,-425.359000,-238.305000, 165.702000,-295.854000,
                                         36.329500,-268.816000,-242.180000, -82.210600,  53.928400,
                                         38.103800,-241.225000,  49.323100,  37.370200,-147.295000,
                                         -464.650000,-382.364000,-340.569000,-404.365000,-443.335000,
                                         -326.753000, 476.907000,-244.593000,-448.905000, -91.907800,
                                         -66.398600,-133.711000,-348.542000,-144.365000,-188.748000,
                                         -164.683000,-138.777000,-230.507000,-187.727000,-307.054000,
                                         -285.807000,-209.072000,-215.764000,-110.062000,-320.607000),

                                  B6 = c(245.769000,-227.500000, 245.769000, 103.874000, # /  A5 /
                                         244.953000, 615.071000, 683.244000, 683.907000, 696.002000,
                                         190.252000, 116.252000, 226.679000, 103.425000,  83.361100,
                                         99.027300, 222.446000,-107.851000,  42.582000, 208.774000,
                                         180.805000, 145.100000,  83.057000, 122.357000, 574.039000,
                                         332.158000, 377.624000, 453.111000, 408.974000, 300.105000,
                                         194.852000, 230.785000, 310.330000, 285.641000, 342.911000,
                                         277.289000, 405.929000, 234.102000,-162.656000, 293.429000,
                                         -35.236000, 267.231000, 237.847000,  76.425700, -86.804200,
                                         -62.048300, 226.474000, -81.721600, -59.488600, 113.174700,
                                         457.974000, 375.896000, 337.064000, 398.094000, 443.097000,
                                         311.371000,-493.905000, 234.778000, 442.065000,  87.546200,
                                         62.331900, 132.158000, 325.719000, 126.622000, 164.022000,
                                         166.169000, 131.229000, 234.408000, 190.439000, 319.613000,
                                         290.010000, 212.327000, 219.274000, 114.130000, 291.316000),

                                  B7 = c(-93.776700,  77.847400, -93.776700, -43.198300, # / A6 /
                                         -93.554200,-223.241000,-242.849000,-242.329000,-247.255000,
                                         -59.195600, -31.699100, -68.419900, -26.974400, -17.876100,
                                         -25.394700, -67.235500,  49.634900,  -9.714680, -64.972300,
                                         -58.349100, -38.989100, -19.537800, -36.153000,-194.557000,
                                         -122.086000,-134.082000,-159.961000,-145.274000,-108.547000,
                                         -69.631700, -82.071600,-109.267000, -99.181100,-118.174000,
                                         -97.891100,-147.921000, -76.523900,  69.417450, -98.570700,
                                         22.579800, -86.493000, -78.110000, -18.803900,  26.432300,
                                         20.638000, -79.575800,  24.397690,  19.182900, -43.493800,
                                         -161.678000,-131.508000,-116.412000,-139.315000,-152.701000,
                                         -112.385000, 173.692000, -83.350300,-155.019000, -22.391300,
                                         -13.100600, -37.123400,-119.540000, -44.804200, -60.519000,
                                         -51.160500, -39.434300, -72.201800, -55.246500, -96.858900,
                                         -91.289600, -65.254900, -67.506500, -33.864500,-107.375000),

                                  B8 = c(-68.607800,  24.170500, -68.607800, -42.968400, # / A7 /
                                         -68.979700,-134.011000,-136.648000,-135.512000,-138.822000,
                                         -26.808200, -11.568800, -27.885500,  -9.419490,  -2.928140,
                                         -8.595340, -27.653800,  32.970100,  -6.791310, -28.324300,
                                         -35.923600, -17.785600, -10.236000, -25.173500,-104.029000,
                                         -80.687600, -81.393500, -93.295100, -88.006600, -71.288000,
                                         -47.330200, -53.966100, -67.064300, -60.691900, -69.414300,
                                         -60.767400, -92.348000, -41.265400,  43.800520, -52.708200,
                                         16.640930, -44.366300, -42.303300,  -9.530200,  -5.868160,
                                         -2.926490, -51.052500,  -6.821670,  -2.718070, -43.717900,
                                         -90.644500, -73.762000, -63.272200, -77.828900, -80.936600,
                                         -70.072700,  83.604700, -50.445300, -86.964000, -10.916000,
                                         -5.566370, -16.781900, -79.043000, -34.164700, -47.790300,
                                         -23.353500, -22.297200, -32.091900, -22.952800, -38.299400,
                                         -41.906400, -29.030300, -30.013700, -13.127600, -75.908500),

                                  B9 = c(-.501805,   -.432003,   -.501805,   -.521101, # / A8 /
                                         -.515927,   -.579394,   -.266482,   -.235648,   -.264349,
                                         -.717891,   -.579792,   -.348434,   -.265677,   -.140213,
                                         -.271687,   -.368528,   -.264409,   -.379613,   -.677418,
                                         -.622549,   -.646637,   -.552567,   -.193232,   -.588461,
                                         -.513819,   -.798101,   -.768113,   -.441413,   -.923509,
                                         -.259553,   -.544236,   -.546876,   -.315572,   -.522296,
                                         -.347960,   -.424184,   -.316966,   -.267361,   -.178618,
                                         -.196543,   -.371816,   -.316061,   -.767254,   -.308308,
                                         -.386790,   -.284043,   -.328126,   -.603221,   -.423541,
                                         -.422518,   -.403723,   -.602512,   -.387785,   -.130044,
                                         -.555893,   -.592947,   -.356600,   -.198430,   -.176773,
                                         -.226171,   -.289770,   -.659187,   -.405071,   -.203704,
                                         -.493395,   -.542294,   -.339675,   -.410790,   -.293923,
                                         -.383392,   -.397982,   -.399236,   -.574857,   -.254844),

                                  B10 = c(-.00066260,  .00178223, -.00066260,  .00077709, # / A9 /
                                          -.00046390, -.00179050, -.00282380, -.00283430, -.00299680,
                                          .00479949,  .00173412,  .00119333, -.00208540, -.00090960,
                                          -.00207190,  .00112833, -.00196000,  .00066221,  .00520019,
                                          .00046157, -.00170230,  .00043912,  .00240865, -.00272170,
                                          -.00541480, -.01011370, -.00806520, -.00558680, -.00198950,
                                          .00060134, -.00232860, -.00234850, -.00220230, -.00280190,
                                          -.00391030, -.00155890, -.00091860,  .00116445, -.00262940,
                                          -.00112340, -.00176040, -.00092340,  .00222098,  .00075468,
                                          -.00153570, -.00147480,  .00073275, -.00068640, -.00199120,
                                          .00158735, -.00114420, -.00330890, -.00048500, -.00655000,
                                          -.00087640,  .00763971,  .00037252,  .00703456,  .00054808,
                                          .00082323,  .00031112, -.00153770, -.00301020, -.00470030,
                                          .00156087, -.00070500, -.00050860, -.00022910,  .00020184,
                                          -.00195050, -.00038430, -.00036190,  .00405954, -.00422750),

                                  # from _ERB array in original sas codes
                                  ERR = exp(c(.008962,    .015054,    .000000,    .012565,
                                              .009579,    .009733,    .010168,    .010768,    .010204,
                                              .000000,    .006106,    .009996,    .011171,    .011011,
                                              .011174,    .000000,    .010692,    .012063,    .006882,
                                              .017432,    .012871,    .014559,    .015486,    .014226,
                                              .016066,    .015245,    .014831,    .017579,    .021031,
                                              .012128,    .011528,    .010179,    .011154,    .009601,
                                              .012707,    .009512,    .018791,    .009884,    .000000,
                                              .011530,    .000000,    .018632,    .006458,    .011746,
                                              .010894,    .013688,    .012310,    .016073,    .008570,
                                              .006078,    .009575,    .000000,    .008844,    .009283,
                                              .009139,    .005182,    .008504,    .007119,    .008382,
                                              .007751,    .008000,    .011352,    .013219,    .019071,
                                              .007161,    .013324,    .009783,    .009308,    .009321,
                                              .009090,    .009816,    .010112,    .005858,    .015513)/2))
      } else if (taperEquationForm == "KBECQCI") { # where is equation from
        TaperCoeffs <- data.table(EQN_NO = 1:74,
                                  # below B0 to B8 are from _CBEC_Q_2002
                                  B1 = c(0.697074,	0.980777,	0.470279,	0.832203,	0.855187, # /A0 /
                                         0.692238,	0.587918,	0.763041,	0.774001,	0.702051,
                                         0.946160,	0.888629,	0.891464,	0.912164,	0.963706,
                                         0.929690,	0.967745,	0.958944,	0.862635,	0.943965,
                                         1.013822,	1.001957,	0.854996,	0.888422,	0.917640,
                                         0.790946,	1.044910,	0.921736,	0.782832,	0.865189,
                                         0.846834,	0.824203,	0.784898,	0.791505,	0.874374,
                                         0.753585,	0.87323,	0.822588,	0.925665,	0.703487,
                                         1.096978,	0.918348,	0.750708,	1.133297,	0.856489,
                                         0.909755,	1.065324,	0.625287,	0.828758,	0.925039,
                                         0.936312,	0.866315,	0.924016,	0.823282,	0.869486,
                                         0.912291,	0.954651,	0.781048,	0.912068,	0.894384,
                                         0.675913,	0.693763,	0.587359,	0.678661,	0.792409,
                                         1.055272,	0.873000,	0.878697,	0.827157,	0.829601,
                                         0.856953,	0.843142,	0.832931,	1.12594),

                                  B2 = c(0.984191,	0.984880,	0.825508,	1.031080,	1.004580, # / A1 /
                                         1.025710,	0.990603,	0.987319,	0.977164,	0.930470,
                                         0.970066,	1.018170,	0.970117,	0.987020,	1.002770,
                                         1.007040,	1.000920,	0.998840,	0.983681,	0.962949,
                                         0.986204,	0.970817,	0.953731,	1.020540,	1.043920,
                                         1.059030,	1.085390,	1.007230,	1.060500,	0.918710,
                                         1.007570,	0.958215,	0.922923,	0.899732,	0.943590,
                                         0.930390,	0.983721,	1.028930,	0.996021,	0.940797,
                                         0.954388,	1.012930,	1.004220,	0.920441,	1.000540,
                                         0.978524,	0.994625,	0.998921,	1.045750,	1.041100,
                                         1.004120,	1.008480,	1.008580,	1.008530,	0.981791,
                                         1.034900,	1.039110,	1.065120,	1.015880,	0.946690,
                                         0.988752,	1.063850,	1.006390,	1.027590,	0.989184,
                                         0.979499,	0.986406,	0.982530,	1.046070,	0.972121,
                                         0.980573,	1.008130,	0.998840,	0.972037),

                                  B3 = c(0.092048,	0.000000,	0.408597,	0.000000,	0.000000, # / A2 /
                                         0.065998,	0.167431,	0.088551,	0.078353,	0.169054,
                                         0.036379,	0.000000,	0.057080,	0.028816,	0.000000,
                                         0.000000,	0.000000,	0.000000,	0.051593,	0.054259,
                                         0.000000,	0.017320,	0.089223,	0.000000,	-0.030260,
                                         0.000000,	-0.117568,	0.000000,	0.000000,	0.083304,
                                         0.000000,	0.064930,	0.114861,	0.138371,	0.052367,
                                         0.118792,	0.040875,	0.000000,	0.000000,	0.161374,
                                         0.000000,	0.000000,	0.079550,	0.000000,	0.000000,
                                         0.000000,	-0.067298,	0.154001,	0.000000,	-0.031225,
                                         0.000000,	0.022381,	0.000000,	0.037550,	0.045308,
                                         -0.026858,	-0.040248,	0.000000,	0.000000,	0.078050,
                                         0.113469,	0.000000,	0.127014,	0.055666,	0.075818,
                                         0.000000,	0.048413,	0.047642,	0.000000,	0.079652,
                                         0.057827,	0.031402,	0.045616,	0.000000),

                                  B4 = c(0.571823,	0.425990,	0.518996,	0.555893,	0.616982, # / B1/
                                         0.617946,	0.561594,	0.567474,	0.562123,	0.368305,
                                         0.267499,	0.281762,	0.329031,	0.280213,	0.316520,
                                         0.322973,	0.229936,	0.338360,	0.300719,	0.385844,
                                         0.365871,	0.350235,	0.509025,	0.535892,	0.551160,
                                         0.535476,	0.519758,	0.585822,	0.531431,	0.408556,
                                         0.439966,	0.466737,	0.496895,	0.467790,	0.464744,
                                         0.492167,	0.41579,	0.223070,	0.336874,	0.445750,
                                         0.292216,	0.265524,	0.391716,	0.406280,	0.416442,
                                         0.458164,	0.409168,	0.496851,	0.575368,	0.400319,
                                         0.323048,	0.341563,	0.382689,	0.381356,	0.414707,
                                         0.145265,	0.335144,	0.406749,	0.401949,	0.285474,
                                         0.329021,	0.541075,	0.520076,	0.567757,	0.318595,
                                         0.261666,	0.358853,	0.349046,	0.346671,	0.365467,
                                         0.260685,	0.319773,	0.261189,	-0.144764),

                                  B5 = c(-0.532732,	-0.408586,	0.000000,	-0.819495,	-0.600928, # /B2/
                                         -0.732773,	0.000000,	-0.445677,	-0.406941,	-0.871972,
                                         -0.758032,	-0.513748,	-0.535456,	-0.430046,	-0.855607,
                                         -0.893384,	-0.632784,	-0.673099,	-0.609452,	-1.12617,
                                         -0.933513,	-0.922568,	-0.154652,	-0.929744,	-1.087550,
                                         -1.477960,	-1.631900,	-0.311250,	-1.393960,	-0.255448,
                                         -0.857115,	-0.728184,	-0.398502,	-0.596136,	-0.837250,
                                         -0.483977,	-0.487686,	-0.506310,	-0.673516,	-0.614371,
                                         -0.828280,	-0.651588,	-0.779147,	-0.379927,	-0.606263,
                                         -0.624715,	-0.562042,	-0.557429,	-0.755372,	-0.628840,
                                         -0.941452,	-0.975034,	-0.580517,	-0.637805,	-0.786627,
                                         -0.642841,	-0.625804,	0.000000,	-0.619283,	-0.262013,
                                         -0.622173,	-0.920213,	-0.510024,	-0.810195,	-0.497244,
                                         -0.643975,	-0.577954,	-0.584707,	-0.553554,	-0.617612,
                                         -0.469767,	-0.620014,	-0.503513,	-0.578112),

                                  B6 = c(0.585361,	0.599333,	0.586112,	0.684864,	0.506335, # /  B3 /
                                         0.511722,	0.422530,	0.573964,	0.429284,	0.744146,
                                         0.658246,	0.540385,	0.605273,	0.615161,	0.716103,
                                         0.614989,	0.736691,	0.646087,	0.701826,	0.693264,
                                         0.600301,	0.582394,	0.498134,	0.490758,	0.632185,
                                         0.764237,	0.752113,	0.518927,	0.823577,	0.381950,
                                         0.495484,	0.468689,	0.407022,	0.426090,	0.447888,
                                         0.376916,	0.521597,	0.609010,	0.519966,	0.684340,
                                         0.564943,	0.776771,	0.742984,	0.346425,	0.493724,
                                         0.475407,	0.428397,	0.807706,	0.315451,	0.455121,
                                         0.608207,	0.577124,	0.483719,	0.442884,	0.492388,
                                         0.582900,	0.527018,	0.366657,	0.435591,	0.518040,
                                         0.538638,	0.379487,	0.423550,	0.342971,	0.650584,
                                         0.589574,	0.590280,	0.583692,	0.626023,	0.566992,
                                         0.594807,	0.607053,	0.738097,	0.693395),

                                  B7 = c(1.031500,	0.000000,	-1.139980,	2.764040,	0.000000,# / B4/
                                         1.581340,	-1.042440,	0.604079,	0.932614,	0.000000,
                                         2.158570,	0.961118,	1.351210,	1.337750,	1.520660,
                                         1.957730,	2.241810,	1.313000,	0.000000,	3.04716	,
                                         2.154230,	1.924880,	-0.954664,	1.802300,	3.073710,
                                         5.437050,	6.194730,	0.739471,	4.189560,	0.279214,
                                         2.657920,	2.374190,	1.239400,	1.298160,	3.543260,
                                         1.156240,	3.06834,	2.750420,	4.268100,	2.192590,
                                         5.902140,	0.000000,	0.000000,	1.620100,	1.904660,
                                         1.862690,	1.677760,	0.000000,	1.794500,	0.894543,
                                         2.169960,	2.121340,	1.025690,	1.968660,	1.210020,
                                         0.000000,	1.076270,	-2.012740,	2.539620,	0.641732,
                                         1.616470,	2.702480,	0.702013,	2.541040,	0.000000,
                                         2.69459,	1.065000,	1.085990,	0.660229,	1.672490,
                                         1.803020,	1.403530,	-0.491759,	6.50447),

                                  B8 = c(0.018622,	0.027804,	0.000000,	0.036282,	0.079865, # / B5	 /
                                         0.040625,	0.000000,	0.000000,	0.036028,	0.091529,
                                         0.090207,	0.071218,	0.037795,	0.070896,	0.057347,
                                         0.056401,	0.070556,	0.054767,	0.065195,	0.063239,
                                         0.130682,	0.113347,	0.086719,	0.110408,	0.057013,
                                         0.084584,	0.099053,	0.075925,	0.102576,	0.043055,
                                         0.059184,	0.057825,	0.043678,	0.075816,	0.042725,
                                         0.040249,	0.050408,	0.067192,	0.072168,	0.000000,
                                         0.107168,	-0.019603,	0.091586,	0.071055,	0.045593,
                                         0.055198,	0.061444,	0.000000,	0.075022,	0.058064,
                                         0.060198,	0.067085,	0.058644,	0.058989,	0.065964,
                                         0.070425,	0.050757,	0.055123,	0.063571,	0.084762,
                                         0.088421,	0.031049,	0.000000,	0.019376,	0.050670,
                                         0.08509,	0.058842,	0.078816,	0.077040,	0.069729,
                                         0.060404,	0.067863,	0.035516,	0.082599),

                                  B9 = c(-0.225146,	-0.401537,	0.000000,	-0.491508,	-0.531571,# /  b6	 /
                                         -0.209009,	0.000000,	-0.182052,	-0.169494,	-0.743705,
                                         -0.749478,	-0.654727,	-0.360031,	-0.639182,	-0.480415,
                                         -0.552446,	-0.720140,	-0.519713,	-0.556178,	-0.355561,
                                         -0.556858,	-0.383764,	-0.189895,	-0.275766,	-0.375225,
                                         -0.559917,	-0.637429,	-0.408469,	-0.711948,	-0.204499,
                                         -0.232023,	-0.271688,	-0.222667,	-0.270822,	-0.318190,
                                         -0.057124,	-0.501109,	-0.793186,	-0.692041,	-0.316217,
                                         -1.014470,	-0.194663,	-0.466741,	-0.247628,	-0.289813,
                                         -0.304118,	-0.283161,	-0.109494,	0.000000,	-0.258598,
                                         -0.422735,	-0.355627,	-0.292710,	-0.350828,	-0.332804,
                                         -0.524672,	-0.297841,	-0.271375,	-0.316662,	-0.643002,
                                         -0.612866,	0.000000,	0.106217,	0.000000,	-0.351825,
                                         -0.666734,	-0.329744,	-0.463912,	-0.504864,	-0.385494,
                                         -0.442514,	-0.438919,	-0.356541,	-1.00605),

                                  # error term from ARRAY _ERB_Q_2002
                                  ERR = c(1.00446,	1.00768,	 1.00952,	1.00613,	1.008825,
                                          1.004719,	1.003938,	1.007869,	1.005168, 1.002143,
                                          1.00315,	1.005004,	1.005722,	1.005777,	1.005358,
                                          1.003793,	1.005464,	1.005911,	1.003682,	1.011184,
                                          1.00638,	1.007075,	1.007603,	1.007159,	1.007517,
                                          1.007491,	1.00579,	1.009798,	1.010023,	1.006131,
                                          1.005544,	1.005038,	1.006373,	1.004945,	1.006178,
                                          1.004648,	1.0103354,	1.004987,	1.007584,	1.00955,
                                          1.007599,	1.002855,	1.003156,	1.004631,	1.00536,
                                          1.00639,	1.006045,	1.007925,	1.004216,	1.002821,
                                          1.006966,	1.004492,	1.004334,	1.004274,	1.004354,
                                          1.002498,	1.004025,	1.003508,	1.00483,	1.003882,
                                          1.003722,	1.005371,	1.006804,	1.009481,	1.003595,
                                          1,			1.004711,	1.004692,	1.0046,		1.004587,
                                          1.002349,	1.004949,	1.002927,	1))
      } else if (taperEquationForm == "KBEVQCI_OLD") {
        ## this one is treaky, as in the original sas codes, there is no comment for this values, i.e, cut lines
        # using matrix method
        # the matrix has 10 rows and 74 columns
        coeffs <- c( .926472,   1.101792,    .926472,    .672378, # from _CBEC_Q
                     .866667,    .984285,   1.135583,   1.173494,   1.144134,
                     .842384,    .704739,   1.192696,   1.582557,   1.372364,
                     1.583827,   1.185559,   1.609732,   1.226561,    .865989,
                     1.222384,   2.332067,   1.735756,   2.006167,   2.496071,
                     .990422,    .992562,    .649193,   1.115784,    .340047,
                     1.207477,   1.253020,   1.361797,   1.312899,   1.400177,
                     1.217588,   1.117082,   1.266397,   1.109871,    .987946,
                     1.054211,    .792637,   1.073364,   1.579174,    .986780,
                     1.082503,   1.112471,   1.032756,   1.106740,   1.675228,
                     .974504,   1.222762,   1.112247,   1.225558,   1.134229,
                     1.316970,    .841285,   1.011506,   1.461466,   1.341984,
                     1.294196,   1.352042,   1.048176,   1.300887,   1.344632,
                     1.146634,   1.197434,   1.517568,   1.356492,   1.372405,
                     1.384212,   1.234876,   1.231969,   1.335956,   1.380198,
                     1.023400,    .969435,   1.023400,   1.149360,
                     1.049010,   1.026200,    .995001,    .985894,    .993345,
                     1.123420,   1.208190,    .981241,    .892999,    .946051,
                     .892642,    .983256,    .870578,    .978191,   1.111810,
                     1.011830,    .844348,    .940212,    .868523,    .821778,
                     1.068810,   1.026020,   1.239080,   1.023700,   1.525200,
                     .945081,    .956041,    .921515,    .926191,    .934137,
                     .942922,    .986962,   0.963983,    .967993,   1.042700,
                     .997259,   1.146680,   1.015280,    .881474,   1.027500,
                     .978596,    .995964,   1.005460,    .995549,    .912974,
                     1.077140,    .986634,   1.033920,    .983522,   1.021980,
                     .933305,   1.054190,   1.055710,    .874587,    .963313,
                     .976095,    .955550,    .986993,    .911281,    .922500,
                     1.015090,   1.005740,    .938039,    .980958,    .969735,
                     .977568,    .997534,    .998729,    .922608,    .935203,
                     .999683,   1.000429,    .999683,    .997229,
                     .999110,   1.000662,    .999269,    .999232,    .999225,
                     .996431,    .992804,    .999920,   1.001111,    .999789,
                     1.001128,    .999864,   1.002709,    .998981,    .996946,
                     .999229,   1.000894,    .999218,   1.001385,   1.001576,
                     .996624,   1.004024,    .993890,    .998966,    .982712,
                     .999882,    .999780,   1.000689,   1.000310,    .999642,
                     .999602,    .999152,    .999834,   1.001018,    .997706,
                     .999812,    .995253,    .998820,   1.006172,    .997511,
                     .999494,    .997311,    .998124,   1.003166,   1.001646,
                     .996946,    .998821,    .997325,    .999177,    .996523,
                     1.001822,   1.000479,    .996782,   1.006517,    .999675,
                     .999388,   1.000253,   1.000590,   1.002578,   1.001281,
                     .999170,    .999200,    .999824,    .998925,    .999491,
                     .998811,    .999003,    .998974,   1.003277,   1.000656,
                     121.365000, -47.728900, 121.365000,  73.711200,
                     121.991000, 244.333000, 252.892000, 251.359000, 257.022000,
                     57.109400,  29.736900,  61.717800,  25.666500,  14.968800,
                     24.158500,  61.047100, -51.683300,  16.972100,  60.571100,
                     -133.480000,  44.702800,  28.652900,  53.192400, 201.623000,
                     146.127000, 151.666000, 174.433000, 163.160000, 131.079000,
                     87.286900, 100.448000, 125.413000, 114.078000, 132.142000,
                     112.689000, 168.309000,  73.290000, -71.755900, 101.902000,
                     -22.873000,  89.070600,  83.363300,  25.673500,   7.225950,
                     4.206740,  95.228000,   8.792230,   4.073990,  78.621200,
                     169.168000, 138.851000, 120.901000, 146.438000, 153.783000,
                     128.699000,-156.170000,  93.935600, 162.475000,  27.513400,
                     17.939300,  39.495800, 143.388000,  63.486100,  86.179000,
                     50.551900,  29.181600,  69.195100,  53.450600,  85.176500,
                     88.050800,  62.893100,  64.890800,  30.638600,-453.654000,
                     -272.245000, 198.234000,-272.245000,-133.272000,
                     -272.272000,-634.980000,-692.263000,-691.926000,-704.738000,
                     -187.239000,-113.372000,-219.227000,-101.253000, -79.652700,
                     -96.925900,-215.498000, 110.779000, -48.983200,-203.473000,
                     390.901000,-149.679000, -91.178600,-138.478000,-579.877000,
                     -354.998000,-393.699000,-466.130000,-425.603000,-321.202000,
                     -211.746000,-248.130000,-325.419000,-299.577000,-355.819000,
                     -291.096000,-425.359000,-160.612000, 165.702000,-295.854000,
                     36.329500,-268.816000,-242.180000, -82.210600,  53.928400,
                     38.103800,-241.225000,  49.323100,  37.370200,-147.295000,
                     -464.650000,-382.364000,-340.569000,-404.365000,-443.335000,
                     -326.753000, 476.907000,-244.593000,-448.905000, -91.907800,
                     -66.398600,-133.711000,-348.542000,-144.365000,-188.748000,
                     -164.683000, -57.156400,-230.507000,-187.727000,-307.054000,
                     -285.807000,-209.072000,-215.764000,-110.062000,1292.540000,
                     245.769000,-227.500000, 245.769000, 103.874000,
                     244.953000, 615.071000, 683.244000, 683.907000, 696.002000,
                     190.252000, 116.252000, 226.679000, 103.425000,  83.361100,
                     99.027300, 222.446000,-107.851000,  42.582000, 208.774000,
                     -406.866000, 145.100000,  83.057000, 122.357000, 574.039000,
                     332.158000, 377.624000, 453.111000, 408.974000, 300.105000,
                     194.852000, 230.785000, 310.330000, 285.641000, 342.911000,
                     277.289000, 405.929000, 139.097000,-162.656000, 293.429000,
                     -35.236000, 267.231000, 237.847000,  76.425700, -86.804200,
                     -62.048300, 226.474000, -81.721600, -59.488600, 113.174700,
                     457.974000, 375.896000, 337.064000, 398.094000, 443.097000,
                     311.371000,-493.905000, 234.778000, 442.065000,  87.546200,
                     62.331900, 132.158000, 325.719000, 126.622000, 164.022000,
                     166.169000,  40.737200, 234.408000, 190.439000, 319.613000,
                     290.010000, 212.327000, 219.274000, 114.130000,-1312.69000,
                     -93.776700,  77.847400, -93.776700, -43.198300,
                     -93.554200,-223.241000,-242.849000,-242.329000,-247.255000,
                     -59.195600, -31.699100, -68.419900, -26.974400, -17.876100,
                     -25.394700, -67.235500,  49.634900,  -9.714680, -64.972300,
                     150.395000, -38.989100, -19.537800, -36.153000,-194.557000,
                     -122.086000,-134.082000,-159.961000,-145.274000,-108.547000,
                     -69.631700, -82.071600,-109.267000, -99.181100,-118.174000,
                     -97.891100,-147.921000, -50.776400,  69.417450, -98.570700,
                     22.579800, -86.493000, -78.110000, -18.803900,  26.432300,
                     20.638000, -79.575800,  24.397690,  19.182900, -43.493800,
                     -161.678000,-131.508000,-116.412000,-139.315000,-152.701000,
                     -112.385000, 173.692000, -83.350300,-155.019000, -22.391300,
                     -13.100600, -37.123400,-119.540000, -44.804200, -60.519000,
                     -51.160500, -11.880200, -72.201800, -55.246500, -96.858900,
                     -91.289600, -65.254900, -67.506500, -33.864500, 474.139000,
                     -68.607800,  24.170500, -68.607800, -42.968400,
                     -68.979700,-134.011000,-136.648000,-135.512000,-138.822000,
                     -26.808200, -11.568800, -27.885500,  -9.419490,  -2.928140,
                     -8.595340, -27.653800,  32.970100,  -6.791310, -28.324300,
                     75.119000, -17.785600, -10.236000, -25.173500,-104.029000,
                     -80.687600, -81.393500, -93.295100, -88.006600, -71.288000,
                     -47.330200, -53.966100, -67.064300, -60.691900, -69.414300,
                     -60.767400, -92.348000, -40.414400,  43.800520, -52.708200,
                     16.640930, -44.366300, -42.303300,  -9.530200,  -5.868160,
                     -2.926490, -51.052500,  -6.821670,  -2.718070, -43.717900,
                     -90.644500, -73.762000, -63.272200, -77.828900, -80.936600,
                     -70.072700,  83.604700, -50.445300, -86.964000, -10.916000,
                     -5.566370, -16.781900, -79.043000, -34.164700, -47.790300,
                     -23.353500, -14.452800, -32.091900, -22.952800, -38.299400,
                     -41.906400, -29.030300, -30.013700, -13.127600, 248.431000,
                     -.501805,   -.432003,   -.501805,   -.521101,
                     -.515927,   -.579394,   -.266482,   -.235648,   -.264349,
                     -.717891,   -.579792,   -.348434,   -.265677,   -.140213,
                     -.271687,   -.368528,   -.264409,   -.379613,   -.677418,
                     -.689615,   -.646637,   -.552567,   -.193232,   -.588461,
                     -.513819,   -.798101,   -.768113,   -.441413,   -.923509,
                     -.259553,   -.544236,   -.546876,   -.315572,   -.522296,
                     -.347960,   -.424184,   -.242209,   -.267361,   -.178618,
                     -.196543,   -.371816,   -.316061,   -.767254,   -.308308,
                     -.386790,   -.284043,   -.328126,   -.603221,   -.423541,
                     -.422518,   -.403723,   -.602512,   -.387785,   -.130044,
                     -.555893,   -.592947,   -.356600,   -.198430,   -.176773,
                     -.226171,   -.289770,   -.659187,   -.405071,   -.203704,
                     -.493395,   -.340099,   -.339675,   -.410790,   -.293923,
                     -.383392,   -.397982,   -.399236,   -.574857,   -.251243,
                     -.00066260,  .00178223, -.00066260,  .00077709,
                     -.00046390, -.00179050, -.00282380, -.00283430, -.00299680,
                     .00479949,  .00173412,  .00119333, -.00208540, -.00090960,
                     -.00207190,  .00112833, -.00196000,  .00066221,  .00520019,
                     .00125141, -.00170230,  .00043912,  .00240865, -.00272170,
                     -.00541480, -.01011370, -.00806520, -.00558680, -.00198950,
                     .00060134, -.00232860, -.00234850, -.00220230, -.00280190,
                     -.00391030, -.00155890, -.00196872,  .00116445, -.00262940,
                     -.00112340, -.00176040, -.00092340,  .00222098,  .00075468,
                     -.00153570, -.00147480,  .00073275, -.00068640, -.00199120,
                     .00158735, -.00114420, -.00330890, -.00048500, -.00655000,
                     -.00087640,  .00763971,  .00037252,  .00703456,  .00054808,
                     .00082323,  .00031112, -.00153770, -.00301020, -.00470030,
                     .00156087, -.00025252, -.00050860, -.00022910,  .00020184,
                     -.00195050, -.00038430, -.00036190,  .00405954,  .00559064)

        err <- c( .008962,    .015054,    .000000,    .012565, # from _ERB_Q in sas codes
                  .009579,    .009733,    .010168,    .010768,    .010204,
                  .000000,    .006106,    .009996,    .011171,    .011011,
                  .011174,    .000000,    .010692,    .012063,    .006882,
                  .030417,    .012871,    .014559,    .015486,    .014226,
                  .016066,    .015245,    .014831,    .017579,    .021031,
                  .012128,    .011528,    .010179,    .011154,    .009601,
                  .012707,    .009512,    .000000,    .009884,    .000000,
                  .011530,    .000000,    .018632,    .006458,    .011746,
                  .010894,    .013688,    .012310,    .016073,    .008570,
                  .006078,    .009575,    .000000,    .008844,    .009283,
                  .009139,    .005182,    .008504,    .007119,    .008382,
                  .007751,    .008000,    .011352,    .013219,    .019071,
                  .007161,    .000000,    .009783,    .009308,    .009321,
                  .009090,    .009816,    .010112,    .005858,    .019712)
        coeffs <- data.table(as.data.frame(matrix(data = coeffs,
                                                  nrow = 74,
                                                  ncol = 10)))
        names(coeffs) <- paste("B", 1:10, sep = "")


        TaperCoeffs <- cbind(data.table(EQN_NO = 1:74),
                             coeffs,
                             data.table(ERR = exp(err/2)))
        rm(coeffs, err)

      }
      taperCoeffs <- setkey(headtable, EQN_NO)[setkey(TaperCoeffs, EQN_NO),
                                               nomatch = 0]

      taperCoeffs[, EQN_NO := NULL]
    }



    return(taperCoeffs)

  })


#' @rdname taperCoeffsGenerator
setMethod("taperCoeffsGenerator",
          signature = signature(taperEquationForm = "missing"),
          definition = function(){
            return(taperCoeffsGenerator(taperEquationForm = "KBEC"))
          })






