#' Site index conversion from one species to another species
#'
#'
#' @description This function takes a known species and its site index and convert the site
#'              index for the species for a given bec zone
#'
#' @param spCode_to character, The species code the site index conversion is for. This is not
#'                             case sensitive.
#' @param BECZone character, The BEC zone the conversion takes place.
#' @param BECSubZone character, The sub BEC zone the conversion takes place.
#' @param availableSI character, Contains available site index for given species.
#'                                      Must be provided like \code{"at=10, bc=25"}.
#'
#' @return either valid site index or NA
#' @note The site index conversion functions were taken from 1) sindex.dll (primarily),
#'       2) expert knowledge of Gord Nigh and Dave Waddell
#'       The original functions were written by Dave Waddell, and can be found at
#'       https://github.com/bcgov/FAIB_PSPL/blob/main/05_site_index_conversions/site_index_conversion_equations_v3.r
#'       The function derives site index for target species based on priority orders of
#'       the available site index
#'       specifically, for at: sw>sx>se>bl>pl>fd
#'       ba: hw>ss>sx>cw>fd>bg(one_to_one)>bl(one_to_one)
#'       bl: sw>sx>pl>at>fd>lw>sb>ba(one_to_one)>bg(one_to_one)
#'       cw: hw>ba>ss>sx>fd>sw
#'       fd(coastal): hw>cw>ba>ss>sx
#'       fd(interior): pl>bl>hw>sw>sx>at>lw>sb
#'       hw(coastal): cw>ba>ss>sx>fd>hm(one_to_one)
#'       hw(interior):fd>sw>sx>pl>lw>hm(one_to_one)
#'       lw: fd>bl>pl>sw>sx>hw(interior)>sb>lt(one_to_one)
#'       pl: sw>sx>hw(interior)>at>bl>fd(interior)>lw>sb>ss(interior)>pa
#'       sb: pl>lw>fd(interior)>bl>sw>sx
#'       ss: hw(coastal)>ba>cw>fd(coastal)>sx(one_to_one)>pw(one_to_one)>fd(one_to_one)
#'       sw: pl>at>hw(interior)>bl>lw>fd(interior)>sb>se(one_to_one)>sx(one_to_one)
#'       sx(coastal): hw>ba>cw>fd
#'       sx: pl>at>hw(interior)>bl>lw>fd(interior)>sb>sw(one_to_one)
#'       se: sw(one_to_one)
#'       bg: ba
#'       pw: ss(coastal)>hw(coastal)>sw>fd(one_to_one)
#'       lt: lw(one_to_one)
#'       hm: hw
#'       pa: pl
#'       dr: fd(coastal)>hw(coastal)
#'       py: fd
#'
#'
#'
#'
#' @importFrom data.table ':='
#' @importFrom dplyr case_when
#' @importFrom stringr str_split
#'
#' @examples
#' \dontrun{
#' # convert sw to at
#' at_si <- convertSI2SI(spCode_to = "AT",
#'                       BECZone = "unknown", # for some species, BEC zone must be provided
#'                       BECSubZone = "unknown",
#'                       availableSI = "sw = 30, pl = 50, fd = 20")
#' print(at_si) # 32.8353
#' # convert ta to fd in CWH, for which the conversion equation can not be found
#' py_si <- convertSI2SI(spCode_to = "py",
#'                               BECZone = "CWH", # for some species, BEC zone must be provided
#'                               BECSubZone = "unknown",
#'                               availableSI = "fd = 30")
#' print(py_si) # 30
#' }
#'
#' @export
#' @docType methods
#' @rdname convertSI2SI
#'
#' @author Yong Luo
convertSI2SI <- function(spCode_to,
                         BECZone = as.character(NA),
                         BECSubZone = as.character(NA),
                         availableSI){
  spCode_to <- tolower(spCode_to)
  availableSI <- gsub(" ", "", availableSI)
  inputtable <- data.table(avail_si = unlist(str_split(availableSI, ",")))

  inputtable[, ':='(sp_avail = unlist(lapply(avail_si, function(x){unlist(str_split(x, "="))[1]})),
                    si = as.numeric(unlist(lapply(avail_si, function(x){unlist(str_split(x, "="))[2]}))))]
  inputtable[, ':='(avail_si = NULL,
                    sp_avail = tolower(sp_avail),
                    id_temp = 1)]
  spCode_from <- inputtable$sp_avail
  all_sp_codes <- c("sw", "sx", "se", "bl", "pl", "fd",
                    "hw", "ss", "cw", "at", "lw", "sb",
                    "ba", "bg", "pw", "lt", "hm", "pa",
                    "dr", "py")
  if(!(spCode_to %in% all_sp_codes)){
    warning(paste0("spCode_to (", spCode_to, ") is not in the species list. Please check."))
  }
  if(length(spCode_from[!(spCode_from %in% all_sp_codes)]) > 0){
    warning(paste0("There are/is invalid species code in availableSI (",
                   paste(spCode_from[!(spCode_from %in% all_sp_codes)], collapse = ", "),
                   "). Please check."))
  }
  inputtable <- reshape(data = inputtable,
                        direction = "wide",
                        idvar = "id_temp",
                        timevar = "sp_avail",
                        v.names = "si",
                        sep = "_")
  inputtable[, id_temp := NULL]
  names(inputtable) <- paste0(gsub("si_", "", names(inputtable)), "_si")


  inputtable$bec_zone <- BECZone
  inputtable$bec_subzone <- BECSubZone

  all_sp_codes <- paste0(all_sp_codes, "_si")[!(paste0(all_sp_codes, "_si") %in% paste0(spCode_from, "_si"))]
  inputtable[, c(all_sp_codes) := NA]

  spCode_for_function <- spCode_to
  if(spCode_to %in% c("fd", "hw")){
    if(BECZone %in% c("CWH", "CDF", "MH")){
      spCode_for_function <- paste0(spCode_to, "c") # coastal
    } else {
      spCode_for_function <- paste0(spCode_to, "i") # interior
    }
  }
  if(spCode_to %in% c("sx")){
    if(BECZone %in% c("CWH", "CDF", "MH")){
      spCode_for_function <- paste0(spCode_to, "c") # coastal
    }
  }
  # first attempt for the function name
  thefunctionName <- paste0("convert_", spCode_for_function, "_si")
  rm(spCode_for_function)
  # load dave's conversion functions
  dave_function_env <- daveConversionFunctions()

  if(thefunctionName %in% ls(envir = dave_function_env)){
    thefunction <- get(thefunctionName, envir = dave_function_env)
    site_index <- thefunction(inputtable)
  } else {
    site_index <- NA
  }


  # second attempt for one to one conversion functions
  if(is.na(site_index)){
    if(spCode_to %in% c("sw", "ss", "se", "sx")){
      spCode_from_one <- NULL
      if(spCode_to == "sw" & "se" %in% spCode_from){
        spCode_from_one <- "se" # set se as priority
      } else if("sx" %in% spCode_from){
        spCode_from_one <- "sx"
      } else if("sw" %in% spCode_from){
        spCode_from_one <- "sw"
      }
      if(!is.null(spCode_from_one)){
        thefunctionName <- paste0("convert_", spCode_to, "_from_", spCode_from_one)
      }
    } else if (spCode_to %in% c("ba", "bl", "bg", "pw", "ss", "lt", "lw",
                                "hm", "hw", "pa", "pl", "dr", "py")){

      thefunctionName <- paste0("convert_", spCode_to)
    }
    if(thefunctionName %in% ls(envir = dave_function_env)){
      thefunction <- get(thefunctionName, envir = dave_function_env)
      site_index <- thefunction(inputtable)
    } else {
      site_index <- NA
    }

    if(is.na(site_index)){
      # last attempt for special conversions
      if((spCode_to == "pw" & "fd" %in% spCode_from) |
         (spCode_to == "ss" & "fd" %in% spCode_from)){
        thefunctionName <- paste0("convert_", spCode_to, "_from_fd")
        if(thefunctionName %in% ls(envir = dave_function_env)){
          thefunction <- get(thefunctionName, envir = dave_function_env)
          site_index <- thefunction(inputtable)
        } else {
          site_index <- NA
        }
      }
    }
  }
  return(site_index)
}


daveConversionFunctions <- function(){
  ####################################
  ## the below functions were written by Dave at
  ## https://github.com/bcgov/FAIB_PSPL/blob/main/05_site_index_conversions/site_index_conversion_equations_v3.r
  ####################################
  env_dave_functions <- new.env()


  # v3.0
  # set up for missing species check


  # Translated from C code used for SIndex

  # Aspen At
  env_dave_functions$convert_at_si <- function(inp){
    si <- case_when(
      inp$sw_si > 0 ~ -4.768112309  + 1.253446979 * inp$sw_si,
      inp$sx_si > 0 ~ -4.768112309  + 1.253446979 * inp$sx_si ,
      inp$se_si > 0 ~ -4.768112309  + 1.253446979 * inp$se_si ,
      inp$bl_si > 0 ~ -7.216706405  + 1.457496490 * inp$bl_si ,
      inp$pl_si > 0 ~ -7.452123778  + 1.362442366 * inp$pl_si ,
      inp$fd_si > 0 ~ -12.846637615 + 1.700742166 * inp$fd_si,
      TRUE ~ inp$at_si )

    return(si)

  }



  # Balsam Ba
  env_dave_functions$convert_ba_si <- function(inp){
    si <- case_when(
      inp$hw_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ -1.977317550 + 0.986193290 * inp$hw_si ,
      inp$ss_si > 0 ~ 1.928007878 + 0.789940825 * inp$ss_si ,
      inp$sx_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH')  ~ 1.928007878 + 0.789940825 * inp$sx_si ,
      inp$cw_si > 0 ~ -0.738658778 + 1.033530568 * inp$cw_si ,
      inp$fd_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ -2.403353051 + 0.886587768 * inp$fd_si ,
      TRUE ~ inp$ba_si )

    return(si)

  }

  # Balsam Bl
  env_dave_functions$convert_bl_si <- function(inp){
    si <- case_when(
      inp$sw_si > 0 ~ 1.680000000 + 0.860000000 * inp$sw_si ,
      inp$sx_si > 0 ~ 1.680000000 + 0.860000000 * inp$sx_si ,
      inp$pl_si > 0 ~ 0.474311930 + 0.917431190 * inp$pl_si ,
      inp$at_si > 0 ~ 4.951440000 + 0.686108000 * inp$at_si ,
      inp$fd_si > 0 ~ -0.221100912 + 0.981651373 * inp$fd_si ,
      inp$lw_si > 0 ~ -1.360550450 + 0.954128438 * inp$lw_si ,
      inp$sb_si > 0 ~ -3.497247692 + 1.436697244 * inp$sb_si ,
      TRUE ~ inp$bl_si )

    return(si)

  }

  # Cedar Cw
  env_dave_functions$convert_cw_si <- function(inp){
    si <- case_when(
      inp$hw_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~  -1.198473280 + 0.954198470 * inp$hw_si ,
      inp$ba_si > 0 ~  0.714694652 + 0.967557249 * inp$ba_si ,
      inp$ss_si > 0 ~ 2.580152661 + 0.764312974 * inp$ss_si ,
      inp$sx_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH')  ~  2.580152661 + 0.764312974 * inp$sx_si ,
      inp$fd_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH')~  -1.610687019 + 0.857824425 * inp$fd_si  ,
      inp$sw_si > 0 ~ inp$sw_si,   # added for fail over
      TRUE ~ inp$cw_si )

    return(si)

  }



  # Coastal Fir Fdc
  env_dave_functions$convert_fdc_si <- function(inp){
    si <- case_when(
      inp$hw_si > 0 ~ 0.480533930 + 1.112347050 * inp$hw_si ,
      inp$cw_si > 0 ~ 1.877641825 + 1.165739708 * inp$cw_si ,
      inp$ba_si > 0 ~ 2.710789765 + 1.127919909 * inp$ba_si ,
      inp$ss_si > 0 ~ 4.885428248 + 0.890989987 * inp$ss_si ,
      inp$sx_si > 0 ~ 4.885428248 + 0.890989987 * inp$sx_si ,
      TRUE ~ inp$fd_si)

    return(si)

  }

  # Interior Fir Fdi
  env_dave_functions$convert_fdi_si <- function(inp){

    si <- case_when(
      inp$pl_si > 0 ~ 0.708411210 + 0.934579440 * inp$pl_si ,
      inp$bl_si > 0 ~ 0.225233640 + 1.018691590 * inp$bl_si ,
      inp$hw_si > 0 ~ 4.560000000 + 0.887000000 * inp$hw_si ,
      inp$sw_si > 0 ~ 4.750000000 + 0.737000000 * inp$sw_si ,
      inp$sx_si > 0 ~ 4.750000000 + 0.737000000 * inp$sx_si ,
      inp$at_si > 0 ~ 7.553548000 + 0.587978600 * inp$at_si ,
      inp$lw_si > 0 ~ -0.690000000 + 0.983000000 * inp$lw_si ,
      inp$sb_si > 0 ~ -3.337383186 + 1.463551403 * inp$sb_si ,
      TRUE ~ inp$fd_si )

    return(si)

  }

  # Coastal Hemlock Hwc
  env_dave_functions$convert_hwc_si <- function(inp){
    si <- case_when(
      inp$cw_si > 0 ~ 1.256000000 + 1.048000000 * inp$cw_si ,
      inp$ba_si > 0 ~ 2.005000000 + 1.014000000 * inp$ba_si ,
      inp$ss_si > 0 ~ 3.960000000 + 0.801000000 * inp$ss_si ,
      inp$sx_si > 0 ~ 3.960000000 + 0.801000000 * inp$sx_si ,
      inp$fd_si > 0 ~ -0.432000000 + 0.899000000 * inp$fd_si ,
      TRUE ~ inp$hw_si )

    return(si)

  }

  # Interior Hemlock Hwi
  env_dave_functions$convert_hwi_si <- function(inp){
    si <- case_when(
      inp$fd_si > 0 ~ -5.140924460 + 1.127395720 * inp$fd_si ,
      inp$sw_si > 0 ~ 0.214205210 + 0.830890646 * inp$sw_si ,
      inp$sx_si > 0 ~ 0.214205210 + 0.830890646 * inp$sx_si ,
      inp$pl_si > 0 ~ -4.342264694 + 1.053640861 * inp$pl_si ,
      inp$lw_si > 0 ~ -5.918827507 + 1.108229993 * inp$lw_si ,
      TRUE ~ inp$hw_si)

    return(si)

  }

  # Larch Lw
  env_dave_functions$convert_lw_si <- function(inp){
    si <- case_when(
      inp$fd_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ 0.701932860 + 1.017294000 * inp$fd_si ,
      inp$bl_si > 0  ~ 1.425961536 + 1.048076921 * inp$bl_si ,
      inp$pl_si > 0 ~  1.923076920 + 0.961538460 * inp$pl_si ,
      inp$sw_si > 0 ~ 3.817307686 + 0.884615383 * inp$sw_si ,
      inp$sx_si > 0 ~ 3.817307686 + 0.884615383 * inp$sx_si ,
      inp$hw_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~  5.340793500 + 0.902339778 * inp$hw_si ,
      inp$sb_si > 0 ~ -2.239423073 + 1.505769228 * inp$sb_si ,
      TRUE ~ inp$lw_si )

    return(si)

  }

  # Pine Pli
  env_dave_functions$convert_pl_si <- function(inp){
    si <- case_when(
      inp$sw_si > 0 ~  1.970000000 + 0.920000000 * inp$sw_si ,
      inp$sx_si > 0 ~ 1.970000000 + 0.920000000 * inp$sx_si ,
      inp$hw_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~  4.121200000 + 0.949090000 * inp$hw_si ,
      inp$at_si > 0 ~ 5.469680000 + 0.733976000 * inp$at_si ,
      inp$bl_si > 0 ~ -0.517000000 + 1.090000000 * inp$bl_si ,
      inp$fd_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ -0.758000000 + 1.070000000 * inp$fd_si ,
      inp$lw_si > 0 ~ -2.000000000 + 1.040000000 * inp$lw_si ,
      inp$sb_si > 0 ~ -4.329000000 + 1.566000000 * inp$sb_si ,
      inp$ss_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ inp$sb_si ,  # assumed to be shore pine
      TRUE ~inp$pl_si )

    return(si)

  }

  env_dave_functions$convert_sb_si <- function(inp){
    si <- case_when(
      inp$pl_si > 0 ~ 2.764367820 + 0.638569600 * inp$pl_si ,
      inp$lw_si > 0 ~ 1.487228620 + 0.664112384 * inp$lw_si ,
      inp$fd_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ 2.280332063 + 0.683269472 * inp$fd_si ,
      inp$bl_si > 0 ~ 2.434227337 + 0.696040864 * inp$bl_si ,
      inp$sw_si > 0 ~ 4.022349932 + 0.587484032 * inp$sw_si ,
      inp$sx_si > 0 ~ 4.022349932 + 0.587484032 * inp$sx_si ,
      TRUE ~ inp$sb_si )

    return(si)

  }

  # Sitka Spruce Ss
  env_dave_functions$convert_ss_si <- function(inp){
    si <- case_when(
      inp$hw_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ -4.943820220 + 1.248439450 * inp$hw_si ,
      inp$ba_si > 0 ~ -2.440699123 + 1.265917602 * inp$ba_si ,
      inp$cw_si > 0 ~ -3.375780271 + 1.308364544 * inp$cw_si ,
      inp$fd_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ -5.483146062 + 1.122347066 * inp$fd_si ,
      TRUE ~ inp$ss_si )

    return(si)

  }

  # White Spruce Sw
  env_dave_functions$convert_sw_si <- function(inp){
    si <- case_when(
      inp$pl_si > 0 ~ -2.141304350 + 1.086956520 * inp$pl_si ,
      inp$at_si > 0 ~ 3.804000000 + 0.797800000 * inp$at_si ,
      inp$hw_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ -0.257801914 + 1.203527813 * inp$hw_si ,
      inp$bl_si > 0 ~ -1.953488370 + 1.162790700 * inp$bl_si ,
      inp$lw_si > 0 ~ -4.315217390 + 1.130434781 * inp$lw_si ,
      inp$fd_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ -6.445047490 + 1.356852100 * inp$fd_si ,
      inp$sb_si > 0 ~ -6.846739125 + 1.702173910 * inp$sb_si ,
      TRUE ~ inp$sw_si )

    return(si)

  }

  # Hybrid Coastal Spruce Sx
  env_dave_functions$convert_sxc_si <- function(inp){
    si <- case_when(
      inp$hw_si > 0 ~ -4.943820220 + 1.248439450 * inp$hw_si ,
      inp$ba_si > 0 ~ -2.440699123 + 1.265917602 * inp$ba_si ,
      inp$cw_si > 0 ~ -3.375780271 + 1.308364544 * inp$cw_si ,
      inp$fd_si > 0 ~ -5.483146062 + 1.122347066 * inp$fd_si ,
      TRUE ~ inp$sx_si )

    return(si)

  }

  # Hybrid  Spruce Sx
  env_dave_functions$convert_sx_si <- function(inp){
    si <- case_when(
      inp$pl_si > 0 ~ -2.141304350 + 1.086956520 * inp$pl_si ,
      inp$at_si > 0 ~ 3.804000000 + 0.797800000 * inp$at_si ,
      inp$hw_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ -0.257801914 + 1.203527813 * inp$hw_si ,
      inp$bl_si > 0 ~ -1.953488370 + 1.162790700 * inp$bl_si ,
      inp$lw_si > 0 ~ -4.315217390 + 1.130434781 * inp$lw_si ,
      inp$fd_si > 0 & !inp$bec_zone %in% c('CDF','CWH','MH') ~ -6.445047490 + 1.356852100 * inp$fd_si ,
      inp$sb_si > 0 ~ -6.846739125 + 1.702173910 * inp$sb_si ,
      TRUE ~ inp$sx_si )

    return(si)

  }

  ###########################################################################
  # one to one conversions
  # vetted by Gord Nigh


  # Spruce : Se & Sw

  env_dave_functions$convert_sw_from_se <- function(inp) {
    # use Se
    # sw/se/sx all interchangeable
    #if (inp$sw_si == 0)

    si <- case_when(
      inp$se_si > 0 ~ inp$se_si ,
      TRUE ~ inp$sw_si)

    return(si)

  }

  env_dave_functions$convert_sw_from_sx <- function(inp) {
    # use sx

    si <- case_when(
      inp$sx_si > 0 ~ inp$sx_si ,
      TRUE ~ inp$sw_si)

    return(si)

  }

  env_dave_functions$convert_ss_from_sx <- function(inp) {
    # use sx

    si <- case_when(
      inp$sx_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ inp$sx_si ,  # only convert for Coastal
      TRUE ~ inp$ss_si)

    return(si)

  }

  env_dave_functions$convert_se_from_sw <- function(inp) {
    si <- case_when(
      inp$sw_si > 0 ~ inp$sw_si ,
      TRUE ~ inp$se_si)

    return(si)
  }

  env_dave_functions$convert_sx_from_sw <- function(inp){ # note that orginal function name is convert_sx

    si <- case_when(
      inp$sw_si > 0 ~ inp$sw_si,
      TRUE ~ inp$sx_si)

    return(si)
  }



  ##########################################

  ## ba/bl/bg all interchangeable
  ## not in SIndex

  env_dave_functions$convert_ba <- function(inp){
    si <- case_when(
      inp$bg_si > 0 ~ inp$bg_si ,   # use BG first as Ba = Bg
      inp$bl_si > 0 ~ inp$bl_si ,   # use BL , but it will be much lower
      TRUE ~ inp$ba_si)

    return(si)
  }

  env_dave_functions$convert_bl <- function(inp) {
    si <- case_when(
      inp$ba_si > 0 ~ inp$ba_si,
      inp$bg_si > 0 ~ inp$bg_si,
      TRUE ~ inp$bl_si)

    return(si)
  }

  env_dave_functions$convert_bg <- function(inp) {

    si <- case_when(
      inp$ba_si > 0 ~ inp$ba_si ,
      TRUE ~ inp$bg_si)

    return(si)
  }


  #####################################
  # Pw Ss

  env_dave_functions$convert_pw <- function(inp){
    si <- case_when(
      inp$ss_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ inp$ss_si , # coastal
      inp$hw_si > 0 & inp$bec_zone %in% c('CDF','CWH','MH') ~ inp$hw_si , # coastal added to fill missings
      inp$sw_si >0 ~ inp$sw_si,
      TRUE ~ inp$pw_si)

    return(si)
  }

  env_dave_functions$convert_ss <- function(inp){

    si <- case_when(
      inp$pw_si > 0 ~ inp$pw_si,
      TRUE ~ inp$ss_si)

    return(si)
  }

  ##########################################

  # Lt /  Lw

  env_dave_functions$convert_lt <- function(inp){ # note that the original function name is convert_lt_from_lw
    ## lt/lw interchangeable

    si <- case_when(
      inp$lw_si > 0 ~ inp$lw_si ,
      TRUE ~ inp$lt_si )

    return(si)
  }

  env_dave_functions$convert_lw <- function(inp){


    si <- case_when(
      inp$lt_si > 0 ~ inp$lt_si,
      TRUE ~ inp$lw_si)

    return(si)
  }

  ###############################################################
  # hm/hw interchangeable

  env_dave_functions$convert_hm <- function(inp){
    si <- case_when(
      inp$hw_si > 0 ~  inp$hw_si,
      TRUE ~ inp$hm_si)

    return(si)
  }

  env_dave_functions$convert_hw <- function(inp){
    si <- case_when(
      inp$hm_si > 0 ~ inp$hm_si,
      TRUE ~ inp$hw_si)

    return(si)
  }

  ##########################################################
  # Pa / Pl

  env_dave_functions$convert_pa <- function(inp){
    # pa/pl interchangeable

    si <- case_when(
      inp$pl_si > 0 ~ inp$pl_si,
      TRUE ~ inp$pa_si)

    return(si)
  }

  env_dave_functions$convert_pl <- function(inp){
    si <- case_when(
      inp$pa_si > 0 ~ inp$pa_si,
      TRUE ~ inp$pl_si)

    return(si)
  }

  ###########################################################
  # alder conversion
  # dr is special
  # there are certain cases where Fd is missing, therefore use Hw

  env_dave_functions$convert_dr <- function(inp){
    si <- case_when(
      inp$fd_si > 0 & inp$bec_zone %in% c('CWH','CDF','MH') & inp$bec_subzone %in% c('ds','db','xm') ~ inp$fd_si * 0.55,
      inp$fd_si > 0 & inp$bec_zone %in% c('CWH','CDF','MH') ~ inp$fd_si * 0.73,
      inp$hw_si > 0 & inp$bec_zone %in% c('CWH','CDF','MH') ~ inp$hw_si * 0.73,
      TRUE ~ inp$fd_si *0.73
    )
  }

  ####################################################################
  # py is special
  # added, this is not in SIndex

  env_dave_functions$convert_py <- function(inp){
    si <- case_when(
      inp$fd_si > 0 ~ inp$fd_si ,
      TRUE ~ inp$py_si)

    return(si)
  }

  ##########################################################
  # Pw seems to be missing a lot

  env_dave_functions$convert_pw_from_fd <- function(inp){
    # pw/fd interchangeable (checking on this)
    si <- case_when(
      inp$fd_si > 0 ~ inp$fd_si,
      TRUE ~ inp$pw_si)

    return(si)
  }

  ##########################################################
  # Ss in CWH sometimes missing
  # since Ss can use Pw and Pw can use Fd
  # if Pw is missing, use Fd

  env_dave_functions$convert_ss_from_fd <- function(inp){
    # ss/fd interchangeable (checking on this)
    si <- case_when(
      inp$fd_si > 0 ~ inp$fd_si,
      TRUE ~ inp$ss_si)

    return(si)
  }
  return(env_dave_functions)
}
