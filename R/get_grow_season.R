# to get growing season, an equivalent function to grow_sn_orcl
# input is the measurement date formatted as (YYYY-MM-DD)
# growingStart is the first day of growing season for a given year, the default is May 01
# growingEnd is the last day of growing season for a given year, the default is Sep 31
# output is the numeric growing season information.

get_grow_season <- function(measurementDate,
                            growingStart = "05-01",
                            growingEnd = "08-31"){
  measurementDatetable <- data.table::data.table(MEAS_DT = as.Date(measurementDate))
  measurementDatetable[,':='(GS_YY = as.numeric(substr(MEAS_DT, 1, 4)),
                             GS_MM = as.numeric(substr(MEAS_DT, 6, 7)),
                             GS_DD = as.numeric(substr(MEAS_DT, 9, 10)))]

  measurementDatetable[, GS_MSDT := paste0(sprintf("%04d", GS_YY),
                                           sprintf("%02d", GS_MM),
                                           sprintf("%02d", GS_DD))]
  measurementDatetable[GS_MM >= 9, GS_NN := 0]
  measurementDatetable[GS_MM <= 4, ':='(GS_YY = GS_YY - 1,
                                        GS_NN = 0)]
  measurementDatetable[is.na(GS_NN), ':='(GS_ST = as.Date(paste0(GS_YY, "-", growingStart))-1, # growing season starts at May 01
                                          GS_ED = as.Date(paste0(GS_YY, "-", growingEnd))+1)] # growing season ends at August 31
  measurementDatetable[is.na(GS_NN), GS_YY := GS_YY - 1]
  measurementDatetable[is.na(GS_NN), GS_NN := round(100*(as.numeric(MEAS_DT - GS_ST)/as.numeric(GS_ED - GS_ST)))]
  measurementDatetable[is.na(GS_NN), GS_NN := 0]
  measurementDatetable[, GROW_SN := as.numeric(paste0(GS_YY, ".", sprintf("%02d", GS_NN)))]
  return(measurementDatetable$GROW_SN)
}


