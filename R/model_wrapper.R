#' Wrapper for running models
#'
#' @param Some inputs
#' @return Some outputs
#' @export

model_wrapper = function(indata) {

  # Settings

  Pos_param_snow = 5
  Pos_param_rof = c(1,2,3,4)


  # Precipitation correction factor

  indata$Prec = indata$Param[6] * indata$Prec


  # Test inputs

  if(!"NTimes" %in% names(indata)) stop("NTimes missing as input")
  if(!"NZones" %in% names(indata)) stop("NZones missing as input")
  if(!"Prec" %in% names(indata)) stop("Prec missing as input")
  if(!"Tair" %in% names(indata)) stop("Tair missing as input")
  if(!"PET" %in% names(indata)) stop("PET missing as input")
  if(!"SWE" %in% names(indata)) stop("SWE missing as input")
  if(!"Param" %in% names(indata)) stop("Param missing as input")
  if(!"St" %in% names(indata)) stop("St missing as input")
  if(!"StUH1" %in% names(indata)) stop("StUH1 missing as input")
  if(!"StUH2" %in% names(indata)) stop("StUH2 missing as input")

  # Run snow model

  insnow = list(NTimes = indata$NTimes,
                NZones = indata$NZones,
                Prec = indata$Prec,
                Tair = indata$Tair,
                SWE = indata$SWE,
                Param = indata$Param[Pos_param_snow],
                MeltRain_all = matrix(0, nrow=indata$NTimes, ncol=indata$NZones),
                SWE_all = matrix(0, nrow=indata$NTimes, ncol=indata$NZones))

  res_snow = snow_wrapper(insnow)

  # Run runoff model

  inrof = list(NTimes = indata$NTimes,
               Prec = rowMeans(res_snow$MeltRain_all),
               PET = indata$PET,
               St = indata$St,
               StUH1 = indata$StUH1,
               StUH2 = indata$StUH2,
               Q = matrix(0, nrow=NTimes, ncol=1),
               Param = indata$Param[Pos_param_rof])

  res_hyd = gr4j_wrapper(inrof)

  # Function outputs

  res = list(SWE_all = res_snow$SWE_all,
             St = res_hyd$St,
             StUH1 = res_hyd$StUH1,
             StUH2 = res_hyd$StUH2,
             Q = res_hyd$Q)

  return(res)

}


#' Wrapper for snow model
#'
#' @param Some inputs
#' @return Some outputs
#' @useDynLib HMOD
#' @export


snow_wrapper = function(insnow) {

#   # Load shared library
#
#   if(!is.loaded("snow_wrapper")) {
#     dyn.load("snow_wrapper.so")
#   }

  # Test inputs

  if(!"NTimes" %in% names(insnow)) stop("NTimes missing as input")
  if(!"NZones" %in% names(insnow)) stop("NZones missing as input")
  if(!"Prec" %in% names(insnow)) stop("Prec missing as input")
  if(!"Tair" %in% names(insnow)) stop("Tair missing as input")
  if(!"SWE" %in% names(insnow)) stop("SWE missing as input")
  if(!"Param" %in% names(insnow)) stop("Param missing as input")
  if(!"MeltRain_all" %in% names(insnow)) stop("MeltRain_all missing as input")
  if(!"SWE_all" %in% names(insnow)) stop("SWE_all missing as input")

  if(length(insnow$Prec) != insnow$NTimes*insnow$NZones) stop("Prec has wrong dimensions")
  if(length(insnow$Tair) != insnow$NTimes*insnow$NZones) stop("Tair has wrong dimensions")
  if(length(insnow$SWE) != insnow$NZones) stop("SWE has wrong dimensions")
  if(length(insnow$Param) != 1) stop("Param has wrong dimensions")
  if(length(insnow$MeltRain_all) != insnow$NTimes*insnow$NZones) stop("MeltRain_all has wrong dimensions")
  if(length(insnow$SWE_all) != insnow$NTimes*insnow$NZones) stop("SWE_all has wrong dimensions")

  # Run model

  RES_TMP <- .Fortran("snow_wrapper",
                      NTimes = as.integer(insnow$NTimes),
                      NZones = as.integer(insnow$NZones),
                      Prec = as.double(insnow$Prec),
                      Tair = as.double(insnow$Tair),
                      SWE = as.double(insnow$SWE),
                      Param = as.double(insnow$Param),
                      MeltRain_all = as.double(insnow$MeltRain_all),
                      SWE_all = as.double(insnow$SWE_all),
                      PACKAGE = "HMOD")

  # Process outputs

  RES_OUT = list(MeltRain_all = matrix(RES_TMP$MeltRain_all, ncol=insnow$NZones, byrow=FALSE),
                 SWE_all = matrix(RES_TMP$SWE_all,ncol=insnow$NZones, byrow=FALSE))

  return(RES_OUT)

}



#' Wrapper for GR4J model
#'
#' @param Some inputs
#' @return Some outputs
#' @useDynLib HMOD
#' @export

gr4j_wrapper = function(indata) {

#   # Load shared library
#
#   if(!is.loaded("gr4j_wrapper")) {
#     dyn.load("gr4j_wrapper.so")
#   }

  # Test inputs

  if(!"NTimes" %in% names(indata)) stop("NTimes missing as input")
  if(!"Prec" %in% names(indata)) stop("Prec missing as input")
  if(!"PET" %in% names(indata)) stop("PET missing as input")
  if(!"St" %in% names(indata)) stop("St missing as input")
  if(!"StUH1" %in% names(indata)) stop("StUH1 missing as input")
  if(!"StUH2" %in% names(indata)) stop("StUH2 missing as input")
  if(!"Q" %in% names(indata)) stop("Q missing as input")
  if(!"Param" %in% names(indata)) stop("Param missing as input")

  if(length(indata$Prec) != indata$NTimes) stop("Prec has wrong dimensions")
  if(length(indata$PET) != indata$NTimes) stop("PET has wrong dimensions")
  if(length(indata$St) != 2) stop("St has wrong dimensions")
  if(length(indata$StUH1) != 20) stop("StUH1 has wrong dimensions")
  if(length(indata$StUH2) != 40) stop("StUH2 has wrong dimensions")
  if(length(indata$Q) != indata$NTimes) stop("Q has wrong dimensions")
  if(length(indata$Param) != 4) stop("Param has wrong dimensions")

  # Run model

  RES_TMP <- .Fortran("gr4j_wrapper",
                      NTimes = as.integer(indata$NTimes),
                      Prec = as.double(indata$Prec),
                      PET = as.double(indata$PET),
                      St = as.double(indata$St),
                      StUH1 = as.double(indata$StUH1),
                      StUH2 = as.double(indata$StUH2),
                      Q = as.double(indata$Q),
                      Param = as.double(indata$Param),
                      PACKAGE = "HMOD")

  # Process outputs

  RES_OUT = list(St = RES_TMP$St,
                 StUH1 = RES_TMP$StUH1,
                 StUH2 = RES_TMP$StUH2,
                 Q = RES_TMP$Q)

  return(RES_OUT)

}
