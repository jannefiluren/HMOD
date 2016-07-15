#' Wrapper for running the model for filter algorithms
#'
#'
#' @useDynLib HMOD
#' @export


model_wrapper_ens <- function(indata) {

  # Settings

  Pos_param_snow <- 5
  Pos_param_rof <- c(1, 2, 3, 4)

  # Precipitation correction factor

  indata$Prec <- indata$Param[6] * indata$Prec

  # Dimensions of input data

  NTimes <- 1
  NZones <- length(indata$Prec)

  # Run snow model

  MeltRain_all <- rep(0,NZones)

  res_snow <- .Fortran("snow_wrapper_ens",
                       NTimes       = as.integer(NTimes),
                       NZones       = as.integer(NZones),
                       Prec         = as.double(indata$Prec),
                       Tair         = as.double(indata$Tair),
                       SWE          = as.double(indata$SWE),
                       Param        = as.double(indata$Param[Pos_param_snow]),
                       MeltRain_all = as.double(MeltRain_all),
                       PACKAGE      = "HMOD")

  # Compute average melt and rain flux

  MeltRain_mean <- sum(res_snow$MeltRain_all * indata$frac_elev_band)

  # Run runoff model

  Q_all <- 0

  res_hyd <- .Fortran("gr4j_wrapper_ens",
                      NTimes  = as.integer(NTimes),
                      Prec    = as.double(MeltRain_mean),
                      PET     = as.double(indata$PET),
                      St      = as.double(indata$St),
                      StUH1   = as.double(indata$StUH1),
                      StUH2   = as.double(indata$StUH2),
                      Q_all   = as.double(Q_all),
                      Param   = as.double(indata$Param[Pos_param_rof]),
                      PACKAGE = "HMOD")

  # Function outputs

  indata$SWE   = res_snow$SWE
  indata$St    = res_hyd$St
  indata$StUH1 = res_hyd$StUH1
  indata$StUH2 = res_hyd$StUH2
  indata$Q     = res_hyd$Q_all

  return(indata)

#   res <- list(Time = indata$Time,
#               SWE = res_snow$SWE,
#               St = res_hyd$St,
#               StUH1 = res_hyd$StUH1,
#               StUH2 = res_hyd$StUH2,
#               Q = res_hyd$Q_all)

  # return(res)

}

