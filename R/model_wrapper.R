#' Wrapper for running snow and hydrological model
#'
#' The snow model is a simple temperature index model.
#' The hydrological model is the GR4J model.
#'
#' @param indata A list with the following items:
#'
#' \itemize{
#'   \item \code{Prec} Matrix with precipitation, dimensions Ntimes * NZones
#'   \item \code{Tair} Matrix with air temperature, dimensions Ntimes * NZones
#'   \item \code{PET} Vector with potential evapotranspiration, dimensions Ntimes
#'   \item \code{SWE} Initial state of snow water equivalent
#'   \item \code{St} Initial state of storages
#'   \item \code{StUH1} Initial state of first unit hydrograph
#'   \item \code{StUH2} Initial state of second unit hydrograph
#'   \item \code{Param} Model parameters
#'   \item \code{frac_elev_band} Fraction of watershed area for each elevation band
#' }
#'
#' @examples
#'
#' iwsh <- 3
#'
#' indata = list(Prec           = sample_data[[iwsh]]$Prec,
#'               Tair           = sample_data[[iwsh]]$Tair,
#'               PET            = rep(0, nrow(sample_data[[iwsh]]$Prec)),
#'               SWE            = matrix(0, nrow = 1, ncol = ncol(sample_data[[iwsh]]$Prec)),
#'               St             = matrix(0, nrow = 2, ncol = 1),
#'               StUH1          = matrix(0, 20, ncol = 1),
#'               StUH2          = matrix(0, 40, ncol = 1),
#'               Param          = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02),
#'               frac_elev_band = sample_data[[iwsh]]$frac_elev_band)
#'
#' res_sim <- model_wrapper(indata)
#'
#' plot(sample_data[[iwsh]]$Runoff, type = 'l', col = 'black', lwd = 2)
#' lines(res_sim$Q, col = 'red')
#'
#' @return A list with state variables and simulated runoff
#' @export

model_wrapper <- function(indata) {

  # Settings

  Pos_param_snow <- 5
  Pos_param_rof <- c(1, 2, 3, 4)

  # Precipitation correction factor

  indata$Prec <- indata$Param[6] * indata$Prec

  # Test inputs

  if (!"Prec" %in% names(indata))
    stop("Prec missing as input")
  if (!"Tair" %in% names(indata))
    stop("Tair missing as input")
  if (!"PET" %in% names(indata))
    stop("PET missing as input")
  if (!"SWE" %in% names(indata))
    stop("SWE missing as input")
  if (!"St" %in% names(indata))
    stop("St missing as input")
  if (!"StUH1" %in% names(indata))
    stop("StUH1 missing as input")
  if (!"StUH2" %in% names(indata))
    stop("StUH2 missing as input")
  if (!"Param" %in% names(indata))
    stop("Param missing as input")
  if (!"frac_elev_band" %in% names(indata))
    stop("frac_elev_band missing as input")

  # Dimensions of input data

  NTimes <- nrow(indata$Prec)
  NZones <- ncol(indata$Prec)

  # Run snow model

  insnow <- list(Prec = indata$Prec,
                 Tair = indata$Tair,
                 SWE = indata$SWE,
                 Param = indata$Param[Pos_param_snow])

  res_snow <- snow_wrapper(insnow)

  # Compute average melt and rain flux

  MeltRain_mean <- rowSums(res_snow$MeltRain_all *
                             matrix(indata$frac_elev_band, nrow = NTimes, ncol = NZones, byrow = TRUE))

  # Run runoff model

  inrof <- list(Prec = MeltRain_mean,
                PET = indata$PET,
                St = indata$St,
                StUH1 = indata$StUH1,
                StUH2 = indata$StUH2,
                Param = indata$Param[Pos_param_rof])

  res_hyd <- gr4j_wrapper(inrof)

  # Function outputs

  res <- list(SWE_all = res_snow$SWE_all,
              St = res_hyd$St,
              StUH1 = res_hyd$StUH1,
              StUH2 = res_hyd$StUH2,
              Q = res_hyd$Q,
              St_all = res_hyd$St_all)

  return(res)

}


#' Wrapper for running snow model
#'
#' The snow model is a simple temperature index model.
#'
#' @param indata A list with the following items:
#'
#' \itemize{
#'   \item \code{Prec} Matrix with precipitation, dimensions Ntimes * NZones
#'   \item \code{Tair} Matrix with air temperature, dimensions Ntimes * NZones
#'   \item \code{SWE} Initial state of snow water equivalent
#'   \item \code{Param} Model parameters
#' }
#'
#' @examples
#'
#' iwsh <- 3
#'
#' indata = list(Prec   = sample_data[[iwsh]]$Prec,
#'               Tair   = sample_data[[iwsh]]$Tair,
#'               SWE    = matrix(0, nrow = 1, ncol = ncol(sample_data[[iwsh]]$Prec)),
#'               Param  = 3)
#'
#' res_sim <- snow_wrapper(indata)
#'
#' @return Simulated snow water equivalents
#' @useDynLib HMOD
#' @export

snow_wrapper <- function(insnow) {

  # Test inputs

  if (!"Prec" %in% names(insnow))
    stop("Prec missing as input")
  if (!"Tair" %in% names(insnow))
    stop("Tair missing as input")
  if (!"SWE" %in% names(insnow))
    stop("SWE missing as input")
  if (!"Param" %in% names(insnow))
    stop("Param missing as input")

  NTimes <- nrow(insnow$Prec)
  NZones <- ncol(insnow$Prec)

  if (!identical(dim(insnow$Prec), dim(insnow$Tair)))
    stop("Prec and Tair has different dimensions")
  if (length(insnow$SWE) != NZones)
    stop("SWE has wrong dimensions")
  if (length(insnow$Param) != 1)
    stop("Param has wrong dimensions")

  # Allocate output arrays

  MeltRain_all = matrix(0, nrow = NTimes, ncol = NZones)
  SWE_all = matrix(0, nrow = NTimes, ncol = NZones)

  # Run model

  RES_TMP <- .Fortran("snow_wrapper",
                      NTimes = as.integer(NTimes),
                      NZones = as.integer(NZones),
                      Prec = as.double(insnow$Prec),
                      Tair = as.double(insnow$Tair),
                      SWE = as.double(insnow$SWE),
                      Param = as.double(insnow$Param),
                      MeltRain_all = as.double(MeltRain_all),
                      SWE_all = as.double(SWE_all),
                      PACKAGE = "HMOD")

  # Process outputs

  RES_OUT <- list(MeltRain_all = matrix(RES_TMP$MeltRain_all, ncol = NZones, byrow = FALSE),
                  SWE_all = matrix(RES_TMP$SWE_all, ncol = NZones, byrow = FALSE))

  return(RES_OUT)

}


#' Wrapper for running hydrological model
#'
#' The hydrological model is the GR4J model.
#'
#' @param indata A list with the following items:
#'
#' \itemize{
#'   \item \code{Prec} Vector with precipitation, dimensions Ntimes
#'   \item \code{PET} Vector with potential evapotranspiration, dimensions Ntimes
#'   \item \code{St} Initial state of storages
#'   \item \code{StUH1} Initial state of first unit hydrograph
#'   \item \code{StUH2} Initial state of second unit hydrograph
#'   \item \code{Param} Model parameters
#' }
#'
#' @examples
#'
#' iwsh <- 3
#'
#' indata = list(Prec   = rowMeans(sample_data[[iwsh]]$Prec),
#'               PET    = rep(0, nrow(sample_data[[iwsh]]$Prec)),
#'               St     = matrix(0, nrow = 2, ncol = 1),
#'               StUH1  = matrix(0, 20, ncol = 1),
#'               StUH2  = matrix(0, 40, ncol = 1),
#'               Param  = c(74.59, 0.81, 214.98, 1.24))
#'
#' res_sim <- gr4j_wrapper(indata)
#'
#' plot(res_sim$Q, col = 'red', type ="l")
#'
#' @return A list with state variables and simulated runoff
#' @useDynLib HMOD
#' @export

gr4j_wrapper <- function(indata) {

  # Test inputs

  if (!"Prec" %in% names(indata))
    stop("Prec missing as input")
  if (!"PET" %in% names(indata))
    stop("PET missing as input")
  if (!"St" %in% names(indata))
    stop("St missing as input")
  if (!"StUH1" %in% names(indata))
    stop("StUH1 missing as input")
  if (!"StUH2" %in% names(indata))
    stop("StUH2 missing as input")
  if (!"Param" %in% names(indata))
    stop("Param missing as input")

  if (!identical(dim(indata$Prec), dim(indata$PET)))
    stop("Prec and PET has different dimensions")
  if (length(indata$St) != 2)
    stop("St has wrong dimensions")
  if (length(indata$StUH1) != 20)
    stop("StUH1 has wrong dimensions")
  if (length(indata$StUH2) != 40)
    stop("StUH2 has wrong dimensions")
  if (length(indata$Param) != 4)
    stop("Param has wrong dimensions")

  # Allocate output arrays

  NTimes <- length(indata$Prec)

  Q_all <- matrix(0, nrow = NTimes, ncol = 1)
  St_all <- matrix(0, nrow = NTimes, ncol = 2)
  StUH1_all <- matrix(0, nrow = NTimes, ncol = 1)
  StUH2_all <- matrix(0, nrow = NTimes, ncol = 1)

  # Run model

  RES_TMP <- .Fortran("gr4j_wrapper",
                      NTimes = as.integer(NTimes),
                      Prec = as.double(indata$Prec),
                      PET = as.double(indata$PET),
                      St = as.double(indata$St),
                      StUH1 = as.double(indata$StUH1),
                      StUH2 = as.double(indata$StUH2),
                      Q_all = as.double(Q_all),
                      St_all = as.double(St_all),
                      Param = as.double(indata$Param),
                      PACKAGE = "HMOD")

  # Process outputs

  RES_OUT <- list(St = RES_TMP$St,
                  StUH1 = RES_TMP$StUH1,
                  StUH2 = RES_TMP$StUH2,
                  Q = RES_TMP$Q_all,
                  St_all = matrix(RES_TMP$St_all, ncol = 2, byrow = FALSE))

  return(RES_OUT)

}
