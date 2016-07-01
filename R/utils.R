

#' Calibration wrapper for hydrological model (GR4J)
#'
#' Wrapper for running GR4J and computing performance measures
#'
#' @param Param Vector with parameter values
#' @param indata List with required inputs (excluding Param)
#' @param evaldata Runoff data for validation
#' @return Goodness of fit measure
#' @export

calib_wrapper_gr4j = function(Param,indata,evaldata) {

  # Run model

  indata <- c(indata, list(Param = Param))

  res <- gr4j_wrapper(indata)

  # Compute performance measure

  sim <- as.numeric(res$Q)
  obs <- as.numeric(evaldata)

  sim <- sim[366:length(sim)]
  obs <- obs[366:length(obs)]

  return(hydroGOF::NSE(sim, obs))

}


#' Calibration wrapper for snow and hydrological model
#'
#' Wrapper for running snow module and GR4J and computing performance measures
#'
#' @param Param Vector with parameter values
#' @param indata List with required inputs (excluding Param)
#' @param evaldata Runoff data for validation
#' @return Goodness of fit measure
#' @export

calib_wrapper_model = function(Param,indata,evaldata) {

  # Run model

  indata <- c(indata, list(Param = Param))

  res <- model_wrapper(indata)

  # Compute performance measure

  sim <- as.numeric(res$Q)
  obs <- as.numeric(evaldata)

  sim <- sim[366:length(sim)]
  obs <- obs[366:length(obs)]

  return(hydroGOF::NSE(sim, obs))

}


