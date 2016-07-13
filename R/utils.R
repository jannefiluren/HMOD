

#' Plotting model results
#'
#' @param data_obs Observed data (in format given by package NVEDATA)
#' @param data_mod Model data (in raw output format)
#' @param filename Output filename
#' @import ggplot2
#' @export

plotting_results <- function(data_obs, data_mod, filename) {

  summary_df = data.frame(Time = data_obs$time_vec,
                          Qobs = data_obs$Runoff,
                          Qsim = data_mod$Q,
                          St_1 = data_mod$St_all[ ,1],
                          St_2 = data_mod$St_all[ ,2],
                          SWE = rowMeans(data_mod$SWE_all))

  p1 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=Qobs), color="black") + geom_line(aes(y=Qsim), color="red") + ylab("Runoff") + theme_bw()

  p2 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=St_1), color="black") + geom_line(aes(y=St_2), color="red") + ylab("Storages") + theme_bw()

  p3 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=SWE), color="black") + ylab("SWE") + theme_bw()

  p <- gridExtra::grid.arrange(p1, p2, p3)

  ggsave(p, file = filename, width = 11, height = 10, dpi = 600)

}


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


