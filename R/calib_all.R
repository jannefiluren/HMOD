


#' Calibrate the model for all watersheds
#'
#' @param data_obs List with input and evaluation data for a set of watersheds
#' @export


calib_em_all <- function(data_obs) {

  # Prepare data for simulations

  prepare_sim_data <- function(data_obs) {

    NTimes <- nrow(data_obs$Prec)
    NZones <- ncol(data_obs$Prec)

    indata = list(Time           = data_obs$time_vec,
                  Prec           = data_obs$Prec,
                  Tair           = data_obs$Tair,
                  PET            = rep(0, NTimes),
                  SWE            = matrix(0, nrow = 1, ncol = NZones),
                  St             = matrix(0, nrow = 2, ncol = 1),
                  StUH1          = matrix(0, 20, ncol = 1),
                  StUH2          = matrix(0, 40, ncol = 1),
                  frac_elev_band = data_obs$frac_elev_band,
                  Runoff         = data_obs$Runoff)

    return(indata)

  }

  # Function for calibrating one watershed

  calib_single_wsh <- function(indata) {

    lparam <- c(10, -8, 10, 0, 1, 0.7)
    uparam <- c(1000, 8, 500, 5, 7, 1.8)
    sparam <- (lparam + uparam)/2

    control <- list(MinMax = "max", write2disk = FALSE, verbose = FALSE)

    evaldata <- indata$Runoff

    res_calib <- hydroPSO::hydroPSO(sparam, fn = calib_wrapper_model, indata, evaldata,
                                    lower = lparam, upper = uparam, control = control)

    return(res_calib)

  }

  # Run the model using optimal parameters

  run_optimal <- function(indata, res_calib) {

    indata$Param <- unname(res_calib$par)

    res_sim <- model_wrapper(indata)

  }

  # Run the calibration for all watersheds and plot the results

  indata <- lapply(sample_data, prepare_sim_data)

  res_calib <- lapply(indata, calib_single_wsh)

  res_sim <- Map(run_optimal, indata, res_calib)

  Map(plotting_results, sample_data, res_sim, MoreArgs = list(path = "C:/Users/jmg/Desktop/calib_res"))

}








