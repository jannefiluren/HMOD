#' Test hydrological model
#'
#' Run GR4J and check against results from airGR package
#'
#' @export

gr4j_test_run = function() {

  # Create list for running GR4J

  InputsPrecip <- data_airgr$InputsPrecip
  InputsPE <- data_airgr$InputsPET

  St <- matrix(0, nrow = 2, ncol = 1)
  StUH1 <- matrix(0, nrow = 20, ncol = 1)
  StUH2 <- matrix(0, nrow = 40, ncol = 1)

  Param <- data_airgr$Param

  St <- c(0.3 * Param[1], 0.5 * Param[3])

  data_run <- list(Prec = InputsPrecip,
                   PET = InputsPE,
                   St = St,
                   StUH1 = StUH1,
                   StUH2 = StUH2,
                   Param = Param)

  # Run GR4J

  newres <- gr4j_wrapper(data_run)

  # Plot results and print maximum error

  plot(newres$Q)
  lines(data_airgr$SimRunoff)

  max_error <- max(abs(data_airgr$SimRunoff - newres$Q), na.rm = TRUE)

  cat(paste("Max error: ", max_error, sep = ""))

}


#' Test calibration of hydrological model
#'
#' Calibrate GR4J and check against results from airGR package
#'
#' @export

gr4j_test_calib = function() {

  # Prepare data

  indata <- list(Prec = data_airgr$InputsPrecip,
                 PET = data_airgr$InputsPET,
                 St = matrix(0, nrow = 2, ncol = 1),
                 StUH1 = matrix(0, 20, ncol = 1),
                 StUH2 = matrix(0, 40, ncol = 1))

  # Calibration settings

  sparam <- c(500, 0, 200, 1)
  lparam <- c(10, -8, 10, 0)
  uparam <- c(1000, 6, 500, 5)

  control <- list(MinMax = "max", write2disk = FALSE, verbose = FALSE)

  # Run calibration

  evaldata <- data_airgr$ObsRunoff

  res_calib <- hydroPSO::hydroPSO(sparam, fn = calib_wrapper_gr4j, indata, evaldata,
                                  lower = lparam, upper = uparam, control = control)

  # Compare with results from airGR package

  print(res_calib$par[1]/data_airgr$Param[1])
  print(res_calib$par[2]/data_airgr$Param[2])
  print(res_calib$par[3]/data_airgr$Param[3])
  print(res_calib$par[4]/data_airgr$Param[4])

  print(res_calib$value)

}



#' Test calibration of snow and hydrological model
#'
#' Calibrate the models and check results against observations
#'
#' @export

model_test_calib = function() {

  # Prepare model and input data

  indata <- list(Time = data_storholen$DatesR,
                 Prec = data_storholen$Prec,
                 Tair = data_storholen$Tair,
                 PET = rep(0, nrow(data_storholen$Prec)),
                 SWE = matrix(0, nrow = 1, ncol = ncol(data_storholen$Prec)),
                 St = matrix(0,  nrow = 2, ncol = 1),
                 StUH1 = matrix(0, 20, ncol = 1),
                 StUH2 = matrix(0, 40, ncol = 1),
                 frac_elev_band = rep(1/8, 8))

  # Calibration settings

  lparam <- c(10, -8, 10, 0, 1, 0.7)
  uparam <- c(1000, 8, 500, 5, 7, 1.8)
  sparam <- (lparam + uparam)/2

  control <- list(MinMax = "max", write2disk = FALSE, verbose = FALSE)

  # Run calibration

  evaldata <- data_storholen$Qobs

  res_calib <- hydroPSO::hydroPSO(sparam, fn = calib_wrapper_model, indata, evaldata,
                                  lower = lparam, upper = uparam, control = control)

  # Print results

  print(res_calib$value)

}




