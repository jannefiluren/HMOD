

# Prepare data ------------------------------------------------------------

iwsh <- 3

NTimes <- nrow(sample_data[[iwsh]]$Prec)
NZones <- ncol(sample_data[[iwsh]]$Prec)

indata <- list(Prec = sample_data[[iwsh]]$Prec,
               Tair = sample_data[[iwsh]]$Tair,
               PET = rep(0, NTimes),
               SWE = matrix(0, nrow = 1, ncol = NZones),
               St = matrix(0,nrow = 2, ncol = 1),
               StUH1 = matrix(0, 20, ncol = 1),
               StUH2 = matrix(0, 40, ncol = 1))

# Calibrate model ---------------------------------------------------------

lparam <- c(10, -8, 10, 0, 1, 0.7)
uparam <- c(1000, 8, 500, 5, 7, 1.8)
sparam <- (lparam + uparam)/2

control <- list(MinMax = "max", write2disk = FALSE, verbose = FALSE)

evaldata <- sample_data[[iwsh]]$Runoff

res_calib <- hydroPSO::hydroPSO(sparam, fn = calib_wrapper_model, indata, evaldata,
                                lower = lparam, upper = uparam, control = control)

# Run model with optimal parameter values ---------------------------------

indata$Param <- unname(res_calib$par)

res_sim <- model_wrapper(indata)

# Plot results ------------------------------------------------------------

plot(evaldata, type = "l", col = "black", lwd = 2)
lines(res_sim$Q, col = "red")

