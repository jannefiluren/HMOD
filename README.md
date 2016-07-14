# HMOD

R package with a simple snow (temperature index) and hydrological model (GR4J).

## Installation

Package available upon request.

Note that for installation on windows the package requires Rtools (https://cran.r-project.org/bin/windows/Rtools/).

Build this package from source code ``` install.packages(path_to_file, repos = NULL, type = "source")```.

## Example how to run the model

```R

iwsh <- 3

data_obs <- sample_data[[iwsh]]

indata = list(Time           = data_obs$time_vec,
              Prec           = data_obs$Prec,
              Tair           = data_obs$Tair,
              PET            = rep(0, nrow(data_obs$Prec)),
              SWE            = matrix(0, nrow = 1, ncol = ncol(data_obs$Prec)),
              St             = matrix(0, nrow = 2, ncol = 1),
              StUH1          = matrix(0, 20, ncol = 1),
              StUH2          = matrix(0, 40, ncol = 1),
              Param          = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02),
              frac_elev_band = data_obs$frac_elev_band)

data_sim <- model_wrapper(indata)

plot_interactive(data_obs, data_sim)

```

## Example how to calibrate the model

```R

# Prepare data ------------------------------------------------------------

iwsh <- 3

data_obs <- sample_data[[iwsh]]

NTimes <- nrow(data_obs$Prec)
NZones <- ncol(data_obs$Prec)

indata = list(Time           = data_obs$time_vec,
              Prec           = data_obs$Prec,
              Tair           = data_obs$Tair,
              PET            = rep(0, nrow(data_obs$Prec)),
              SWE            = matrix(0, nrow = 1, ncol = ncol(data_obs$Prec)),
              St             = matrix(0, nrow = 2, ncol = 1),
              StUH1          = matrix(0, 20, ncol = 1),
              StUH2          = matrix(0, 40, ncol = 1),
              frac_elev_band = data_obs$frac_elev_band)

# Calibrate model ---------------------------------------------------------

lparam <- c(10, -8, 10, 0, 1, 0.7)
uparam <- c(1000, 8, 500, 5, 7, 1.8)
sparam <- (lparam + uparam)/2

control <- list(MinMax = "max", write2disk = FALSE, verbose = FALSE)

evaldata <- data_obs$Runoff

res_calib <- hydroPSO::hydroPSO(sparam, fn = calib_wrapper_model, indata, evaldata,
                                lower = lparam, upper = uparam, control = control)

# Run model with optimal parameter values ---------------------------------

indata$Param <- unname(res_calib$par)

res_sim <- model_wrapper(indata)

# Plot results ------------------------------------------------------------

plot_interactive(data_obs, data_sim)

```
