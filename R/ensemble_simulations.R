library(profvis)


init_model <- function(model_vec, data_obs) {

  indata = list(SWE            = matrix(0, nrow = 1, ncol = length(data_obs$frac_elev_band)),
                St             = matrix(0, nrow = 2, ncol = 1),
                StUH1          = matrix(0, 20, ncol = 1),
                StUH2          = matrix(0, 40, ncol = 1),
                frac_elev_band = data_obs$frac_elev_band)

  return(indata)

}

assign_indata <- function(model_vec, itime) {

  NTimes = 1
  NZones = ncol(data_obs$Prec)

  model_vec$Time  = data_obs$time_vec[itime]
  model_vec$Prec  = matrix(data_obs$Prec[itime, ], nrow = NTimes, ncol = NZones)
  model_vec$Tair  = matrix(data_obs$Tair[itime, ], nrow = NTimes, ncol = NZones)
  model_vec$PET   = 0
  model_vec$Param = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02)

  model_vec$frac_elev_band = data_obs$frac_elev_band

  return(model_vec)

}

# Select watershed

iwsh <- 3

data_obs <- sample_data[[iwsh]]

# Initilize model

data_sim  <- vector("list",500)

data_sim <- lapply(data_sim, init_model, data_obs)

# Init output data

Q <- c()

# Run model

t1 <- Sys.time()

for (itime in seq_along(data_obs$time_vec)) {


  # cat(paste("Time step ",itime,"\n",sep = ""))


  # Add input data

  data_sim <- lapply(data_sim, assign_indata, itime)

  # Run model

  data_sim <- lapply(data_sim, model_wrapper)

  # Save outputs

  Q <- rbind(Q,unlist(lapply(data_sim, function(x) x$Q)))

}

t2 <- Sys.time()

t2 - t1

