

#' Run ensemble for one watershed
#'
#' @examples
#'
#' NEns <- 10
#'
#' iwsh <- 3
#'
#' data_obs <- sample_data[[iwsh]]
#'
#' param <- c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02)
#'
#' Qsim <- run_ensemble(data_obs, param, NEns)
#'
#' plot(rowMeans(Qsim),type = "l")
#' lines(apply(Qsim,1,min))
#' lines(apply(Qsim,1,max))
#'
#' @import compiler
#' @export


run_ensemble <- function(data_obs, param, NEns) {

  # Initlize model variables

  init_model <- function(model_data, data_obs, param) {

    indata = list(SWE            = rep(0, length(data_obs$frac_elev_band)),
                  St             = rep(0, 2),
                  StUH1          = rep(0, 20),
                  StUH2          = rep(0, 40),
                  Param          = param,
                  frac_elev_band = data_obs$frac_elev_band)

    return(indata)

  }

  # Add input data

  assign_indata <- function(model_data, itime) {

    model_data$Time  = data_obs$time_vec[itime]
    model_data$Prec  = data_obs$Prec[itime, ] # * runif(1, min = 0.5, max = 1.5)
    model_data$Tair  = data_obs$Tair[itime, ] # + rnorm(1, sd = 2)
    model_data$PET   = 0

    return(model_data)

  }

  # Compile functions fora small speed gain

  init_model <- cmpfun(init_model)
  assign_indata <- cmpfun(assign_indata)
  model_wrapper_ens <- cmpfun(model_wrapper_ens)

  # Initilize model

  data_sim  <- vector("list",NEns)

  data_sim <- lapply(data_sim, init_model, data_obs, param = param)

  # Allocate output arrays

  Q <- matrix(0, nrow = length(data_obs$time_vec), ncol = NEns)

  # Run model

  for (itime in seq_along(data_obs$time_vec)) {

    # Add input data

    data_sim <- lapply(data_sim, assign_indata, itime)

    # Run model


    data_sim <- lapply(data_sim, model_wrapper_ens)

    # Save outputs

    Q[itime, ] <- unlist(lapply(data_sim, function(x) { x$Q } ))

  }

  return(Q)

}





