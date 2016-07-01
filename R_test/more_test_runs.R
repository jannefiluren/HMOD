

library(HMOD)

# Load data ---------------------------------------------------------------

load(file = "data_hbv.RData")


# Prepare data ------------------------------------------------------------

iwsh <- 3

Runoff <- (res[[iwsh]]$Runoff * 86400 * 1000) / (res[[iwsh]]$metadata$area_total * 1e6)

NTimes = nrow(res[[iwsh]]$Prec)
NZones = ncol(res[[iwsh]]$Prec)

indata = list(NTimes = NTimes,
              NZones = NZones,
              Prec   = res[[iwsh]]$Prec,
              Tair   = res[[iwsh]]$Tair,
              PET    = matrix(0, nrow=nrow(res[[iwsh]]$Prec), ncol=1),
              SWE    = matrix(0, nrow=1, ncol=NZones),
              St     = matrix(0, nrow=2, ncol=1),
              StUH1  = matrix(0, 20, ncol=1),
              StUH2  = matrix(0, 40, ncol=1))


# Calibrate model ---------------------------------------------------------

lparam = c(10,-8,10,0,1,0.7)
uparam = c(1000,8,500,5,7,1.8)
sparam = (lparam + uparam)/2

control = list(MinMax='max',write2disk=FALSE,verbose=FALSE)

evaldata = Runoff

res_calib = hydroPSO::hydroPSO(sparam,fn=calib_wrapper_model,indata, evaldata,
                               lower=lparam,upper=uparam,control=control)


# Run model ---------------------------------------------------------------

indata$Param <- unname(res_calib$par)

res_sim <- model_wrapper(indata)


# Plot results ------------------------------------------------------------

plot(evaldata, type = "l", col = "black", lwd = 2)
lines(res_sim$Q, col = "red")


# Run ensemble ------------------------------------------------------------

indata_vec <- vector("list",1000)

for (i in seq_along(indata_vec)) {
  indata_vec[[i]] <- indata
}

tstart <- Sys.time()
res_ens <- lapply(indata_vec,model_wrapper)
tstop <- Sys.time()

tstop - tstart





















