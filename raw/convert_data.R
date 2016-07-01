
# Download sample data ----------------------------------------------------

library(NVEDATA)
library(lubridate)

metadata <- get_metadata()
metadata <- metadata[metadata$br23_HBV == "Y", ]
path_met <- '//hdata/grid/metdata/met_obs_v2.0'
path_runoff <- '//hdata/fou/Vannbalansekart/Data/Runoff_All'
regine_main <- metadata$regine_main
time_vec <- seq(ymd("2000-10-01"), ymd("2010-10-10"), by = "days")
sample_data <- load_data_elev(path_met, path_runoff, regine_main, time_vec)

# Save to project ---------------------------------------------------------

devtools::use_data(sample_data, internal = FALSE, overwrite = TRUE)


