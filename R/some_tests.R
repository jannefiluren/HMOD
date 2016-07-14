# library(tidyr)
# library(dplyr)
# library(ggplot2)
# library(plotly)
#
# iwsh <- 3
#
# indata = list(Prec           = sample_data[[iwsh]]$Prec,
#               Tair           = sample_data[[iwsh]]$Tair,
#               PET            = rep(0, nrow(sample_data[[iwsh]]$Prec)),
#               SWE            = matrix(0, nrow = 1, ncol = ncol(sample_data[[iwsh]]$Prec)),
#               St             = matrix(0, nrow = 2, ncol = 1),
#               StUH1          = matrix(0, 20, ncol = 1),
#               StUH2          = matrix(0, 40, ncol = 1),
#               Param          = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02),
#               frac_elev_band = sample_data[[iwsh]]$frac_elev_band)
#
# data_sim <- model_wrapper(indata)
#
# data_obs <- sample_data[[iwsh]]
#
# df_obs <- data.frame(Time = data_obs$time_vec,
#                      Runoff_Obs = data_obs$Runoff)
#
# df_sim <- data.frame(Time = data_obs$time_vec,
#                      Runoff_Sim = data_sim$Q,
#                      States_SWE = rowMeans(data_sim$SWE_all),
#                      States_St1 = data_sim$St_all[ ,1],
#                      States_St2 = data_sim$St_all[ ,2])
#
#
# df_final <- full_join(df_obs, df_sim, by= "Time") %>% gather(Tmp, Value, -Time) %>% separate(Tmp, into = c("Variable", "Type"), sep = "_")
#
# ggplot(df_final, aes(x = Time, y = Value)) + geom_line(aes(col = Type)) + facet_grid(Variable ~ ., scales="free_y") + theme_bw()
#
# ggplotly()
