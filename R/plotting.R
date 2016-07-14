

#' Standard plot of model results
#'
#' @param data_obs Observed data (in format given by package NVEDATA)
#' @param data_mod Model data (in raw output format)
#' @param path Path for saving plots
#' @import ggplot2
#' @export
#' @examples
#'
#'
#' iwsh <- 3
#'
#' data_obs <- sample_data[[iwsh]]
#'
#' indata = list(Time           = data_obs$time_vec,
#'               Prec           = data_obs$Prec,
#'               Tair           = data_obs$Tair,
#'               PET            = rep(0, nrow(data_obs$Prec)),
#'               SWE            = matrix(0, nrow = 1, ncol = ncol(data_obs$Prec)),
#'               St             = matrix(0, nrow = 2, ncol = 1),
#'               StUH1          = matrix(0, 20, ncol = 1),
#'               StUH2          = matrix(0, 40, ncol = 1),
#'               Param          = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02),
#'               frac_elev_band = data_obs$frac_elev_band)
#'
#' data_sim <- model_wrapper(indata)
#'
#' path <- getwd()
#'
#' plotting_results(data_obs, data_sim, path)

plotting_results <- function(data_obs, data_sim, path) {

  df_raw <- data.frame(Time        = data_obs$time_vec,
                       Runoff_Obs  = data_obs$Runoff,
                       Runoff_Sim  = data_sim$Q,
                       States_SWE  = rowMeans(data_sim$SWE_all),
                       States_St1  = data_sim$St_all[ ,1],
                       States_St2  = data_sim$St_all[ ,2])

  df_final <- gather(df_raw, Tmp, Value, -Time) %>% separate(Tmp, into = c("Variable", "Type"), sep = "_")

  p <- ggplot(df_final, aes(x = Time, y = Value)) + geom_line(aes(col = Type)) + facet_grid(Variable ~ ., scales="free_y") + theme_bw()

  filename <- file.path(path, paste("plot_", data_obs$regine_main, ".png", sep=""))

  ggsave(p, file = filename, width = 8, height = 7, dpi = 600, device = "png")

  #   summary_df = data.frame(Time = data_obs$time_vec,
  #                           Qobs = data_obs$Runoff,
  #                           Qsim = data_mod$Q,
  #                           St_1 = data_mod$St_all[ ,1],
  #                           St_2 = data_mod$St_all[ ,2],
  #                           SWE = rowMeans(data_mod$SWE_all))
  #
  #   p1 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=Qobs), color="black") + geom_line(aes(y=Qsim), color="red") + ylab("Runoff") + theme_bw()
  #
  #   p2 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=St_1), color="black") + geom_line(aes(y=St_2), color="red") + ylab("Storages") + theme_bw()
  #
  #   p3 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=SWE), color="black") + ylab("SWE") + theme_bw()
  #
  #   p <- gridExtra::grid.arrange(p1, p2, p3)

}





#' Interactive plot of model results
#'
#' @param data_obs Observed data (in format given by package NVEDATA)
#' @param data_mod Model data (in raw output format)
#' @import tidyr
#' @import ggplot2
#' @import plotly
#' @export
#' @examples
#'
#' iwsh <- 3
#'
#' data_obs <- sample_data[[iwsh]]
#'
#' indata = list(Time           = data_obs$time_vec,
#'               Prec           = data_obs$Prec,
#'               Tair           = data_obs$Tair,
#'               PET            = rep(0, nrow(data_obs$Prec)),
#'               SWE            = matrix(0, nrow = 1, ncol = ncol(data_obs$Prec)),
#'               St             = matrix(0, nrow = 2, ncol = 1),
#'               StUH1          = matrix(0, 20, ncol = 1),
#'               StUH2          = matrix(0, 40, ncol = 1),
#'               Param          = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02),
#'               frac_elev_band = data_obs$frac_elev_band)
#'
#' data_sim <- model_wrapper(indata)
#'
#' plot_interactive(data_obs, data_sim)

plot_interactive <- function(data_obs, data_sim) {

  df_raw <- data.frame(Time        = data_obs$time_vec,
                       Inputs_Prec = rowMeans(data_obs$Prec),
                       Inputs_Tair = rowMeans(data_obs$Tair),
                       Runoff_Obs  = data_obs$Runoff,
                       Runoff_Sim  = data_sim$Q,
                       States_SWE  = rowMeans(data_sim$SWE_all),
                       States_St1  = data_sim$St_all[ ,1],
                       States_St2  = data_sim$St_all[ ,2])

  df_final <- gather(df_raw, Tmp, Value, -Time) %>% separate(Tmp, into = c("Variable", "Type"), sep = "_")

  ggplot(df_final, aes(x = Time, y = Value)) + geom_line(aes(col = Type)) + facet_grid(Variable ~ ., scales="free_y") + theme_bw()

  ggplotly()

}

