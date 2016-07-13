# library(tidyr)
# library(ggplot2)
#
# summary_df = data.frame(Time = sample_data[[iwsh]]$time_vec,
#                         Qobs = sample_data[[iwsh]]$Runoff,
#                         Qsim = res_sim$Q,
#                         St_1 = res_sim$St_all[ ,1],
#                         St_2 = res_sim$St_all[ ,2],
#                         SWE = rowMeans(res_sim$SWE_all))
#
#
#
#
# # a <- gather(summary_df, Variable, Value, -Time)
#
# p1 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=Qobs), color="black") + geom_line(aes(y=Qsim), color="red") + ylab("Runoff") + theme_bw()
#
# p2 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=St_1), color="black") + geom_line(aes(y=St_2), color="red") + ylab("Storages") + theme_bw()
#
# p3 <- ggplot(data=summary_df, aes(x=Time)) + geom_line(aes(y=SWE), color="black") + ylab("SWE") + theme_bw()
#
# p <- gridExtra::grid.arrange(p1, p2, p3)
#
# ggsave(p, file="results.png", width=11, height=10)



