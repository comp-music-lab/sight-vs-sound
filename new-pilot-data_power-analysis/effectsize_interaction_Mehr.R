rm(list = ls())

#####
library(rankFD)

#####
filepath_data_exp_2 <- "./data/SBS_exp2.csv"
filepath_data_exp_3 <- "./data/SBS_exp3.csv"
al_CI <- 0.1

#######
T_exp2 <- read.table(filepath_data_exp_2, header = TRUE, sep = ",")
idx <- T_exp2$condition == 2 | T_exp2$condition == 3
df_exp2 <- data.frame(score = T_exp2$score[idx], domain = T_exp2$condition[idx], variance = "low")

T_exp3 <- read.table(filepath_data_exp_3, header = TRUE, sep = ",")
idx <- T_exp3$condition == 2 | T_exp3$condition == 3
df_exp3 <- data.frame(score = T_exp3$score[idx], domain = T_exp3$condition[idx], variance = "high")

df = rbind(df_exp2, df_exp3)

#####
my_log <- file(paste("./output/effectsize_interaction_Mehr.txt", sep = ""))
sink(my_log, append = FALSE, type = "output")
sink(my_log, append = TRUE, type = "message")

result_es = rankFD(score ~ domain * variance, data = df, alpha = al_CI, CI.method = "logit",
                   effect = "unweighted", hypothesis = "H0p", Factor.Information = TRUE, info = TRUE)
print(result_es)

#######
ATS <- result_es$ANOVA.Type.Statistic[3, 1]
df_effect <- result_es$ANOVA.Type.Statistic[3, 2]
df_error <- result_es$ANOVA.Type.Statistic[3, 3]
eta_p = (ATS * df_effect)/(ATS * df_effect + df_error)
cat(paste("\n", eta_p, " - partial eta squared based on approximated ANOVA type statistics\n", sep = ","))

#######
closeAllConnections()