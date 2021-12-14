rm(list = ls())

#####
library(rankFD)

#####
filepath_data_exp <- c("./data/SBS_exp1.csv", "./data/SBS_exp2.csv", "./data/SBS_exp3.csv")
al <- 0.05/6
be <- 0.8
H1_alt <- c("greater", "greater", "less")
cl_d_0_4 <- pnorm(0.4/sqrt(2))
message <- c("\n###### Mehr et al. (2018) - Experiment 1 (triad, low-variance)\n",
             "\n###### Mehr et al. (2018) - Experiment 2 (diad, low-variance)\n",
             "\n###### Mehr et al. (2018) - Experiment 3 (diad, high-variance)\n"
             )

#####
#my_log <- file(paste("./output/effectsize_two-sample_Mehr.txt", sep = ""))
#sink(my_log, append = FALSE, type = "output")
#sink(my_log, append = TRUE, type = "message")

for (i in 1:length(filepath_data_exp)) {
  T <- read.table(filepath_data_exp[i], header = TRUE, sep = ",")
  idx <- T$condition == 2 | T$condition == 3
  df <- data.frame(score = T$score[idx], domain = T$condition[idx])
  
  #####
  cat(message[i])
  result_es <- rank.two.samples(score ~ domain, data = df, alternative = H1_alt[i], method = "t.app", info = TRUE)
  print(result_es)
  
  p_raw <- result_es$Analysis$Estimator
  if (p_raw < 0.5) {
    p_raw <- 1 - p_raw
  }
  
  if (cl_d_0_4 < p_raw && p_raw < 0.7) {
    p_conservative <- cl_d_0_4
  } else if (0.7 < p_raw && p_raw < 0.75) {
    p_conservative <- 0.7
  } else if (0.75 < p_raw && p_raw < 0.8) {
    p_conservative <- 0.75
  } 
  
  result_ss <- noether(alpha = al, power = be, t = sum(df$domain == 2)/nrow(df), p = p_conservative, x1 = df$score[df$domain == 2])
  print(result_ss)
}

closeAllConnections()