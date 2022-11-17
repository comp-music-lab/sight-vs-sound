###### read data ######
df_data <- read.csv(paste(OUTPUTDIR, INPUT_FILENAME, sep = ""), header = TRUE)
df_data <- df_data[!(df_data$data_id %in% DATAID_INVALID), ]

###### data formatting by aggregation ######
df_stats <- aggregate(df_data$score, by = list(df_data$participant_id, df_data$instrument, df_data$domain, df_data$varcond), FUN = mean)
names(df_stats) <- c("participant_id", "instrument", "domain", "varcond", "score")

###### one-sample t-test ######
# Although the data may not follow the normal distribution, the simulation shows the t-test can maintain the nominal type I error rate.
# See the Matlab script "sim_onesample.m" for the simulation.

my_log <- file(paste(OUTPUTDIR, "versusAtChance.txt", sep = ""))
sink(my_log, append = FALSE, type = "output")
sink(my_log, append = TRUE, type = "message")

INSTRUMENT = unique(df_stats$instrument)
DOMAIN = unique(df_stats$domain)
VARIANCE = unique(df_stats$varcond)

for (i in 1:length(INSTRUMENT)) {
  for (j in 1:length(DOMAIN)) {
    for (k in 1:length(VARIANCE)) {
      cat(paste("##### ", INSTRUMENT[i], " x ", DOMAIN[j], " x ", VARIANCE[k], ": Testing for versus at chance (50%) #####", sep = ""))
      
      idx = df_stats$instrument == INSTRUMENT[i] & df_stats$domain == DOMAIN[j] & df_stats$varcond == VARIANCE[k]
      print(t.test(x = df_stats$score[idx], mu = 0.5, alternative = "two.sided", conf.level = 0.05))
    }
  }
}

closeAllConnections()