rm(list = ls())

###### configuration ######
INPUT_FILENAME <- "datatable_pilot.csv"
DOMAIN <- c("Audio-only", "Visual-only")
INSTRUMENT <- c("Piano", "Tsugaru shamisen")
VARIANCE <- c("low-variance", "high-variance")

al_level_test <- 0.05/6
beta <- 0.8

###### read data ######
df_data <- read.csv(paste("./data/", INPUT_FILENAME, sep = ""), header = TRUE)

###### data formatting by aggregation ######
PARTICIPANT_ID <- unique(df_data$participant_id)
df_stats <- data.frame(participant_id = as.numeric(), score = as.numeric(),
                       instrument = as.character(), domain = as.character(), varcond = as.character())

for (i in PARTICIPANT_ID) {
  for (j in INSTRUMENT) {
    for (k in DOMAIN) {
      for (l in VARIANCE) {
        idx <- df_data$participant_id == i & df_data$instrument == j & df_data$domain == k & df_data$varcond == l
        score_i <- sum(df_data$score[idx])/sum(idx)
        df_stats <- rbind(df_stats,
                          data.frame(participant_id = i, score = score_i, instrument = j, domain = k, varcond = l)
        )
      }
    }
  }
}

###### extract ######
idx <- df_stats$instrument == "Tsugaru shamisen" & df_stats$varcond == "low-variance"
df_i <- data.frame(participant_id = df_stats$participant_id[idx], score = df_stats$score[idx], domain = df_stats$domain[idx])

###### Noether's formula ######
Z <- aggregate(x = df_i[c("score")], by = list(df_i$participant_id), FUN = function(x) {return(x[2] - x[1])})

N_pos <- 0
N_neg <- 0

for(i in 1:nrow(Z)) {
  for(j in i:nrow(Z)) {
    if(Z$score[i] + Z$score[j] > 0) {
      N_pos = N_pos + 1
    } else {
      N_neg = N_neg + 1
    }
  }
}

p_dash <- N_pos/(N_pos + N_neg)
N_noether <- (qnorm(1 - al_level_test) + qnorm(beta))^2/(3*(p_dash - 0.5)^2)

###### Variance adjustment due to tied values ######
Z_abs <- round(abs(Z$score), 1)
Z_uni <- unique(Z_abs)
P_i <- sapply(Z_uni, function(x) {return(sum(x == Z_abs)/length(Z_abs))})

N_divine <- N_noether * (1 - sum(P_i^3)/4)

###### Sample size ######
N <- ceiling(N_divine)
cat(paste('Sample size: N = ', N, sep = ""))