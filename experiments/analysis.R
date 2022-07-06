rm(list = ls())

###### import libraries ######
library(nparcomp)
library(nparLD)
library(MBESS)
library(MASS)
library(ggplot2)

###### configuration ######
INPUT_FILENAME <- "datatable_20220701.csv"
OUTPUT_FILEID <- "analysis_20220701"
DOMAIN <- c("Audio-only", "Visual-only")
INSTRUMENT <- c("Piano", "Tsugaru shamisen")
VARIANCE <- c("low-variance", "high-variance")

al_CI <- 0.05

G_VIOLIN_ADJUST <- 0.6
G_VIOLIN_SCALE <- "area"
G_JITTER_WID <- 0.05
G_TITLE_SIZE <- 10
G_X_SIZE <- 8
G_Y_SIZE <- 8
G_X_ROTATE <- -0
G_WID <- 6
G_HEI <- 4.8

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

###### Hypothesis testing (paired two-sample tests) ######
my_log <- file(paste("./output/", OUTPUT_FILEID, ".txt", sep = ""))
sink(my_log, append = FALSE, type = "output")
sink(my_log, append = TRUE, type = "message")

pval_list <- vector(mode = "numeric", length = 6)
es_raw_list <- vector(mode = "numeric", length = 6)
es_conv_list <- vector(mode = "numeric", length = length(es_raw_list))
petasq_lu_list <- matrix(0, nrow = 2, ncol = 2)

# Contrast matrix + ƒ¿ to estimate the second degree of freedom of the ANOVA-type statistics
P_a <- diag(c(rep(1, length(DOMAIN)))) - 1/length(DOMAIN)*matrix(1, nrow = length(DOMAIN), ncol = length(DOMAIN))
P_b <- diag(c(rep(1, length(VARIANCE)))) - 1/length(VARIANCE)*matrix(1, nrow = length(VARIANCE), ncol = length(VARIANCE))
C <- P_a %x% P_b
gC <- ginv(C%*%t(C))
T <- t(C)%*%gC%*%C
D <- diag(diag(T))
Lmd <- diag(rep(1/(length(PARTICIPANT_ID) - 1), length(DOMAIN)*length(VARIANCE)))

printtext <- c("\n###### hypothesis testing 1 (Piano ~ low-variance) ######\n",
               "\n###### hypothesis testing 2 (Piano ~ high-variance) ######\n",
               "\n###### hypothesis testing 3 (Piano) ######\n",
               "\n###### hypothesis testing 4 (Tsugaru shamisen ~ low-variance) ######\n",
               "\n###### hypothesis testing 5 (Tsugaru shamisen ~ high-variance) ######\n",
               "\n###### hypothesis testing 6 (Tsugaru shamisen) ######\n"
               )
plottitle <- c("Piano ~ low-variance",
               "Piano ~ high-variance",
               "Piano",
               "Tsugaru shamisen ~ low-variance",
               "Tsugaru shamisen ~ high-variance",
               "Tsugaru shamisen"
               )
alternative <- c("greater", "less", NaN, "greater", "less", NaN)

### main loop ###
for(i in 1:6) {
  dev.new()
  cat(printtext[i])
  
  if(i == 1) {
    idx <- df_stats$instrument == "Piano" & df_stats$varcond == "low-variance"
  } else if(i == 2) {
    idx <- df_stats$instrument == "Piano" & df_stats$varcond == "high-variance"
  } else if(i == 3) {
    idx <- df_stats$instrument == "Piano"
  } else if(i == 4) {
    idx <- df_stats$instrument == "Tsugaru shamisen" & df_stats$varcond == "low-variance"
  } else if(i == 5) {
    idx <- df_stats$instrument == "Tsugaru shamisen" & df_stats$varcond == "high-variance"
  } else if(i == 6) {
    idx <- df_stats$instrument == "Tsugaru shamisen"
  }
  
  if(i == 1 || i == 2 || i == 4 || i == 5) {
    df_i <- data.frame(participant_id = df_stats$participant_id[idx], score = df_stats$score[idx], domain = df_stats$domain[idx])
    
    result_effectsize <- npar.t.test.paired(score ~ domain, df_i, conf.level = 1 - al_CI, alternative = alternative[i], nperm = 10000)
    ggsave(file = paste("./output/", OUTPUT_FILEID, "_H", i, ".png", sep = ""), width = G_WID, height = G_HEI)
    
    pval_list[i] <- result_effectsize$Analysis[2, 5]
    es_raw_list[i] <- result_effectsize$Analysis[2, 2]
    es_conv_list[i] <- sqrt(2)*qnorm(es_raw_list[i])
  } else if(i == 3 || i == 6) {
    df_i <- data.frame(participant_id = df_stats$participant_id[idx], score = df_stats$score[idx], Domain = df_stats$domain[idx], Variance = df_stats$varcond[idx])
    
    result_effectsize <- nparLD(score ~ Domain*Variance, data = df_i, subject = "participant_id",
                                plot.CI = TRUE, alpha = al_CI, show.covariance = TRUE)
    summary(result_effectsize)
    
    g_i <- ggplot(data = result_effectsize$Conf.Int, aes(x = Time1, group = Time2, colour = Time2))
    g_i <- g_i + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, position = position_dodge(width = 0.2))
    g_i <- g_i + geom_point(aes(y = RTE, fill = Time2), position = position_dodge(width = 0.2), shape = 22)
    g_i <- g_i + geom_line(aes(y = RTE), position = position_dodge(width = 0.2))
    g_i <- g_i + geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey27")
    g_i <- g_i + labs(x = "", y = "Relative Effects",
                      title = paste("Interaction effects and ", 100*(1 - al_CI), "% CI (", df_stats$instrument[idx][1], ")", sep = "")) + 
      theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
      ylim(0, 1)
    ggsave(plot = g_i, file = paste("./output/", OUTPUT_FILEID, "_H", i, ".png", sep = ""), width = G_WID, height = G_HEI)
    
    pval_list[i] <- result_effectsize$ANOVA.test[3, 3]
    es_raw_list[i] <- result_effectsize$ANOVA.test[3, 1]
    
    df_effect <- result_effectsize$ANOVA.test[3, 2]
    df_error <- sum(diag(D%*%result_effectsize$covariance))^2 / sum(diag(D^2%*%result_effectsize$covariance^2%*%Lmd))
    es_conv_list[i] <- es_raw_list[i] * df_effect/(es_raw_list[i] * df_effect + df_error)
    es_conv_list[i] <- es_conv_list[i] - (1 - es_conv_list[i]) * df_effect/df_error
    
    k <- i/3
    ncf_lu <- conf.limits.ncf(F.value = es_raw_list[i], conf.level = 0.9, df.1 = df_effect, df.2 = df_error)
    petasq_lu_list[k, ] <- c(ncf_lu$Lower.Limit/(ncf_lu$Lower.Limit + df_effect + df_error + 1), 
                             ncf_lu$Upper.Limit/(ncf_lu$Upper.Limit + df_effect + df_error + 1))
    petasq_lu_list[k, ] <- petasq_lu_list[k, ] - (1 - petasq_lu_list[k, ]) * df_effect/df_error
  }
}

###### p-value/effect size/test statistics ######
print("")
print(plottitle)

cat("\np-value\n")
print(pval_list)
cat("\nRelative effect (1, 2, 4, 5) and ANOVA-type statistic (3, 6)\n")
print(es_raw_list)
cat("\nEffect size corresponding to Cohen's D (1, 2, 4, 5) and partial eta squared (3, 6)\n")
print(es_conv_list)

###### Confidence interval of adjusted partial eta squared ######
cat("\n90% CI of adjusted partial eta squared (equivalence testing for the interaction effects of piano)\n")
cat(paste(petasq_lu_list[1, 1], " - ", es_conv_list[3], " - ", petasq_lu_list[1, 2], "\n", sep = ""))

cat("\n90% CI of adjusted partial eta squared (equivalence testing for the interaction effects of Tsugaru shamisen)\n")
cat(paste(petasq_lu_list[2, 1], " - ", es_conv_list[6], " - ", petasq_lu_list[2, 2], "\n", sep = ""))

closeAllConnections()