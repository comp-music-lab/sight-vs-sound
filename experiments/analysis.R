###### import libraries ######
library(nparcomp)
library(nparLD)
library(MBESS)
library(MASS)
library(ggplot2)

###### configuration ######
INPUT_FILENAME <- "datatable.csv"
OUTPUT_FILEID <- "analysis"

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
df_data <- df_data[df_data$domain %in% c("Audio-only", "Visual-only"), ]
df_data <- df_data[!(df_data$data_id %in% DATAID_INVALID), ]

###### data formatting by aggregation ######
df_stats <- aggregate(df_data$score, by = list(df_data$participant_id, df_data$instrument, df_data$domain, df_data$varcond), FUN = mean)
names(df_stats) <- c("participant_id", "instrument", "domain", "varcond", "score")

###### Setup for getting the second degree of freedom of the ANOVA-type statistics to obtain partial η^2 for the equivalence testing of interaction effects ######
# <Notes>
# The ANOVA-type statistics provides more accurate alpha-level control than the Wald-type statistics for small or moderate sample sizes (Friedrich et al., 2017).
# But the ANOVA-type statistics does not have asymptotically consistent alpha-level control. The wild bootstrap procedure can remedy this issue (Friedrich et al., 2017).
# The Wald-type statistics has asymptotically consistent control. But it requires large sample sizes otherwise its behavior become liberal (Friedrich et al., 2017; Konietschke et al., 2010).
# The sampling distribution of ANOVA-type statistics for repeated measures design can be approximated by a central F(f, ∞) (Friedrich et al., 2017).
# **IMPORTANT** This sampling distribution can be further approximated by F(f, f0) (finite second degree of freedom) for the case of the main effects of the whole-plot factors or interactions involving only whole-plot factors (Friedrich et al., 2017; Noguchi et al., 2012). See Noguchi et al. (2012) for examples.
# The following lines follow p.301 of Brunner et al. (2018).
#
# <References>
# Brunner E., Bathke A. C., & Konietschke F. (2018). Rank and pseudo-rank procedures for independent observations in factorial designs: Using R and SAS. Springer. https://ci.nii.ac.jp/ncid/BB28708839
# Friedrich, S., Konietschke, F., & Pauly, M. (2017). A wild bootstrap approach for nonparametric repeated measurements. Computational Statistics & Data Analysis, 113, 38–52. https://doi.org/10.1016/j.csda.2016.06.016
# Konietschke, F., Bathke, A. C., Hothorn, L. A., & Brunner, E. (2010). Testing and estimation of purely nonparametric effects in repeated measures designs. Computational Statistics & Data Analysis, 54(8), 1895–1905. https://doi.org/10.1016/j.csda.2010.02.019
# Noguchi, K., Gel, Y. R., Brunner, E., & Konietschke, F. (2012). nparLD: An R Software Package for the Nonparametric Analysis of Longitudinal Data in Factorial Experiments. Journal of Statistical Software, 50, 1–23. https://doi.org/10.18637/jss.v050.i12

tr = function(x) {sum(diag(x))}
a = length(unique(df_stats$domain))
b = length(unique(df_stats$varcond))
n = length(unique(df_stats$participant_id))

P_a = diag(a) - (1/a)*matrix(1, a, a)
P_b = diag(b) - (1/b)*matrix(1, b, b)
C = P_a %x% P_b
T = t(C)%*%ginv(C%*%t(C))%*%C
D = diag(diag(T))
Lmd = diag(rep(1/(n - 1), a*b))

###### Hypothesis testing (paired two-sample tests) ######
my_log <- file(paste("./output/", OUTPUT_FILEID, "_", FILEID, ".txt", sep = ""))
sink(my_log, append = FALSE, type = "output")
sink(my_log, append = TRUE, type = "message")

pval_list <- vector(mode = "numeric", length = 6)
es_raw_list <- vector(mode = "numeric", length = 6)
es_conv_list <- vector(mode = "numeric", length = length(es_raw_list))
petasq_lu_list <- matrix(0, nrow = 2, ncol = 2)

printtext <- c("\n###### hypothesis testing 1 (Piano x low-variance) ######\n",
               "\n###### hypothesis testing 2 (Piano x high-variance) ######\n",
               "\n###### hypothesis testing 3 (Piano) ######\n",
               "\n###### hypothesis testing 4 (Tsugaru shamisen x low-variance) ######\n",
               "\n###### hypothesis testing 5 (Tsugaru shamisen x high-variance) ######\n",
               "\n###### hypothesis testing 6 (Tsugaru shamisen) ######\n"
               )
plottitle <- c("Piano x low-variance",
               "Piano x high-variance",
               "Piano",
               "Tsugaru shamisen x low-variance",
               "Tsugaru shamisen x high-variance",
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
    
    # Hypothesis testing
    result_effectsize <- npar.t.test.paired(score ~ domain, df_i, conf.level = 1 - al_CI, alternative = alternative[i], nperm = 10000)
    pval_list[i] <- result_effectsize$Analysis[2, 5]
    es_raw_list[i] <- result_effectsize$Analysis[2, 2]
    es_conv_list[i] <- sqrt(2)*qnorm(es_raw_list[i])
    
    # Equivalence testing
    result_equiv <- npar.t.test.paired(score ~ domain, df_i, conf.level = 0.90, alternative = "two.sided", nperm = 10000)
    cat("\n***Check confidence intervals for equivalence testing***\n")
    print(result_equiv$Analysis)
    
  } else if(i == 3 || i == 6) {
    df_i <- data.frame(participant_id = df_stats$participant_id[idx], score = df_stats$score[idx], Domain = df_stats$domain[idx], Variance = df_stats$varcond[idx])
    
    # Hypothesis testing
    result_effectsize <- nparLD(score ~ Domain*Variance, data = df_i, subject = "participant_id",
                                plot.CI = TRUE, alpha = al_CI, show.covariance = TRUE)
    
    summary(result_effectsize)
    pval_list[i] <- result_effectsize$ANOVA.test[3, 3]
    es_raw_list[i] <- result_effectsize$ANOVA.test[3, 1]
    
    # Calculate partial η^2 for equivalence testing
    V = result_effectsize$covariance
    
    df_effect = tr(T%*%V)^2/tr(T%*%V%*%T%*%V)
    df_error = tr(D*V)^2/tr(D^2 %*% V^2 %*% Lmd)
    F = result_effectsize$ANOVA.test[3, 1]
    
    petasq = F*df_effect/(F*df_effect + df_error) # convert F-value to partial eta squared
    petasqadj = petasq - (1 - petasq)*df_effect/df_error # convert to adjusted partial eta squared
    
    es_conv_list[i] <- petasqadj
    
    # estimate 90% CI endpoints of the non-centrality parameters of non-central F distribution and convert them to CI of partial eta-squared
    k <- i/3
    ncf_lu <- conf.limits.ncf(F.value = F, conf.level = 0.9, df.1 = df_effect, df.2 = df_error)
    petasq_lu_list[k, ] <- c(ncf_lu$Lower.Limit/(ncf_lu$Lower.Limit + df_effect + df_error + 1), 
                             ncf_lu$Upper.Limit/(ncf_lu$Upper.Limit + df_effect + df_error + 1))
    petasq_lu_list[k, ] <- petasq_lu_list[k, ] - (1 - petasq_lu_list[k, ]) * df_effect/df_error
    
    # Plot result
    g_i <- ggplot(data = result_effectsize$Conf.Int, aes(x = Time1, group = Time2, colour = Time2))
    g_i <- g_i + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, position = position_dodge(width = 0.2))
    g_i <- g_i + geom_point(aes(y = RTE, fill = Time2), position = position_dodge(width = 0.2), shape = 22)
    g_i <- g_i + geom_line(aes(y = RTE), position = position_dodge(width = 0.2))
    g_i <- g_i + geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey27")
    g_i <- g_i + labs(x = "", y = "Relative Effects",
                      title = paste("Interaction effects and ", 100*(1 - al_CI), "% CI (", df_stats$instrument[idx][1], ")", sep = "")) + 
      theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
      ylim(0, 1)
    ggsave(plot = g_i, file = paste("./output/", OUTPUT_FILEID, "_H", i, "_", FILEID, ".png", sep = ""), width = G_WID, height = G_HEI)
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