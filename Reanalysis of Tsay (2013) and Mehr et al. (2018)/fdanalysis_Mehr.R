#####
library(rankFD)

#####
filepath_data_exp2 = "./data/SBS_exp2.csv"
filepath_data_exp3 = "./data/SBS_exp3.csv"

#####
T1 = read.table(filepath_data_exp2, header = TRUE, sep = ",");
T1$var_cond = 0 #low-variance
T2 = read.table(filepath_data_exp3, header = TRUE, sep = ",");
T2$var_cond = 1 #high-variance

T = rbind(subset(T1, select = -c(met)), T2)
colnames(T)[4] <- "domain"

#####
model = rankFD(score ~ domain * var_cond, data = T, alpha = 0.05, CI.method = "logit",
               effect = "unweighted", hypothesis = "H0p", Factor.Information = TRUE, info = TRUE)

# (Result)
#
#Nonparametric Methods for General Factorial Designs      
#
#---------------------------------------------------------------------------
#  #Hypotheses: Tested in Relative Effects 
#  #Ranking Method: Pseudo-Ranks 
#  #Confidence Intervals: 95 % with Logit-Transformation 
#  
#  
#  ---------------------------------------------------------------------------
#  
#  Call:
#  score ~ domain * var_cond
#
#Descriptive:
#  domain var_cond Size Rel.Effect Std.Error  Lower  Upper
#1      1        0  100     0.3590    0.0235 0.3143 0.4063
#2      1        1   50     0.6544    0.0305 0.5923 0.7116
#3      2        0  100     0.3841    0.0221 0.3418 0.4282
#4      2        1   50     0.7150    0.0290 0.6549 0.7683
#5      3        0  100     0.4936    0.0249 0.4451 0.5423
#6      3        1   50     0.3939    0.0336 0.3303 0.4614
#
#Wald.Type.Statistic:
#  Statistic df p-Value
#domain            12.1329  2  0.0023
#var_cond          50.1492  1  0.0000
#domain:var_cond   62.3668  2  0.0000
#
#ANOVA.Type.Statistic:
#  Statistic    df1      df2 p-Value
#domain             6.1110 1.9884 349.7635  0.0025
#var_cond          50.1492 1.0000 349.7635  0.0000
#domain:var_cond   31.9079 1.9938 349.7635  0.0000
#
#Kruskal-Wallis Test:
#  NULL
#
#MCTP:
#  NULL
#
#Factor.Information:
#  $domain
#domain Rel.Effect Std.Error  Lower  Upper
#1      1     0.5067    0.0176 0.4722 0.5411
#2      2     0.5496    0.0170 0.5161 0.5825
#3      3     0.4438    0.0181 0.4086 0.4795
#
#$var_cond
#var_cond Rel.Effect Std.Error  Lower  Upper
#1        0     0.4122    0.0124 0.3882 0.4367
#2        1     0.5878    0.0124 0.5633 0.6118
#
#$`domain:var_cond`
#domain var_cond Rel.Effect Std.Error  Lower  Upper
#1      1        0     0.3590    0.0235 0.3143 0.4063
#4      1        1     0.6544    0.0305 0.5923 0.7116
#2      2        0     0.3841    0.0221 0.3418 0.4282
#5      2        1     0.7150    0.0290 0.6549 0.7683
#3      3        0     0.4936    0.0249 0.4451 0.5423
#6      3        1     0.3939    0.0336 0.3303 0.4614
#
#
#(partial eta squared based on approximated ANOVA type statistics)
#eta_p = (31.9079 * 1.9938)/(31.9079 * 1.9938 + 349.7635)