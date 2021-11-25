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
idx = (T$domain == 2 | T$domain == 3) & (T$var_cond == 0)
T_i <- T[idx, ]

rank.two.samples(score ~ domain, data = T_i, alternative = "greater", method = "t.app", info = TRUE)
# (result)
# Nonparametric Methods for 2 Independent Samples      
#  
#  #Alternative: Relative Effect is greater than 1/2 
#  #Method: T-Approximation with 188.1132 DF 
#  #Interpretation: If p(2,3) >1/2, then data in group 3 tend to be larger than # those in group 2 
#  #Confidence Level: 95 % 
#  #Number of permutations: 10000 
#  
# #Wilcoxon-Mann-Whitney Test: asymptotic 
#  #Shift-Effect: delta(.): Hodges-Lehmann Estimator 
# ---------------------------------------------------------------------------
# 
# Call:
# score ~ domain
# 
# Descriptive:
#   Sample Size
# 2      2  100
# 3      3  100
# 
# ----------------------Analysis of Relative Effects-------------------------
# Test Results:
#  Effect Estimator Std.Error      T  Lower Upper p.Value
#  p(2,3)    0.6207    0.0389 3.0996 0.5563     1  0.0011
# 
# Studentized Permutation Test:
#  Effect Estimator Std.Error      T  Lower Upper p.Value
#  p(2,3)    0.6207    0.0389 3.0996 0.5566     1   0.002
# 
# 
# -------------------Analysis of Distribution Functions----------------------- 
#  
# Wilcoxon-Mann-Whitney Test:
#  Effect Estimator Std.Error Statistic p.Value
#  p(2,3)    0.6207    0.0402    3.0039  0.0013
# 
# Shift Effects:
#      Effect Estimator Lower Upper
#  delta(3-2)       0.1     0   Inf

noether(alpha = 0.05, power = 0.8, t = 0.5, p = 0.6, x1 = T_i$score[T_i$domain == 2])
# (result)
# "Advance information is not used in case of a continuous distribution!"
#                               Results
# alpha (2-sided)                0.0500
# Power                          0.8000
# relevant relative effect p     0.6000
# N (total sample size needed) 249.0999
# t=n1/N                         0.5000
# n1 in Group 1                124.5499
# n2 in Group 2                124.5499
# N rounded                    250.0000
# n1 rounded                   125.0000
# n2 rounded                   125.0000

idx = (T$domain == 2 | T$domain == 3) & (T$var_cond == 1)
T_i <- T[idx, ]

rank.two.samples(score ~ domain, data = T_i, alternative = "less", method = "t.app", info = TRUE)
# (result)
# Nonparametric Methods for 2 Independent Samples      
#  
#  #Alternative: Relative Effect is less than 1/2 
#  #Method: T-Approximation with 97.1401 DF 
#  #Interpretation: If p(2,3) >1/2, then data in group 3 tend to be larger than # those in group 2 
#  #Confidence Level: 95 % 
#  #Number of permutations: 10000 
#  
# #Wilcoxon-Mann-Whitney Test: asymptotic 
#  #Shift-Effect: delta(.): Hodges-Lehmann Estimator 
# ---------------------------------------------------------------------------
# 
# Call:
# score ~ domain
# 
# Descriptive:
#   Sample Size
# 2      2   50
# 3      3   50
# 
# ----------------------Analysis of Relative Effects-------------------------
# Test Results:
#  Effect Estimator Std.Error       T Lower  Upper p.Value
#  p(2,3)     0.202    0.0427 -6.9813     0 0.2729       0
# 
# Studentized Permutation Test:
#  Effect Estimator Std.Error       T Lower  Upper p.Value
#  p(2,3)     0.202    0.0427 -6.9813     0 0.2732       0
# 
# 
# -------------------Analysis of Distribution Functions----------------------- 
#  
# Wilcoxon-Mann-Whitney Test:
#  Effect Estimator Std.Error Statistic       p.Value
#  p(2,3)     0.202    0.0561   -5.3158 <2.220446e-16
# 
# Shift Effects:
#      Effect Estimator Lower Upper
#  delta(3-2)      -0.2  -Inf  -0.2

noether(alpha = 0.05, power = 0.8, t = 0.5, p = 0.25, x1 = T_i$score[T_i$domain == 2])
# (result)
# (result)
# "Advance information is not used in case of a continuous distribution!"
#                               Results
# alpha (2-sided)               0.05000
# Power                         0.80000
# relevant relative effect p    0.25000
# N (total sample size needed) 37.96195
# t=n1/N                        0.50000
# n1 in Group 1                18.98098
# n2 in Group 2                18.98098
# N rounded                    38.00000
# n1 rounded                   19.00000
# n2 rounded                   19.00000