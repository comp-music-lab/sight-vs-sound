#This code was used on 2021-1-29 to perform the analyses reported in:
#Chiba, G., Savage, P. E., & Fujii, S. Sight vs. sound in the judgment of music performance: evidence from Tsugaru shamisen competitions in Japan.

#Set working directory
setwd("/Volumes/NO NAME/2021/01_SFC/spring-semester/zemi/sight-vs-sound/pilot_data")

#Install and load packages
install.packages("TOSTER")
install.packages("pwr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("plotrix")
install.packages("compute.es")
install.packages("latex2exp")

library(TOSTER)
library(pwr)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(plotrix)
library(compute.es)
library(psych)
library(latex2exp) 
library(gridExtra)
library(ggsci)

#power analysis
pwr.t.test(n =, d = 0.4, sig.level = .025, power = .95, type=c("paired"), alternative=("two.sided"))

#Specify data download location
file.data <- "/Volumes/NO NAME/2021/01_SFC/spring-semester/zemi/sight-vs-sound/pilot_data/sight-vs-sound_6s.csv"

#Load pilot data
data<-read.csv(file=file.data)
data$winlos<-rowMeans(data[,3:4])
winlos <- data$winlos

#Define function for mean and 95% confidence interval
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-std.error(x)*1.96
  ymax <- m+std.error(x)*1.96
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#Plot data
chart_1 <- ggplot(data, aes(x=group, y=winner, color=group)) + geom_abline(intercept=33, slope=0, linetype=2, alpha=0.8) + geom_violin() + stat_summary(fun.data=data_summary, geom ="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(0,100) + labs(title = '', x = TeX('group'), y = TeX('Selecting Actual Winner ($\\mathrm{%}$)')) + theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"))
chart_2 <- ggplot(data, aes(x=group, y=loser, color=group)) + geom_abline(intercept=33, slope=0, linetype=2, alpha=0.8) + geom_violin() + stat_summary(fun.data=data_summary, geom ="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(0,100) + labs(title = '', x = TeX('group'), y = TeX('Selecting Actual Loser ($\\mathrm{%}$)')) + theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"))

grid.arrange(chart_1, chart_2, ncol=2, top="6 seconds clip")
