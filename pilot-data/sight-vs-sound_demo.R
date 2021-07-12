#This code was used on 2021-1-29 to perform the analyses reported in:
#Chiba, G., Savage, P. E., & Fujii, S. Sight vs. sound in the judgment of music performance: evidence from Tsugaru shamisen competitions in Japan.

#Set working directory
setwd("/Users/chibagakuto/Desktop/pilot-data")

#Install and load packages
install.packages("TOSTER")
install.packages("pwr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("plotrix")
install.packages("compute.es")

library(TOSTER)
library(pwr)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(plotrix)
library(compute.es)
library(psych)

#power analysis
pwr.anova.test(n = , k = 3 , f = 0.4, sig.level = 0.017, power = 0.8)

#Specify data download location
file.data <- "/Users/chibagakuto/Desktop/pilot-data/sight-vs-sound_demo.csv"

#Load pilot data
data<-read.csv(file=file.data)
data$winlos<-rowMeans(data[,4:5])
winlos <- data$winlos

#Define function for mean and 95% confidence interval
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-std.error(x)*1.96
  ymax <- m+std.error(x)*1.96
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#Plot data
ggplot(data, aes(x=group, y=winner, color=group)) + geom_violin() + stat_summary(fun.data=data_summary, geom ="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(0,100) + theme(axis.text=element_text(size=10),axis.title=element_text(size=10,face="bold"))
ggplot(data, aes(x=group, y=loser, color=group)) + geom_violin() + stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(0,100) + theme(axis.text=element_text(size=10),axis.title=element_text(size=10,face="bold"))
