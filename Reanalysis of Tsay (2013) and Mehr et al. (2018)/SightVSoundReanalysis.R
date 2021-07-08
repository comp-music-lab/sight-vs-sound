#R script for Chiba et al. sight vs sound in tsugaru shamisen

#Reanalyze Mehr et al. 2018 Study 1
d<-read.csv("SBS_exp1.csv")
newdata <- subset(d, condition >= 2 )
t.test(score~condition,newdata)
#t = -4.5134, df = 243.22, p-value = 9.94e-06


#Reanalyze Mehr et al. 2018 Study 2
d<-read.csv("SBS_exp2.csv")
newdata <- subset(d, condition >= 2 )
t.test(score~condition,newdata)
#t = -2.994, df = 184.75, p-value = 0.003131

#Reanalyze Mehr et al. 2018 Study 3
d<-read.csv("SBS_exp3.csv")
newdata <- subset(d, condition >= 2 )
t.test(score~condition,newdata)
#t = 6.059, df = 97.92, p-value = 2.551e-08