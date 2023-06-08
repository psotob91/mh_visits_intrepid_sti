rm(list = ls())
graphics.off()
library(openxlsx)
library(glmmTMB)
library(forecast)

#load dataset. The directory where the file should be can be found with the following command. It can be changed with setwd()
getwd()
df<-read.xlsx("Chlorophyll.xlsx",colNames = T)
#Transform character variables to factors
df$Genotype<-factor(df$Genotype)
#Exp is the number of independent experiment
df$Exp<-factor(df$Exp)
#define time variables
df$T1<-df$Time
df$T2<-df$Time^2
df$T3<-df$Time^3
df$Time<-factor(df$Time)
df$Replicate<-factor(df$Replicate)
df$id<-df$Genotype:df$Replicate:df$Exp
#Scale to the first time point
for (i in 1:(length(unique(df$id)))){
  df$CHL[which(df$id==unique(df$id)[i])]<-df$CHL[which(df$id==unique(df$id)[i])]/df$CHL[which(df$id==unique(df$id)[i])][1]
  
}
#Create the initial model
# Make the WT reference category
df <- within(df, Genotype <- relevel(Genotype, ref = "WT"))
#Response variable to log scale to reduce the overall variance

mdl<-glmmTMB(log(CHL)~1+Genotype+T1+T2+(1+T1+T2|id),data=df,REML = F)
summary(mdl)
coefficients(mdl)
#Total number of observations
N<-dim(df)[1]
#Total number of time points of measurements
tp<-length(unique(df$Time))
#Fitted value of the initial mixed model
s<-fitted(mdl)
mm<-rep(0,(N-120))
par(mfrow=c(3,3))
#Plot all the initial residuals
for (i in seq(1,N,tp)){
  r1<-residuals(mdl)[i:(i+120)]
  plot(r1,type="l",ylab="Residuals",main=df$id[i])
}
#Autocorrelations in residuals
par(mfrow=c(3,3))
for (i in seq(1,N,tp)){
  r1<-residuals(mdl)[i:(i+120)]
  acf(r1,100,main=df$id[i])
}
#Time series modeling for residuals and plot of autocorrelations of new residuals
par(mfrow=c(3,3))
for (i in seq(1,N,tp)){
  r1<-residuals(mdl)[i:(i+120)]
  mm[i]<-mean(r1)
  a<-auto.arima(r1,d=1)
  s[i:(i+120)]<-s[i:(i+120)]+fitted(a)
  acf(a$residuals,100,main=df$id[i])
  print(a)
}

#Check if means are equal to zero. All values should be close to zero
mm<-mm[which(mm!=0)]
mm #indeed

df$fit<-s
test<-0
#Inside the "for" loops replace "WT" "MUT1" "MUT2" with your genotypes names on the dataset
#If you have more than 3 genotypes include more loops
for (i in 1:tp){
  
  t<- t.test(df$fit[which(df$Genotype=="WT" & df$Time==i)],df$fit[which(df$Genotype=="MUT1" & df$Time==i)])
  test[i]<-t$p.value
}
test2<-0
for (i in 1:tp){
  
  t<- t.test(s[which(df$Genotype=="WT" & df$Time==i)],s[which(df$Genotype=="MUT2" & df$Time==i)])
  test2[i]<-t$p.value
}

test3<-0
for (i in 1:tp){
  
  t<- t.test(s[which(df$Genotype=="MUT1" & df$Time==i)],s[which(df$Genotype=="MUT2" & df$Time==i)])
  test3[i]<-t$p.value
}
#Number of tests performed in each time point
no.tests<-length(unique(df$Genotype))*(length(unique(df$Genotype))-1)/2
#gather all p-values in one vector
pvals<-c(test,test2,test3)
#define time point for each p-value
Comps<-rep(1:tp,no.tests)
#define the comparisons
Comps2<-c(rep("WT-MUT1",tp),rep("WT-MUT2",tp),rep("MUT1-MUT2",tp))
#Create a data frame with everything
fdf<-data.frame(Comps2,Comps,pvals)


#Final table containing significant differences with p-value correction using manual bonferroni correction
FINAL<-fdf[which(fdf$pvals<(0.05/no.tests)),]
FINAL
#optionally write results in xlsx file
#write.xlsx(FINAL,"pvalues.xlsx") 