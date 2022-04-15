# 2021 Examination
# AUTHOR :Oluwakemi Dada
# Date : 07-12-2021
#set working diretory
# Does the nature of the sediment vary with the water depth at the level of significance  = 1%?
BIO<-read.table("BIOENV.txt", header=TRUE, row.names = 1, dec = ".");
attach(BIO)
View(BIO)

#Step 1: Use a graph to qualitatively determine the relationship between Sediment and Depth  
boxplot(Depth~Sediment) 
#COMMENTS: The sample mean of depth for sediment type G seems significantly
#higher than the sample means for sediment types S and C.  
#Step 2: test the criteria of application for ANOVA (normality and homoscedasticity)

#Normality: Ho - variable follows a Normal Distribution  
BIO[Sediment=="S",7]->X;shapiro.test(X) #p=0.191; Ho accepted
BIO[Sediment=="C",7]->Y;shapiro.test(Y) #p=0.028; Ho accepted
BIO[Sediment=="G",7]->Z;shapiro.test(Z) #p=0.321; Ho accepted
#Homoscedasticity: Ho - same variance 
bartlett.test(Depth~Sediment,data=BIO)  #p=0.268; Ho accepted

#Conclusion : choice of a parametric test for ANOVA
#Step 3: ANOVA

#Ho: means are equal
ANOVA1<-aov(Depth~Sediment,data=BIO)
summary(ANOVA1)#p=0.001; Ho rejected
pairwise.t.test(Depth,Sediment,p.adj="none") 
#The sample mean of depth for sediment type G is significantly
#higher than the sample means for sediment types S and C, which are not significantly different.

##Question 2  Does the level of pollution impact the total abundance of species at the level of significance  = 1% ?
#Comparison of 2 quantitative variables - CORRELATION
#Step 1: test the criteria of application for correlation analysis (normality and homoscedasticity)
#Normality: Ho - variable follows a Normal Distribution  
shapiro.test(sp1)        #p=0.009; Ho rejected
shapiro.test(sp2)         #p=0.001; Ho rejected
shapiro.test(sp3)         #p=0.002; Ho rejected
shapiro.test(sp4)         #p=0.618; Ho accepted
shapiro.test(sp5)         #p=0.000; Ho rejected
shapiro.test(Pollution) #p=0.085; Ho accepted
#Homoscedasticity: Ho - same variance 
var.test(sp4,Pollution)   #p=0.000; Ho rejected because true ratio of variance is not equal to 1
#Conclusion: choice of a non-parametric test for correlation
#Step 2: test of correlation using "spearman" method.
#Ho: rho = 0
cor.test(Pollution,sp1,method="spearman") #p=0.000; Ho rejected
cor.test(Pollution,sp2,method="spearman") #p=0.000; Ho rejected
cor.test(Pollution,sp3,method="spearman") #p=0.079; Ho accepted
cor.test(Pollution,sp4,method="spearman") #p=0.000; Ho rejected
cor.test(Pollution,sp5,method="spearman") #p=0.114; Ho accepted
#Significant relationships exist between  sp1, sp2, sp3and the level of pollution but for specimen 5 it doesn't affect

#Step 3: Graphical representation

par(mfrow=c(2,3))
plot(Pollution~sp1)
abline(lm(Pollution~sp1,data=BIO),lwd=3,col="red")
plot(Pollution~sp2) 
abline(lm(Pollution~sp2,data=BIO),lwd=3,col="red")
plot(Pollution~sp3) 
abline(lm(Pollution~sp3,data=BIO),lwd=3,col="red")
plot(Pollution~sp4)
abline(lm(Pollution~sp4,data=BIO),lwd=3,col="red")
plot(Pollution~sp5) 
abline(lm(Pollution~sp5,data=BIO),lwd=3,col="red")
#COMMENTS: Figure indicates a negative correlation between species sp1,sp2 and sp4 
#and the level of pollution.  

#Question 3  : From a principal component analysis carried out on all the quantitative variables, 
#describe the relationships between the different variables as well as the relationships between environmental gradients and observations / stations.

library(vegan)
library(ade4)


boxplot(BIO[,1:5])
#Transformation of `data`
#High range of variability. Need to use a log10(x+1) transformation
BIOt<-log(BIO[,1:5]+1)
#Step 2: Matrix of association using the Bray-Curtis similarity index
#####1.0 points#####
matdist<-vegdist(BIOt,method="bray")


#Step 5: Principal component Analysis
attach(BIO)
data(BIO)
library(rlang)
library(FactoMineR)
library(Factoshiny)
PCAshiny(BIO)
FactoMineR(BIO)

