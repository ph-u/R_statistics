#!/bin/env Rscript

# Author 	: PokMan Ho (hpokman@connect.hku.hk)
# Script 	: Rcode.R
# Input 	: None
# Argument 	: None
# Output 	: graphs (png)
# Date 		: Jan 2020

## colour
cbp <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#e79f00", "#9ad0f3", "#F0E442", "#999999", "#cccccc", "#6633ff", "#00FFCC", "#0066cc")

{ # set sample df
  set.seed(998) # fix random number generation algorithm, for code reproducibility
  a<-as.data.frame(matrix(nrow = 1e3, ncol = 4)) # initialize empty dataframe
  colnames(a)=c("dep", "indep_1", "indep_2", "indep_3")
  a$dep<-runif(nrow(a)) # sample number of values meet number of rows of dataframe \textbf{a} from uniform distribution between 0 and 1
  a$indep_1<-c("a","b") # repeating "a", "b" pattern until fill up the dataframe (length difference must be in multiples)
  a$indep_2<-sample(c("a","c","d","e"),nrow(a), replace = T) # sample number of values meet number of rows of dataframe \textbf{a} from designated pool with element replacement after each element selection
  a$indep_3<-rnorm(nrow(a)) # sample number of values meet number of rows of dataframe \textbf{a} from normal distribution with mean 0, sd 1
}

a0<-a[which(a$indep_2=="a" & a$indep_1!="a"),]
a1<-a[which(a$indep_2=="a" | a$indep_1!="a"),]

kruskal.test(a$dep~a$indep_1) # y against x
kruskal.test(a$dep~interaction(a$indep_1,a$indep_2,a$indep_3)) # y against combined independent variables x1, x2, x3
wilcox.test(a$dep~a$indep_1) # y against x, with x only have two levels
cor.test(a$indep_3, a$dep, method="spearman") # x, y, method of testing
library(PMCMR)
posthoc.kruskal.nemenyi.test(a$dep~interaction(a$indep_1,a$indep_2,a$indep_3))
posthoc.kruskal.nemenyi.test(a$dep~as.factor(a$indep_1)) # meet code requirement
aov(a$dep~a$indep_2)
summary(aov(a$dep~a$indep_2)) # get a summary form of the y against x ANOVA test
summary(lm(a$dep~a$indep_1+a$indep_2*a$indep_3^2))
# a basic summary of all parametric methods introduced in this note - they are all linear models
# this code in a summary of a linear model with a formula of y against independent x1 and interactive x2 and x3
summary(aov(a$dep~a$indep_1+a$indep_2*a$indep_3^2)) # same formula as the above linear model code, just another approach
t.test(a$dep~a$indep_1, paired=T) # y against x which only have one or two levels
cor.test(a$indep_3, a$dep, method="pearson") # same with spearman above, only switched the method parameter

shapiro.test(a$dep)

library(car)
png("graph/qqPlot.png")
qqPlot(a$dep) # indicated mean with 95\% interval
dev.off()

png("graph/histogram.png")
hist(a$dep)
dev.off()

b<-seq(1e3) # get a number sequence starting from 1, having 1e3 number of elements
b<-data.frame("time"=b, # incorporate the sequence into data frame
              "y1"=runif(length(b))*10, # generate random numbers with elements same with length of the sequence
              "y2"=rnorm(length(b), mean = 10, sd = 5), # same number of elements, from a normal distribution
              "y3"=sample(c(0,2,4), length(b), replace = T)) # sample that number of elements from the given pool with element replacement
png("graph/matplot.png", width = 1000)
matplot(x=b$time, # x-axis
        y=b[,-1], # y-elements, columnwise excluding first column
        type="l", # line-type plot
        col = cbp, # line/point colour according to pre-defined colour-palette
        xlab = "time unit", # manual set x-label
        ylab = "values", # manual set y-label
        main="matplot demonstration") # manual set chart title
legend("topleft", # use a pre-defined legend position
       legend = c("y1", "y2", "y3"), # group labels
       pch = rep(16,3), # dot style indicating group in the graph
       col = cbp) # colour of dots indicating for the groups
dev.off()

png("graph/matplot1.png", width = 1000)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
matplot(x=b$time, y=b[,-1], type="l", col = cbp, xlab = "time unit", ylab = "values", main="matplot demonstration")
legend("topright", inset=c(-.05,0), legend = c("y1", "y2", "y3"), pch = rep(16,3), col = cbp)
dev.off()

## heatmap in lattice pkg
library(lattice)
set.seed(998)
y=c();for(i in -3:6){y<-c(y,rep(i,10))};rm(i)
c0 = data.frame("x"=rep(-5:4,10),"y"=y,"z"=runif(100)-.5);rm(y)
c1 = data.frame("x"=c("A","B","C"),"y"=c(rep("A",3),rep("B",3),rep("C",3)),"z"=runif(9)-.2)
c2 = data.frame("x"=rep(-1:1,3),"y"=c(rep("A",3),rep("B",3),rep("C",2),"D"),"z"=runif(9)-.2)

png("graph/lvPlt1.png")
levelplot(c0$z~c0$x*c0$y, col.regions = rev(gray(0:70/100)), xlab = "x-axis", ylab = "y-axis", main="levelplot from dataframe c0")
dev.off()
png("graph/lvPlt2.png")
levelplot(c1$z~c1$x*c1$y, col.regions = rev(gray(0:80/100)), xlab = "x-axis", ylab = "y-axis", main="levelplot from dataframe c1")
dev.off()
png("graph/lvPlt3.png")
levelplot(c2$z~c2$x*c2$y, col.regions = rev(gray(0:50/100)), xlab = "x-axis", ylab = "y-axis", main="levelplot from dataframe c2")
dev.off()