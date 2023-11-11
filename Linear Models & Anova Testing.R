dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
test_dat  <- read.table(paste(dataPath,'Week7_test_sample.csv',sep = '/'), header=TRUE)

test_dat$Output #output Y values;
test_dat$Input1 #first predictor values;
test_dat$Input2 #second predictor values.

#Fit the data with models fit.1, fit.1.2, fit1.3, fit.1.2.3 as in Section 1.2
fit.1<-lm(Output~1,data=test_dat)
fit.1.2<-lm(Output~1+Input1,data=test_dat)
fit.1.3<-lm(Output~1+Input2,data=test_dat)
fit.1.2.3<-lm(Output~.,data=test_dat)


#1) Sum of Squares explained by Input1 in model fit.1.2 :
anova(fit.1.2)
313.16

#2) Sum of Squares unexplained by Input1 in model fit.1.2: 
anova(fit.1.2)
151.13


#3) Sum of Squares explained by Input2 in model fit.1.3:
anova(fit.1.3)
3.72

#4) Sum of Squares unexplained by Input2 in model fit.1.3:
anova(fit.1.3)
460.57


#5) F statistic for comparison of fit.1 and fit.1.2.3:
# The F-statistic is calculated based on the difference in the residual sum of squares (RSS) between the two models.
anova_result <- anova(fit.1, fit.1.2.3)

f_value <- anova_result$F[2]
515.0699

#6) P-value for comparison of fit.1 with fit.1.2.3:
p_value <- anova_result$Pr[2]
7.090676e-122

#7, remove input 2 because p value is high means it doesnt reject null hypothesis that adding this respective variable contribute significantly to explaining the dependent variable



