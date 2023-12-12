dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
data <- read.csv(paste(dataPath, "W8_G_test_sample.csv", sep="/"))

#Column x contains dependent variable, and t is a column of treatments factor.





#Task
#Use ANOVA model with contrasts to find out which of the treatments are statistically indistinguishable.
#Use methods aov() and summary.lm() (possibly, several times) or TukeyHSD() to test the hypothesis of the same means for a pair of different treatments. Use pvalue=0.01 as a threshold. Check if both methods give the same results.
#Fill the partition of the set of all treatments in the corresponding field of Quiz Tab



# Fit an ANOVA model with contrasts
model_anova <- aov(x ~ t, data = data)


# Perform Tukey's HSD test
tukey_test <- TukeyHSD(model_anova, conf.level = 0.99)







#Check the Gaussian hypothesis using Kolmogorov-Smirnov test.
ks.test(model_anova$residuals,"pnorm") #doesnt show a strong gaussian distribution

plot(model_anova$fitted.values,model_anova$residuals)

















# Fit an ANOVA model
model_anova <- aov(x ~ t, data = dat)


cat("The partition of all treatment levels with no significance in difference of means:", no_significance_string, "\n")




