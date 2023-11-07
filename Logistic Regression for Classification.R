dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"

train_dat <- read.table(paste(dataPath,'Week6_Test_Sample_Train.csv',sep = '/'), header=TRUE)
main_dat <- read.table(paste(dataPath,'Week6_Test_Sample_Test.csv',sep = '/'), header=TRUE)



train_dat$Output # output Y values;
train_dat$Input # predictor X values;
train_dat$Selection.Sequence # vector of 0 and 1.


main_dat$Output # output Y values;
main_dat$Input # predictor X values;


#####wrong#####
LinearModel.Training.Logistic<-glm(train_dat$Selection.Sequence ~ train_dat$Output + train_dat$Input,data=train_dat,
                                   family=binomial(link=logit))
summary(LinearModel.Training.Logistic)


Unscrambling.Sequence.Logistic<-(predict(LinearModel.Training.Logistic,                                         
                                         newdata=data.frame(Logistic.Output=main_dat$Output,                              
                                                            Logistic.Input=main_dat$Input),
                                         type="response")>.5)*1
########################################################################
### the issue with the above is you specify  the dataset name (train_dat$) in the model formula when training the logistic regression model. This led to an incorrect specification of the model and caused issues when making predictions on a different dataset (main_dat).



LinearModel.Training.Logistic <- glm(Selection.Sequence ~ Output + Input, data = train_dat, family = binomial(link = logit))
summary(LinearModel.Training.Logistic)

Unscrambling.Sequence.Logistic <- (predict(LinearModel.Training.Logistic, 
                                           newdata = main_dat, type = "response") > 0.5) * 1







res <- list(Unscrambling.Sequence.Logistic =  Unscrambling.Sequence.Logistic)
write.table(res, file = paste(dataPath,'W6_TestResult.csv',sep = '/'), row.names = F)

















