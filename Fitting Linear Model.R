dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
dat <- read.table(paste(dataPath,'Week5_Test_Sample.csv',sep = '/'), header=TRUE)


dat$Output # output Y values
dat$Input # predictor X values




#Fit linear model #GeneralModel, 
GeneralModel<-lm(Output~Input,dat)
GeneralModel$coefficients
names(GeneralModel)


#analyze residuals 
estimatedResiduals <-GeneralModel$residuals









Subsample.Steeper.var<-
  data.frame(steeperInput.var=dat$Input,steeperOutput.var=rep(NA,nSample))
Subsample.Flatter.var<-
  data.frame(flatterInput.var=dat$Input,flatterOutput.var=rep(NA,nSample))






#separate the mixed models using the alternative approach. 

dat$Returns <- c( diff(dat$Output) / lag(dat$Output))
volatility_threshold <- 0.2  # Adjust as needed
dat$Returns[is.na(dat$Returns)] <- 0


Unscrambling.Sequence.Steeper.var <- dat$Returns > volatility_threshold



Subsample.Steeper.var<-
  data.frame(steeperInput.var=dat$Input,steeperOutput.var=rep(NA,nSample))
Subsample.Flatter.var<-
  data.frame(flatterInput.var=dat$Input,flatterOutput.var=rep(NA,nSample))



Subsample.Steeper.var[Unscrambling.Sequence.Steeper.var,2]<-
  dat[Unscrambling.Sequence.Steeper.var,2]
Subsample.Flatter.var[!Unscrambling.Sequence.Steeper.var,2]<-
  dat[!Unscrambling.Sequence.Steeper.var,2]




# Check the first 10 rows
head(cbind(Subsample.Steeper.var,Subsample.Flatter.var),10)



#Fit separate linear models mSteep and mFlat. 
mSteep <- lm(steeperOutput.var~steeperInput.var,Subsample.Steeper.var)
mSteep$coefficients

mFlat <- lm(flatterOutput.var~flatterInput.var,Subsample.Flatter.var)
mFlat$coefficients


#Variables GeneralModel,mSteep and mFlat are objects of class “lm”. 







res <- list( GeneralModel = GeneralModel,mSteep = mSteep,mFlat = mFlat)
saveRDS(res, file = paste(dataPath,'W5_result.rds',sep = '/'))





















#separate the mixed models using the alternative approach. 
# Create NA vectors
Train.Sample<-data.frame(trainInput=dat$Input,trainOutput=rep(NA,nSample))
Train.Sample.Steeper<-data.frame(trainSteepInput=dat$Input,
                                 trainSteepOutput=rep(NA,nSample))  
Train.Sample.Flatter<-data.frame(trainFlatInput=dat$Input,
                                 trainFlatOutput=rep(NA,nSample))  


head(cbind(dat,
           Train.Sample,
           Train.Sample.Steeper,
           Train.Sample.Flatter))


# Create selectors
Train.Sample.Selector<-dat$Input>=0
Train.Sample.Steeper.Selector<-Train.Sample.Selector&
  (dat$Output>GeneralModel$fitted.values)
Train.Sample.Flatter.Selector<-Train.Sample.Selector&
  (dat$Output<=GeneralModel$fitted.values)


# Select subsamples (Create training samples for steep and flat slopes)
Train.Sample[Train.Sample.Selector,2]<-dat[Train.Sample.Selector,2]
Train.Sample.Steeper[Train.Sample.Steeper.Selector,2]<-dat[Train.Sample.Steeper.Selector,2]
Train.Sample.Flatter[Train.Sample.Flatter.Selector,2]<-dat[Train.Sample.Flatter.Selector,2]
head(Train.Sample)




head(cbind(dat,
           Train.Sample,
           Train.Sample.Steeper,
           Train.Sample.Flatter),10)



#Fit separate linear models mSteep and mFlat. 
mSteep<-lm(trainSteepOutput~trainSteepInput,Train.Sample.Steeper)

summary(Train.Sample.Steep.lm)$coefficients
summary(Train.Sample.Steep.lm)$sigma
summary(Train.Sample.Steep.lm)$df
summary(Train.Sample.Steep.lm)$r.squared
summary(Train.Sample.Steep.lm)$adj.r.squared
summary(Train.Sample.Steep.lm)$fstatistic


mFlat<-lm(trainFlatOutput~trainFlatInput,Train.Sample.Flatter)

summary(Train.Sample.Flat.lm)$coefficients
summary(Train.Sample.Flat.lm)$sigma
summary(Train.Sample.Flat.lm)$df
summary(Train.Sample.Flat.lm)$r.squared
summary(Train.Sample.Flat.lm)$adj.r.squared
summary(Train.Sample.Flat.lm)$fstatistic




#Variables GeneralModel,mSteep and mFlat are objects of class “lm”. 



























