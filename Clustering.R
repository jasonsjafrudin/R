dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
dat <- read.table(paste(dataPath,'Week4_Test_Sample.csv',sep = '/'), header=TRUE)

dat$X
dat$Y

plot(dat$X,dat$Y)


#Fit linear model, 
Estimated.LinearModel <- lm(Y ~ X,data=dat)
names(Estimated.LinearModel)

#analyze residuals 
Estimated.Residuals <- Estimated.LinearModel$residuals


#separate the mixed sample into 2 subsamples: one, for which the residuals are below zero and another, for which they are above zero. 
c(Left.Mean = mean(Estimated.Residuals[Estimated.Residuals < 0]), 
  Right.Mean = mean(Estimated.Residuals[Estimated.Residuals > 0]))



#Create variable Unscrambled.Selection.Sequence using the same method as you did in Section 3 
# Create a vector of 1s for positive residuals and 0s for negative residuals
Unscrambled.Selection.Sequence <- ifelse(Estimated.Residuals > 0, 1, 0)



#save it in file result.csv Create list variable res
res <- list(Unscrambled.Selection.Sequence =  Unscrambled.Selection.Sequence)


#Save res to a file and upload the file using left sidebar.
write.table(res, file = paste(dataPath,'W4_Test_result.csv',sep = '/'), row.names = F)
