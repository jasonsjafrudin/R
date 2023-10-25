dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
dat <- read.table(paste(dataPath,'Week3_Test_Sample.csv',sep = '/'), header=TRUE)



dat$x[1] # mean value of normal distribution;
dat$x[2] # standard deviation of normal distribution;
dat$x[3] # intensity of exponential distribution;
dat$x[4] : dat$x[503] # sample from uniform distribution on [0.1].


# Specify the mean and standard deviation
mean_value <- dat$x[1]
sd_value <- dat$x[2]
# Extract the specific range of data from dat (uniform sample)
uniform_sample <- dat$x[4:503]
# Use qnorm to transform the uniform sample into a normal distribution
# qnorm(): This line uses the qnorm() function to generate a new sample in an normal distribution.
#Generating a new sample from a probability distribution, such as an normal distribution, means creating a set of random values that follow the characteristics of that distribution. 
datNorm_t <- qnorm(uniform_sample, mean = mean_value, sd = sd_value)
datNorm <- as.vector(datNorm_t)



# Specify the intensity (rate parameter)
intensity <- dat$x[3]
# qexp(): This line uses the qexp() function to generate a new sample in an exponential distribution.
#Generating a new sample from a probability distribution, such as an exponential distribution, means creating a set of random values that follow the characteristics of that distribution. 
datExp_t <- qexp(subset_of_data, rate = intensity)
datExp <- as.vector(datExp_t)


res<-cbind(datNorm=datNorm,datExp=datExp)
write.table(res, file = paste("C:/Users/Jason Sjafrudin/Desktop/Stat Files",'W3_Test_result.csv',sep = '/'), row.names = F)













