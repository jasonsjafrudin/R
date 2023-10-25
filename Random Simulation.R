#First section all from the Workshop Seminar
dataFromRandom<-read.table(paste("C:/Users/Jason Sjafrudin/Desktop/Stat Files","randombyte.txt", sep="/"))

dataFromRandom<-na.omit(unname(unlist(dataFromRandom)))
head(dataFromRandom)
length(dataFromRandom)

dataFromRandom<-(as.vector(sapply(dataFromRandom,function(z) head(intToBits(z),8)))==1)*1
head(dataFromRandom)
length(dataFromRandom)





bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

Binary.matrix<-matrix(dataFromRandom,ncol=10)
head(Binary.matrix)

dataFromRandom.dec<-apply(Binary.matrix,1,bitsToInt)/2^10
head(dataFromRandom.dec)

trueSample <- dataFromRandom.dec








#simple pseudo-random number generator algorithm known as the Linear Congruential Generator (LCG) in R:

# Initialize parameters
seed <- 12345
a <- 1664525
c <- 1013904223
m <- 2^32

# Function to generate pseudo-random numbers
generate_random <- function(seed, a, c, m, n) {
  random_numbers <- numeric(n)
  x <- seed
  
  for (i in 1:n) {
    x <- (a * x + c) %% m
    random_numbers[i] <- x / m
  }
  
  return(random_numbers)
}

# Generate a sequence of 10 pseudo-random numbers between 0 and 1
random_sequence <- generate_random(seed, a, c, m, 1000)
print(random_sequence)


pseudoSample <- random_sequence








res<-cbind(pseudoSample=pseudoSample,trueSample=trueSample)

saveRDS(res,paste("C:/Users/Jason Sjafrudin/Desktop/Stat Files",'resultS3.rds',sep='/'))
















#Simulate Normal (mu, sigma)
U_sample <- runif(1000)
hist(U_sample)
N_sample <-qnorm(u_sample,2,5)
hist(N_sample)