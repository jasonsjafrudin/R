dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
data<-read.csv(file=paste(dataPath,"Method_Moments_Data.csv",sep="/"))

data$A #- sample A from one-dimensional distribution;
data$B #- sample B from one-dimensional distribution;
data$Y #- sample Y, response of a linear model;
data$X #- sample X, predictor of a linear model.


# Explore the distribution of sample A
hist(data$A, main = "Histogram of Sample A", xlab = "Values")

# Estimate parameters for a normal distribution for sample A
mu_A <- mean(data$A)
sigma_A <- sd(data$A)

# Test the fit of the normal distribution for sample A
ks_test_A <- ks.test(data$A, "pnorm", mean = mu_A, sd = sigma_A)


# Distribution of A
distribution_A <- "Normal"  # Replace with the actual distribution name

# Selected distribution for the sample A
selected_distribution_A <- "Normal"

# Parameters for sample A
A_param1 <- round(mu_A, 3)
A_param2 <- round(sigma_A, 3)

# D-statistic of Kolmogorov-Smirnov test for sample A
D_A <- ks_test_A$statistic  # D-statistic for sample A










# Explore the distribution of sample B
hist(data$B, main = "Histogram of Sample B", xlab = "Values")

# Estimate parameters for a uniform distribution for sample B
a_B <- min(data$B)
b_B <- max(data$B)

# Test the fit of the uniform distribution for sample B
ks_test_B <- ks.test(data$B, "punif", min = a_B, max = b_B)


# Distribution of B
distribution_B <- "Uniform"

# Selected distribution for the sample B
selected_distribution_B <- "Uniform"

# Parameters for sample B
B_param1 <- round(a_B, 3)
B_param2 <- round(b_B, 3)

# D-statistic of Kolmogorov-Smirnov test for sample B
D_B <- ks_test_B$statistic  # D-statistic for sample B
round(D_B,3)







# Calculate sample moments for Y and X
mean_Y <- mean(data$Y)
mean_X <- mean(data$X)
var_Y <- var(data$Y)
var_X <- var(data$X)
cov_YX <- cov(data$Y, data$X)
n <- length(data$Y)

# Estimate the slope (beta) and intercept (alpha)
beta <- cov_YX / var_X
alpha <- mean_Y - beta * mean_X

# Calculate the residuals
residuals <- data$Y - (alpha + beta * data$X)
# Estimate the residual standard deviation (sigma)
n <- length(residuals)
sigma <- sqrt(sum(residuals^2) / (n - 2))


# Variables
Slope <- round(beta, 3)
Intercept <- round(alpha, 3)
Sigma <- round(sigma, 3)



