dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
df <- read.table(paste(dataPath,'Week2_Test_Sample.csv',sep = '/'), header=TRUE)

sdX <- round(sd(df$x), digits = 2)
sdY <- round(sd(df$y), digits = 2)
cXY <- round(cor(df$x, df$y), digits = 2)

#To calculate slope for a regression line, you'll need to divide the standard deviation of y values 
#by the standard deviation of x values and then multiply this by the correlation between x and y.
a <- (sdY/sdX) * cXY

result <- data.frame(sdX=sdX, sdY=sdY, cXY=cXY, a=a)  

write.table(result, file = paste("C:/Users/Jason Sjafrudin/Desktop/Stat Files",'W2_Test_result.csv',sep = '/'), row.names = F)
