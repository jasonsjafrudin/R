dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
test_data <- read.table(paste(dataPath,'Week4_Test_Sample.csv',sep = '/'), header=TRUE)

min(test_data$x)
max(test_data$x)

#Apply Benford’s Law analysis of the first 2 digits to identify suspicious data in the sample.
install.packages('benford.analysis')
library(benford.analysis)

bfd.cp <- benford(test_data$x)
names(bfd.cp)



# Create variable numbers - array of length 6. 
# (Each element of the array shows the two digits selected from the table of digits sorted by
# differences between the observed and theoretical values. 
#The index for selecting the sorted digits is given below.
# 90 , 1 , 89 , 2 , 88 , 3
#88 , 7 , 90 , 1 , 89 , 2



bfd.cp <- benford(test_data$x) 
Suspects_Table <- suspectsTable(bfd.cp)

benford_Res <- benford(test_data$x, number.of.digits = 2, discrete = TRUE)

dig<-extract.digits(test_data$x, number.of.digits = 2)
observed <- table(dig[,2])
probobserved <- observed/sum(observed)
head(cbind(observed,probobserved))
dbenford <- sapply(10:99, function(z) p.these.digits(z))
diff = probobserved -dbenford

sorted_diffs_indices <- order(diff, decreasing = TRUE)

index <- c(88,7, 90, 1, 89, 2)
numbers<- names(probobserved)[sorted_diffs_indices][index]
numbers <- as.numeric(numbers)



# Create variable Chi_square_p.value - Pearson’s χ2 Test test p-value;
Chi_square_p.value <- bfd.cp$stats$chisq$p.value

# Create variable Mantissa_p.value - Mantissa Arc Test p-value;
Mantissa_p.value <- bfd.cp$stats$mantissa.arc.test$p.value

# Create variable MAD - Mean Absolute Deviation (MAD) value;
MAD <- MAD(bfd.cp) # = mean(abs(probObserved-dbenford))

# Create variable MAD.conformity- Conformity test designed by Dr. Nigrini.
MAD.conformity <- bfd.cp$MAD.conformity


res <- list(numbers=numbers,
            Chi_square_p.value = Chi_square_p.value,
            Mantissa_p.value = Mantissa_p.value,
            MAD = MAD,
            MAD.conformity  = MAD.conformity
)


saveRDS(res, file = paste(dataPath,'resultG5.rds',sep = '/'))




