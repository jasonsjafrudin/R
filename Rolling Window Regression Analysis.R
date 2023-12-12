library(zoo)
datapath <- '/Users/An/Downloads'
test_data <-read.csv(file=paste(datapath,"test_sample.csv",sep="/"), row.names=1,header=TRUE,sep=",")
Window.width<-20
Window.shift<-5

lagdiff <- diff(as.matrix(test_data))
head(lagdiff)

rolling.sd <- rollapply(lagdiff,
                        width=Window.width,
                        by=Window.shift,
                        by.column=TRUE, 
                        FUN=sd)
rolling.dates <- rollapply(test_data[-1,],
                           width=Window.width,
                           by=Window.shift,
                           by.column=FALSE,
                           FUN=function(z) rownames(z))
rownames(rolling.sd) <- rolling.dates[,10]

high.volatility.periods <- rownames(rolling.sd)[rolling.sd[,8]>.3]


coefficients <- rollapply(test_data,
                          width=Window.width,
                          by=Window.shift,
                          by.column=FALSE,
                          FUN=function(z) coef(lm(Output1~USGG3M+USGG5YR+USGG30YR,
                                                  data=as.data.frame(z))))
rolling.dates <- rollapply(test_data[,1:8],
                           width=Window.width,
                           by=Window.shift,
                           by.column=FALSE,
                           FUN=function(z) rownames(z))

rownames(coefficients) <- rolling.dates[,10]

(high.slopeSpread.periods <- rownames(coefficients)[abs(coefficients[,3] - coefficients[,4]) > 3])

(high.slope5Y <- rownames(coefficients)[coefficients[,3] > 2.5])
r.squared <- rollapply(test_data,
                       width=Window.width,
                       by=Window.shift,
                       by.column=FALSE,
                       FUN=function(z) summary(lm(Output1~USGG3M+USGG5YR+USGG30YR,
                                                  data=as.data.frame(z)))$r.squared)
r.squared <- cbind(rolling.dates[,10], r.squared)

(low.r.squared <- r.squared[r.squared[,2] < 0.9, 1])

pvalues <- rollapply(test_data,
                     width=Window.width,
                     by=Window.shift,
                     by.column=FALSE,
                     FUN=function(z) summary(lm(Output1~USGG3M+USGG5YR+USGG30YR,
                                                data=as.data.frame(z)))$coefficients[,4])
rownames(pvalues) <- rolling.dates[,10]
pvalues[1:10,]

USGG3M_insignificant  <- rownames(pvalues)[pvalues[,2] > 0.05]
USGG5Y_insignificant  <- rownames(pvalues)[pvalues[,3] > 0.05]
USGG30Y_insignificant <- rownames(pvalues)[pvalues[,4] > 0.05]



res <- list(high.volatility.periods=high.volatility.periods,
            high.slopeSpread.periods=high.slopeSpread.periods,
            high.slope5Y=high.slope5Y,
            low.r.squared = low.r.squared,
            USGG3M_insignificant=USGG3M_insignificant,
            USGG5Y_insignificant=USGG5Y_insignificant,
            USGG30Y_insignificant=USGG30Y_insignificant)

saveRDS(res, file = paste(datapath,'result_1.rds',sep = '/'))

