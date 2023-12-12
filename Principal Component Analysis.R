dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
test_dat <- read.table(paste(dataPath,'Week9_Test_Sample.csv',sep = '/'), header=TRUE)

typeof(test_dat)
test_dat$Resp # response values;
test_dat$Pred1 # predictor 1 values;
test_dat$Pred2 # predictor 2 values;
test_dat$Pred3 # predictor 3 values;
test_dat$Pred4 # predictor 4 values;
test_dat$Pred5 # predictor 5 values;
test_dat$Pred6 # predictor 6 values;
test_dat$Pred7 # predictor 7 values;
test_dat$Pred8 # predictor 8 values;
test_dat$Pred9 # predictor 9 values;
test_dat$Pred10 # predictor 10 values.


test_dat
data <- subset(test_dat, select = -Resp)
summary(data)

pca_result <-princomp(data)
loadings <- pca_result$loadings
summary(pca_result)






linearmodel <- lm(Resp ~ Pred1 + Pred2 + Pred3 + Pred4 + Pred5 + Pred6 + Pred7 + Pred8 + Pred9 + Pred10, data = test_dat)
summary(linearmodel)


principal_components <- as.data.frame(predict(pca_result))


linearmodel <- lm(Resp ~ Pred1 + Pred2 + Pred3 + Pred4 + Pred5 + Pred6 + Pred7 + Pred8 + Pred9 + Pred10, data = test_dat)
r_squared_full <- summary(linearmodel)$r.squared
target_r_squared <- 0.9 * r_squared_full   # = 0.874856



data_with_selected_pcs <- cbind(response = test_dat$Resp, principal_components)

model_with_selected_pcs <- lm(response ~ ., data = data_with_selected_pcs)
summary(model_with_selected_pcs)$r.squared

model_with_selected_pcs <- lm(response ~ Comp.1 + Comp.2 + Comp.3 + Comp.4, data = data_with_selected_pcs)
summary(model_with_selected_pcs)$r.squared

model_with_selected_pcs <- lm(response ~ Comp.4 + Comp.2 , data = data_with_selected_pcs)
summary(model_with_selected_pcs)$r.squared

model_with_selected_pcs <- lm(response ~ Comp.2 , data = data_with_selected_pcs)
summary(model_with_selected_pcs)$r.squared

