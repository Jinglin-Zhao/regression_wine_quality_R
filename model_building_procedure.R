setwd("C:\\Users\\Lenovo\\Desktop\\regression\\Project")
indicator <- function(model, y_pred, y_true) {
  adj.r.sq <- summary(model)$adj.r.squared
  mse <- MSE(y_pred, y_true)
  rmse <- RMSE(y_pred, y_true)
  mae <- MAE(y_pred, y_true)
  print(paste0("Adjusted R-squared: ", round(adj.r.sq, 4)))
  print(paste0("MSE: ", round(mse, 4)))
  print(paste0("RMSE: ", round(rmse, 4)))
  print(paste0("MAE: ", round(mae, 4)))
}

metrics <- function(y_pred, y_true){
  mse <- MSE(y_pred, y_true)
  rmse <- RMSE(y_pred, y_true)
  mae <- MAE(y_pred, y_true)
  print(paste0("MSE: ", round(mse, 6)))
  print(paste0("RMSE: ", round(rmse, 6)))
  print(paste0("MAE: ", round(mae, 6)))
  corPredAct <- cor(y_pred, y_true)
  print(paste0("Correlation: ", round(corPredAct, 6)))
  print(paste0("R^2 between y_pred & y_true: ", round(corPredAct^2, 6)))
}

CheckNormal <- function(model) {
  hist(model$residuals, breaks = 30)
  shaptest <- shapiro.test(model$residuals)
  print(shaptest)
  if (shaptest$p.value <= 0.05) {
    print("H0 rejected: the residuals are NOT distributed normally")
  } else {
    print("H0 failed to reject: the residuals ARE distributed normally")
  }
}

library(lmtest)
CheckHomos <- function(model){
  plot(model$fitted.values, model$residuals)
  abline(h = 0, col = "red")
  BP <- bptest(model)
  print(BP)
  if (BP$p.value <= 0.05) {
    print("H0 rejected: Error variance spreads INCONSTANTLY/generating patterns (Heteroscedasticity)")
  } else {
    print("H0 failed to reject: Error variance spreads CONSTANTLY (Homoscedasticity)")
  }
}
redDat = read.csv("winequality_red.csv", sep = ";", header = T)
redDat
install.packages("rmdformats")
library("rmdformats") 
library("corrgram")
library("MASS")
library("ggplot2")
install.packages("naniar")
library("naniar")
library("e1071")
library("lattice")
#histogram
attach(redDat)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(quality)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)


truehist(fixed.acidity, h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(volatile.acidity, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(citric.acid, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)




truehist(residual.sugar, h = 0.5, col="slategray3")
mtext("Residual Sugar", side=1, outer=F, line=2, cex=0.8)

truehist(chlorides, h = 0.01, col="slategray3")
mtext("Chloride", side=1, outer=F, line=2, cex=0.8)

truehist(alcohol, h = 0.25, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)


truehist(density, h = 0.0005, col="slategray3")
mtext("Density", side=1, outer=F, line=2, cex=0.8)


par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(free.sulfur.dioxide, h = 5, col="slategray3")
mtext("Free Sulfur Dioxide", side=1, outer=F, line=2, cex=0.8)

truehist(pH, h = 0.1, col="slategray3")
mtext("pH values", side=1, outer=F, line=2, cex=0.8)

truehist(sulphates, h = 0.1, col="slategray3")
mtext("sulphates", side=1, outer=F, line=2, cex=0.8)


truehist(total.sulfur.dioxide, h = 10, col="slategray3")
mtext("total.sulfur.dioxide", side=1, outer=F, line=2, cex=0.8)

# drawing part 1 box plot
par(mfrow=c(1,5), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)

boxplot(fixed.acidity, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)

boxplot(volatile.acidity, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)

boxplot(citric.acid, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)

boxplot(residual.sugar, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)

boxplot(chlorides, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
# drawing part 2 box plot
par(mfrow=c(1,6), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)

boxplot(alcohol, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)

boxplot(density, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)

boxplot(free.sulfur.dioxide, col="slategray2", pch=19)
mtext("free.sulfur.dioxide", cex=0.8, side=1, line=2)

boxplot( pH, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)

boxplot(sulphates, col="slategray2", pch=19)
mtext("sulphates", cex=0.8, side=1, line=2)




boxplot(total.sulfur.dioxide, col="slategray2", pch=19)
mtext("total.sulfur.dioxide", cex=0.8, side=1, line=2)

# skewness
attach(redDat)
skewness(quality)
skewness(chlorides)
skewness(free.sulfur.dioxide)
skewness(residual.sugar)
skewness(alcohol)
skewness(citric.acid)
skewness(density)
skewness(fixed.acidity)
skewness(volatile.acidity)
skewness(total.sulfur.dioxide)
skewness(sulphates)
skewness(pH)
detach(redDat)
# transformation of dataset
preprocess_red <- preProcess(redDat[,1:11], c("BoxCox", "center", "scale"))
new_red <- data.frame(trans = predict(preprocess_red, redDat))
colnames(new_red)
skewness(new_red$trans.quality)
skewness(new_red$trans.chlorides)
skewness(new_red$trans.free.sulfur.dioxide)
skewness(new_red$trans.residual.sugar)
skewness(new_red$trans.alcohol)
skewness(new_red$trans.citric.acid)
skewness(new_red$trans.density)
skewness(new_red$trans.fixed.acidity)
skewness(new_red$trans.volatile.acidity)
skewness(new_red$trans.total.sulfur.dioxide)
skewness(new_red$trans.sulphates)
skewness(new_red$trans.pH)
# move outliers
new_red <- new_red[!abs(new_red$trans.fixed.acidity) > 3,]
new_red <- new_red[!abs(new_red$trans.volatile.acidity) > 3,]
new_red <- new_red[!abs(new_red$trans.citric.acid) > 3,]
new_red <- new_red[!abs(new_red$trans.residual.sugar) > 3,]
new_red <- new_red[!abs(new_red$trans.chlorides) > 3,]
new_red <- new_red[!abs(new_red$trans.density) > 3,]
new_red <- new_red[!abs(new_red$trans.pH) > 3,]
new_red <- new_red[!abs(new_red$trans.sulphates) > 3,]
new_red <- new_red[!abs(new_red$trans.alcohol) > 3,]

boxplot(new_red$trans.alcohol, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)

truehist(new_red$trans.chlorides, h = 0.25, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)

library(ggcorrplot)
corr <- cor(new_red )
ggcorrplot(corr, hc.order = FALSE, type  = "lower",
           lab = TRUE)
new_red
#full model
full <- lm(trans.quality ~ ., new_red)
summary(full)
library(car)
vif(full)
linear_1 <- lm(trans.quality ~ . -trans.density , new_red)
summary(linear_1)




# check normality
library(car)
CheckNormal(model_red3)
CheckHomos(model_red3)
library(car)
vif(full)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2 
lapply(c(1,2,4,5), # showing 4 types of plots
       function(x) plot(full, 
                        which = x, 
                        # labels.id = 1:nrow(X.train_red),
                        cook.levels = c(0.05, 0.1))) %>% invisible()
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
plot(full)
to.rm <- c(391)
# X.train_red[to.rm,]
new_new_red<- new_red[-to.rm,]
#full model
full <- lm(trans.quality ~ ., new_new_red)
summary(full)
#step wise tp get the model
steplinear <- step(full)
summary(steplinear)
#
library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 3)

#fit a regression model and use k-fold CV to evaluate performance
model <- train( trans.quality ~ trans.volatile.acidity + trans.citric.acid + 
                 trans.chlorides + trans.free.sulfur.dioxide + trans.total.sulfur.dioxide + 
                 trans.pH + trans.sulphates + trans.alcohol, data = new_red, method = "lm", trControl = ctrl)

#view summary of k-fold CV               
print(model)




library(mixlm)
forward_model <- forward(full, alpha = 0.05, full = TRUE)
forward_model
summary(forward_model)
#forward(full, alpha = 0.10, full = TRUE)
backward_model <- backward(full, alpha = 0.05, full = TRUE, hierarchy = TRUE)
backward_model
summary(backward_model)
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
plot(backward_model)
#backward(full, alpha = 0.15, full = TRUE, hierarchy = TRUE)
stepwise_model <- stepWise(full, alpha.enter = 0.05, alpha.remove = 0.10, full = TRUE)
# predicting - create training dataset/ test dataset
set.seed(1)
sampleSize <- round(nrow(new_red)*0.8)
idx <- sample(seq_len(sampleSize), size = sampleSize)
X.train_red <- new_red[idx,]
X.test_red <- new_red[-idx,]
X.train_red
rownames(X.train_red) <- NULL
rownames(X.test_red) <- NULL
#training model on training dataset
model_red1 <- lm(trans.quality ~ ., X.train_red)
summary(model_red1)
CheckNormal(model = model_red3)
CheckHomos(model = model_red3)
library(car)
vif(model_red1)
#check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2 
lapply(c(1,2,4,5), # showing 4 types of plots
       function(x) plot(model_red3, 
                        which = x, 
                        # labels.id = 1:nrow(X.train_red),
                        cook.levels = c(0.05, 0.1))) %>% invisible()
to.rm <- c(416,869,1036)
# X.train_red[to.rm,]
X.train_red <- X.train_red[-to.rm,]
rownames(X.train_red) <- NULL
# remove after that. build the model 2
model_red2 <- lm(trans.quality ~ ., X.train_red)
summary(model_red2)
Anova(model_red2)
#check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2 
lapply(c(1,2,4,5), # showing 4 types of plots
       function(x) plot(model_red3, 
                        which = x, 
                        # labels.id = 1:nrow(X.train_red),
                        cook.levels = c(0.05, 0.1))) %>% invisible()
to.rm <- c(1205,432,751)
# X.train_red[to.rm,]
X.train_red <- X.train_red[-to.rm,]
rownames(X.train_red) <- NULL
model_red3 <- lm(trans.quality ~ ., X.train_red)
summary(model_red3)
Anova(model_red3)
print("Adjusted R-squared for 1st model:")
ad.r.sq1 <- summary(model_red1)$adj.r.squared
ad.r.sq1
print("Adjusted R-squared for 2nd model:")
ad.r.sq2 <- summary(model_red2)$adj.r.squared
ad.r.sq2
print(paste0("The difference between both is ", round(ad.r.sq2-ad.r.sq1, 5)*100, "%"))
library(car)
vif(model_red1)
vif(model_red2)

model_redAlc <- lm(trans.quality ~ trans.alcohol, data = X.train_red)
summary(model_redAlc)
model_redAll <- lm(trans.quality ~ ., data = X.train_red)
summary(model_redAll)
step(model_redAll, direction = "backward", trace = F)
model.back_red <- lm(formula = trans.quality ~ trans.volatile.acidity + trans.citric.acid + 
                       trans.chlorides + trans.free.sulfur.dioxide + trans.total.sulfur.dioxide + 
                       trans.pH + trans.sulphates + trans.alcohol, data = X.train_red)
summary(model.back_red)
step(model_redAlc, scope = list(lower = model_redAlc, upper = model_redAll),
     direction = "forward",
     trace = F)
model.forw_red <- lm(formula = trans.quality ~ trans.alcohol + trans.volatile.acidity + 
                       trans.sulphates + trans.total.sulfur.dioxide + trans.pH + 
                       trans.free.sulfur.dioxide + trans.citric.acid + trans.chlorides, 
                     data = X.train_red)
summary(model.forw_red)
anova(model.forw_red)
#final working model
final_model <- lm(formula = trans.quality ~ trans.alcohol + trans.volatile.acidity + 
     trans.sulphates + trans.total.sulfur.dioxide + trans.pH + 
     trans.free.sulfur.dioxide + trans.citric.acid, 
   data = X.train_red)
summary(final_model)
anova(final_model)



step(model_redAlc, scope = list(lower = model_redAlc, upper = model_redAll),
     direction = "both",
     trace = F)
model.both_red <- lm(formula = trans.quality ~ trans.alcohol + trans.volatile.acidity + 
                       trans.sulphates + trans.total.sulfur.dioxide + trans.pH + 
                       trans.free.sulfur.dioxide + trans.citric.acid + trans.chlorides, 
                     data = X.train_red)
summary(model.both_red)
# training acuraccy 
distPred <- predict(model_redAll, X.train_red)  
distPred1 <- ceiling(distPred)
trn_tab <- table(predicted = distPred1, actual = X.train_red$trans.quality)
trn_tab
sum(diag(trn_tab))/length(X.test_red$trans.quality)
#testing accuracy
distPred <- predict(model_redAll, X.test_red)  
distPred1 <- ceiling(distPred)
tst_tab <- table(predicted = distPred1, actual = X.test_red$trans.quality)
sum(diag(tst_tab))/length(X.test_red$trans.quality)
### ordinal logistic model
new_red$category[new_red$trans.quality <= 5] <- 0
new_red$category[new_red$trans.quality > 5] <- 1
new_red
new_red$category <- as.factor(new_red$category)

# predicting - create training dataset/ test dataset
set.seed(1)
sampleSize <- round(nrow(new_red)*0.8)
idx <- sample(seq_len(sampleSize), size = sampleSize)
X.train_red <- new_red[idx,]
X.test_red <- new_red[-idx,]
X.train_red
rownames(X.train_red) <- NULL
rownames(X.test_red) <- NULL
model_glm <- glm(category ~ . -trans.quality, data = X.train_red, family=binomial(link = "logit"))
model_gl <- step(model_glm)
trn_pred <- ifelse(predict(model_gl, type = "response") > 0.5,"Good Wine", "Bad Wine")
head(trn_pred)
trn_tab <- table(predicted = trn_pred, actual = X.train_red$category)
trn_tab
sum(diag(trn_tab))/length(X.train_red$category)
# Making predictions on the test set.
tst_pred <- ifelse(predict(model_gl, newdata = X.test_red, type = "response") > 0.5, "Good Wine", "Bad Wine")
tst_tab <- table(predicted = tst_pred, actual = X.test_red$category)
tst_tab
sum(diag(tst_tab))/length(X.test_red$category)
Anova(model_gl)

final_model$coefficients

# Print the ANOVA table to the console
print(anova_result)

