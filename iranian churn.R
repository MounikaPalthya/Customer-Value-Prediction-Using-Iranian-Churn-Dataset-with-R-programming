getwd()
setwd("C:/Users/mouni/Downloads")
data=read.csv("churn.csv")
View(data)
plot(data)
head(data)
dim(data)
names(data)
summary(data)
cor(data)
hist(data$Customer.Value)
boxplot(data$Customer.Value ~ data$Tariff.Plan)
plot(data$Age, data$Customer.Value)
library(ggplot2)
ggplot(data, aes(x = Age, y = Customer.Value, color = Tariff.Plan)) + geom_point()
aggregate(Customer.Value ~ Age.Group, data = data, FUN = mean)


#training and testing
set.seed(1)
train <- sample(3150,2150)
test <- (1:3150)[-train]
length(test)

# Create training and testing sets
train_data <- polyX[train, ]
test_data <- polyX[test, ]
train_outcome <- Y[train]
test_outcome <- Y[test]

Customer=data$Customer.Value
summary(Customer)

#regression subsets
library(leaps)
m1<-regsubsets(Customer.Value~.,data=data)
summary(m1)
m2<-regsubsets(Customer.Value~.,data=data,nvmax=14)
s=summary(m2)
names(s)
par(mfrow=c(2,2))
s$adjr2
s$bic
s$rss
s$rsq
plot(s$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(s$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(s$adjr2)
points(11,s$adjr2[11], col="red",cex=2,pch=20)
plot(s$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(s$cp)
points(9,s$cp[9],col="red",cex=2,pch=20)
which.min(s$bic)
plot(s$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(9,s$bic[9],col="red",cex=2,pch=20)
coef(m2,9)


# Forward Selection
regfit.fwd <- regsubsets(train_outcome ~ ., data = train_data, nvmax = 14, method = "forward")
summary(regfit.fwd)

# Backward Selection
regfit.bwd <- regsubsets(train_outcome ~ ., data = train_data, nvmax = 14, method = "backward")
summary(regfit.bwd)

# Plotting
par(mfrow = c(2, 2))
plot(regfit.fwd, scale = "adjr2", main = "Adjusted R-squared for Forward Selection")
plot(regfit.fwd, scale = "Cp", main = "Cp for Forward Selection")
plot(regfit.bwd, scale = "adjr2", main = "Adjusted R-squared for Backward Selection")
plot(regfit.bwd, scale = "Cp", main = "Cp for Backward Selection")


#Ridge Reg
library(glmnet)
grid <- 10^seq(2, -5, length = 100)
Q3 <- glmnet(polyX[train, ], Y[train], alpha = 0, lambda = grid)
ridge.pred <-predict(Q3,s=4,newx=as.matrix(polyX))
print("Ridge Regression:")
mean((ridge.pred-Y)^2)


cv.out=cv.glmnet(as.matrix(polyX[train,]),Y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
log(cv.out$lambda.min)

#for lasso alpha=1
library(glmnet)
grid <- 10^seq(2, -5, length = 100)
Q3 <- glmnet(polyX[train, ], Y[train], alpha = 1, lambda = grid)
ridge.pred <-predict(Q3,s=4,newx=as.matrix(polyX))
mean((ridge.pred-Y)^2)


cv.out=cv.glmnet(as.matrix(polyX[train,]),Y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
log(cv.out$lambda.min)


#PCR
library(pls)
pcr.fit=pcr(Y~., data=polyX,scale=TRUE,validation="CV")#cross validation
summary(pcr.fit)

#PCA
PCA <- prcomp(polyX, center =T, scale=F)
plot(PCA)
PCA <- prcomp(polyX, center =T, scale=T)
plot(PCA)
summary(PCA)
PCA$rotation

#rf and nn 
# Create training and testing sets
train_data <- polyX[train, ]
test_data <- polyX[test, ]
train_outcome <- Y[train]
test_outcome <- Y[test]

# Random Forest model
rf_model <- randomForest(
  x = train_data,
  y = train_outcome,
  ntree = 500,  # Adjust the number of trees as needed
  importance = TRUE
)

# Predictions on the test set
rf_pred <- predict(rf_model, newdata = test_data)
confusionMatrix(rf_pred, test_outcome)

# Neural Network model
nn_model <- neuralnet(
  formula = Y ~ .,
  data = cbind(train_data, train_outcome),
  hidden = c(5, 2),  # Adjust the number of hidden layers and neurons as needed
  linear.output = TRUE
)

# Predictions on the test set
nn_pred <- predict(nn_model, newdata = test_data)
nn_pred <- ifelse(nn_pred > 0.5, 1, 0)  # Assuming binary classification
confusionMatrix(nn_pred, test_outcome)



library(ggplot2)
library(gridExtra)

data$Age.Group <- factor(data$Age.Group)
data$Tariff.Plan <- factor(data$Tariff.Plan)
levels(data$Age.Group)
levels(data$Tariff.Plan)
# Plot 1: Customer Value Distribution
plot1 <- ggplot(data, aes(x = Customer.Value)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Customer Value Distribution", x = "Customer Value", y = "Frequency")

# Plot 2: Boxplot by Age Group
plot2 <- ggplot(data, aes(x = Age.Group, y = Customer.Value, fill = Age.Group)) +
  geom_boxplot() +
  labs(title = "Customer Value by Age Group", x = "Age Group", y = "Customer Value")

# Plot 3: Boxplot by Tariff Plan
plot3 <- ggplot(data, aes(x = Tariff.Plan, y = Customer.Value, fill = Tariff.Plan)) +
  geom_boxplot() +
  labs(title = "Customer Value by Tariff Plan", x = "Tariff Plan", y = "Customer Value")

# Plot 4: Scatter Plot of Seconds of Use vs Customer Value
plot4 <- ggplot(data, aes(x = Seconds.of.Use, y = Customer.Value)) +
  geom_point(color = "darkorange") +
  labs(title = "Seconds of Use vs Customer Value", x = "Seconds of Use", y = "Customer Value")

# Arrange plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
