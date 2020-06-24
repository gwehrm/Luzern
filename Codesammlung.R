#Data 
library(ISLR)
data("Auto")
#################################################### 1 Regression ########################################################


########## simple linear regression ########## 
fit_lm_1 <- lm(y ~ x, data = df)

########## multiple linear regression ########## 
fit_lm_2 <- lm(y  ~ x + z, data = df)

## Cross-Validation
library(glmnet)
library(boot)
fit_lm_2 <- glm(mpg~acceleration, data = Auto)
fit_lm_2_cv <- cv.glm(Auto,fit_lm_2, K = 10) # performs a tenfold cross-validation
fit_lm_2_cv$delta # returns two values, first is the "raw", second the adjusted


#### non-linear transformations ####

########## polynomial ########## 
fit_lm_3 <- lm(y ~ x + I(x^2), data = df) ## or fit_lm_3 <- lm(y ~ poly(x,2), data = df)

# cv to get optimal poly
library(boot)
set.seed(17)
cv.errors <- data.frame(degree=seq(1,5,1), 
                        error= rep(NA, 5))
for (i in 1:5) {  # loop through 1-5 degree polynomials
  glm.fit <- glm(mpg~poly(acceleration, i), data=Auto)
  cv.errors$error[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
library(knitr)
kable(cv.errors)

#########3 interaction ###############
fit_lm_4 <- lm(mpg ~ acceleration*horsepower, data = Auto)
summary(fit_lm_4) # significant interaction - 

############# step function #############

fit_lm_4 <- lm(mpg ~ cut(acceleration,5), data = Auto) # cut slices the predictor z into 5 "steps" - same as ordered categorical variable
summary(fit_lm_4)

############## regression spline ###########
library(splines) # necessary for all splines
fit_lm_5 <- lm(mpg~bs(acceleration, knots=c(10,15,20)), data=Auto) # here the knots are set directly, change with degree
summary(fit_lm_5)

############## natural splines with additional constraints ##############
library(splines)
fit_lm_6 <- lm(mpg~ns(acceleration, df=4), data=Auto)
# CV to get optimal df
library(boot)
set.seed(17)
cv.errors <- data.frame(degree=seq(1,10,1), 
                        error= rep(NA, 10))
for (i in 1:10) {  # loop through 1-5 degree polynomials
  glm.fit <- glm(mpg~ns(acceleration, df=i), data=Auto)
  cv.errors$error[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}

############## smoothing splines ###############
library(splines)
fit_lm_7 <- smooth.spline(Auto$acceleration,Auto$mpg,df = 12)# can set the degree of freedom as pleases for cv: 

fit_lm_7 <- smooth.spline(Auto$acceleration,Auto$mpg,cv = T) # cv to determine the value of lambda (the smoothness level)

################# local regression #################
library(splines)
fit_lm_8 <- loess(mpg~acceleration, span = .2, data = Auto)# span	: the parameter α which controls the degree of smoothing.
summary(fit_lm_8)


################### general additive models - can be used for many different fits. ######################
library(splines)
fit_lm_9 <- lm(mpg~ns(acceleration,4)+ns(horsepower,5), data = Auto) # natural splines - still fitted with lm()
summary(fit_lm_9)
library(gam) # necessary for smooth gam
fit_gam_1 <- gam(mpg~s(acceleration,4)+s(horsepower,5), data = Auto) # smoothing splines
summary(fit_gam_1)
fit_gam_1 <- gam(y~s(x,4)+lo(z, span = .5)+w, data = df) # local regression
fit_gam_1 <- gam(I(y>100)~s(x,4)+w, family = binomial, data = df) # logistic regression

##################################################### 2 Classification #########################################################

############# logistic regresison ##################33
fit_glm_log <- glm(I(mpg>22)~acceleration, data = Auto, family = binomial)
# CV
library(boot)
cv_log <- cv.glm(Auto,fit_glm_log, K = 10) # without the K it would be a loocv
cv_log$delta # cv error - that many percent were wrong

############ Linear Discriminant Analysis ##############
library(MASS)
fit_lda <- lda(I(mpg>22)~acceleration, data = Auto)
fit_lda <- lda(I(mpg>22)~acceleration, data = Auto,CV=TRUE) #If CV = TRUE the return value is a list with components class, the MAP classification (a factor), and posterior, posterior probabilities for the classes.
# but not really clear what's meant...



############ Quadratic Discriminant Analysis ##############
library(MASS)
fit_qda <- qda(I(mpg>22)~acceleration, data = Auto)

######################### K-Nearest Neighbors #####################
# the data for knn has to delivered the following:
# matrix containing training data with only predictors - train_x
# matrix containing test data with only predictors - test_x
# vector with labels (y) for train - train_labels
# value for k neighbor
library(class)
set.seed(123)

#### KNN - To find optimal K #####
library(class)
library(dplyr)
label_x <- train_df$high
train_x <- select(train_df,mpg,acceleration)
test_x <- select(test_df,mpg,acceleration)

train_x <- scale(train_x)
test_x <- scale(test_x)
# also important: standardize the data!



cv_knn <- rep(0,10)
for (i in 1:10){
  fit_knn <- knn.cv(train_x,label_x,k=i,prob = TRUE)
  cv_knn[i] <- mean(as.numeric(as.character(fit_knn)))
}
cv_knn

# normal: k = 1
fit_knn <- knn(train_x,test_x, train_labels, k = 1)

##################################################### 3 Resampling Methods #######################################################

#########3 Validation set ##################
set.seed(123)
train <- sample(nrow(df), 0.7*nrow(df))# 
train_df <- df[train,]
test_df <- df[-train,]

############# Leave-One-Out Cross-Validation (LOOCV) ##################
# usually done within the estimation - example with gam and increasing poly fit
library(boot)
set.seed(123) # actually not needed, since no split
cv_error = rep(0,5)
for (i in 1:5){
  fit_glm <- glm(y~poly(x,i), data = df)
  cv_error[i] <- cv.glm(df,fit_glm)$delta[1]
}
cv_error

################ k-Fold Cross-Validation ###################
# see file CV
library(boot)
set.seed(123)
cv_error = rep(0,10)
for (i in 1:10){
  fit_glm <- glm(y~poly(x,i), data = df)
  cv_error[i] <- cv.glm(df,fit_glm, K = 10)$delta[1] #n the K indicates how many splits we want
}
cv_error

############## Bootstrap ####################
library(boot)
# inputs for the boot function are the dataframe, the coefficients and the number of repetitions

boot_fn <- function(data,index){
  return(coef(lm(mpg~acceleration, data = data, subset = index))) # careful! here is data in a function - no changing
}
boot_fn(Auto,1:392)

## or can also be used as an  bootstrap itself
boot_fn(Auto, sample(392,392, replace = T))

## do it 1000 times to get the actual estimate
boot(Auto,boot_fn, 1000)
# compared to the estimate of the regression
summary(lm(mpg~acceleration, data = Auto))$coef





##################### Subset Selection Methods ################
library(leaps)
regfit_full <- regsubsets(y~.,data = df)  # the parameter exhaustive was used - all models are "fitted". Output returns by
summary(regfit_full)                      # default the 8 best variables. alter via nvmax =..

########### Forward /Backwards ############
regfit_fwd <- regsubsets(y~.,method = "forward", data = df)
regfit_bwd <- regsubsets(y~.,method = "backward", data = df)

########### mixed ##########3
regfit_bwd <- regsubsets(y~.,method = "seqrep", data = df)  # not clear if this one is exactly the hybrid version or sth else

######################## Ridge Regression #######################
library(glmnet)
# prerquisits: 
# x as matrix
# y as vector
# no NAs 

data("Auto")
### data needs to be provided as vector / matrix
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]

x_train <- model.matrix(mpg ~.,data = train_df)[,-1]# [,-1] to remove y
x_test <- model.matrix(mpg ~.,data = test_df)[,-1]
y_train <- train_df$mpg
y_test <- test_df$mpg


# define lambda values
grid <- 10^seq(10,-2,length = 1000)

fit_ridge <- glmnet(x_train,y_train,alpha = 0, lambda = grid)# alpha = 0 defines ridge the values are by default standardized
# returns for all different lambdas the coefficients - best is estimated via cv

# cv ridge
set.seed(123)
fit_ridge_cv <- cv.glmnet(x_train,y_train, alpha = 0) # assumed we have train/test df and y values
bestlam <- fit_ridge_cv$lambda.min
fit_ridge_cv$cvm
# to predict use the bestlam
pred_ridge <- predict(fit_ridge, s = bestlam, newx = x_test) # be aware that here fit_ridge was fitted with train data 

# evaluate the goodness
mean((pred_ridge -y_test)^2)




############################# Lasso Regression #######################
library(glmnet)
# prerquisits: 
# x as matrix
# y as vector
# no NAs 


# Matrix:
data("Auto")
### data needs to be provided as vector / matrix
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]

x_train <- model.matrix(mpg ~.,data = train_df)[,-1]# [,-1] to remove y
x_test <- model.matrix(mpg ~.,data = test_df)[,-1]
y_train <- train_df$mpg
y_test <- test_df$mpg


# define lambda values
grid <- 10^seq(10,-2,length = 100)
fit_lasso <- glmnet(x_train,y_train,alpha = 1, lambda = grid)# alpha = 1 defines lasso, the values are by default standardized
# returns for all different lambdas the coefficients - best is estimated via cv

# cv ridge
set.seed(123)
fit_lasso_cv <- cv.glmnet(x_train,y_train, alpha = 1) # assumed we have train/test df and y values
bestlam <- fit_lasso_cv$lambda.min
# to predict use the bestlam
pred_lasso <- predict(fit_lasso, s = bestlam, newx = x_test) # be aware that here fit_lasso was fitted with train data 

# evaluate the goodness
mean((pred_lasso -y_test)^2)
# damn son!






###########################3 Principal Components ####################
data("Auto")
Auto_prc <- Auto[,-c(1,9)]# remove dependant and last value - last, since its a list of names..
pr_out <- prcomp(Auto_prc, scale = TRUE)  # notice 1) the column with the dependant varible is removed from df beforehand.
                                    # notice 2) the variables need to be scaled (done by scale = T) 
                                    # - implies only numerical values. character etc. need to be dummies. 
(pr_out)
# get the variance explained in percentag
pve <- pr_out$sdev^2/sum(pr_out$sdev^2)

############################ PCR and PLR regression #############################



data("Auto")
Auto <- Auto[,-9]# for PCR / PLS it is not necessary to remove the dependant variable (?)-so only last removed
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]
library("pls")

#we need to explicitly state we want a standardization, with "scale=T"
pcr <- pcr(mpg ~ .,
           data = train_df,
           validation = "CV",
           scale = T)
summary(pcr)

# some validationplots for the number of components
validationplot(pcr,
               val.type = "R2",
               estimate = "all",
               legendpos = "top")
validationplot(pcr,
               val.type = "MSEP",
               estimate = "all",
               legendpos = "top")
validationplot(pcr,
               val.type = "RMSEP",
               estimate = "all",
               legendpos = "top")

### predict 3####
predict_pcr <- predict(pcr, test_df, ncomp = 7)

#let's now see the test error
mean((predict_pcr - test_df$mpg) ^ 2)

# if we want to compute the train_error: Do the predict with the train data



########## PLS regression ####################
data("Auto")
Auto <- Auto[,-9]# for PCR / PLS it is not necessary to remove the dependant variable (?)-so only last removed
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]
library("pls")



fit_pls <-
  plsr(
    mpg ~ .,
    data = train_df,
    validation = "CV",
    scale = T
  )
summary(fit_pls)
validationplot(
  fit_pls,
  val.type = "MSEP",
  estimate = "all",
  legendpos = "top"
)
#we can see in the adjCV at 2/3 component is already very close to the best ones...

#let's do some prediction and compute the test error rate
predict_pls <- predict(fit_pls, test_df, ncomp = 3)
mean((predict_pls -test_df$mpg) ^ 2)




########################################################### 4 TREES #############################################################
library(tree)
################## Regression tree ################################
# Data
data("Auto")
Auto <- Auto[,-9]# remove last column
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]

fit_tree <- tree(mpg ~ ., data = test_df) # tree uses binary recursive partitioning

# plot tree
plot(fit_tree)
text(fit_tree, cex=0.75)

# Cross Validation for Pruning
set.seed (3)
fit_tree_cv =cv.tree(fit_tree)
# Runs a K-fold cross-validation experiment to find the number of 
# misclassifications as a function of the cost-complexity parameter \alpha.

fit_tree_cv # get best number is smallest value of fit_tree_cv$dev - see the number of nodes at $size

# pruning
fit_tree_pruned <- prune.tree(fit_tree, best=7) 
# accuracy
predi_tree <- predict(fit_tree_pruned,newdata = test_df)

# accuracy
mean((predi_tree-test_df$mpg)**2)

################### Bagging #############################
# just a special case of random forests with m = p. - means: 
library(randomForest)
# Data
data("Auto")
Auto <- Auto[,-9]# remove last column
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]

############## Apply bagging using the randomForest package in R. 
fit_bag <- randomForest(mpg~.,data=train_df,mtry=7,importance =TRUE)

# mtry = 8 means that we should use all 8 predictors for each split of the tree, 
# hence, do bagging! 

# prediction
yhat_bag <-  predict(fit_bag,newdata= test_df)
# accuracy
mean((yhat_bag-test_df$mpg)^2)
# The test set MSE associated with the bagged regression tree is 13.16, 
# That's almost half that obtained using an optimally-pruned single tree 
# (investigate this on your own).



########## Random Forests ################
# Data
data("Auto")
Auto <- Auto[,-9]# remove last column
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
train_df <- Auto[train,]
test_df <- Auto[-train,]

library(randomForest)
set.seed(123)
fit_rf <- randomForest(mpg~.,data=train_df, mtry=6,importance =TRUE)
yhat.rf = predict(fit_rf ,newdata=test_df)
mean((yhat.rf-test_df$mpg)^2)

# Investigating variable importance 
importance(fit_rf)
# Two measures of variable importance are reported:
# 1) The first is based upon the mean *decrease of accuracy*
# in predictions on the out of bag samples when a given variable 
# is excluded from the model.
# 2) THe second is a measure of the total *decrease in node impurity* 
# that results from splits over that variable, averaged over all trees.
varImpPlot(fit_rf)

################################################### 5 SUPPORT VECTOR MACHINES - Classifier #######################################


## Support Vector Machines
# there are basically three kernels to choose from, linear, polyniomial and radial. Dpending on flexibility, choose one. 
# depending on the kernel, we can train the model differently
# for linear - only cost
# for radial - cost and gamma
# for polynomial - cost, gamma and coef0

library(e1071)     # for the SVM funtion()

data("Auto")
Auto <- Auto[,-9]# for PCR / PLS it is not necessary to remove the dependant variable (?)-so only last removed
set.seed(123)
train <- sample(1:nrow(Auto),nrow(Auto)*0.7)
Auto$eco <- as.factor(ifelse(Auto$mpg>22,"1","0"))
Auto <- Auto[,-1]
train_df <- Auto[train,]
test_df <- Auto[-train,]



#now, we can fit a SVM with a linear kernel (Support Vector Classifier) to this data
# dpendant variable as factor!

svmfit <-
  svm(
    eco~.,
    data = train_df,
    kernel = "linear",
    cost = 10,
    scale = FALSE
  )

#...and plot it out
plot(svmfit, train_df,acceleration~horsepower) # whats the point

# what are the data point used as "support vectors"?
svmfit$index

# let's see the characteristics of this trained SVM
summary(svmfit)

# what is the effect of the "cost" parameter?
#  we can test it by passing in a list of values and use the tune() function to "compare" them
cost_range <-
  c(1e-10,
    1e-7,
    1e-5,
    0.001,
    0.0025,
    0.005,
    0.0075,
    0.01,
    0.1,
    1,
    5,
    10,
    100)
tune.out <- tune(
  svm,
  eco ~.,
  data = train_df,
  kernel = "linear",
  ranges = list(cost = cost_range)
)
summary(tune.out)

# what is the best model? here is simple, as the classes are linearly separable
#  it is the model with null error with the smaller cost value...
bestmod = tune.out$best.model
summary(bestmod)
plot(bestmod, train_df,acceleration~horsepower)

# predict
predi_linear_svm <- predict(bestmod,test_df)
# confusionmatrix
library(caret)
confusionMatrix(table(predi_linear_svm,test_df$eco))

# example polynomial- for coef0 there is nothing specified

cost_range <- c(0.01, 0.1, 1:5, 10, 100)
degree_range <- 2:4

# problems with number of itereations - therefore trying to normalize the data
test_df[,1:7] <- scale(test_df[,1:7])
train_df[,1:7] <- scale(train_df[,1:7])

svmfit__poly = svm(
  eco~ .,
  data = train_df,
  kernel = "polynomial",
  cost = 1000,
  scale = FALSE,
  degree = 5
)
# predict
predi_poly_svm <- predict(svmfit,test_df)
# confusionmatrix
library(caret)
confusionMatrix(table(predi_poly_svm,test_df$eco))




# tuning - problems
tune_poly <- tune(
  svm,
  eco ~ horsepower,
  data = Auto,
  kernel = "polynomial",
  # ranges = list(cost = cost_range, degree = degree_range),
  scale = FALSE
)
svmfit_3c_poly <- tune.out_3c_poly$best.model

# radial


svmfit_NL_circ = svm(
  eco ~ .,
  data = train_df,
  kernel = "radial",
  cost = 10,
  scale = FALSE
)
tune.out_3c_circ <- tune(
  svm,
  t ~ .,
  data = faithful_3classes,
  kernel = "radial",
  ranges = list(cost = cost_range, degree = degree_range),
  scale = FALSE
)
svmfit_3c_circ <- tune.out_3c_circ$best.model

