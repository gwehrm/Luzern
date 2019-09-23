# set working directory 
#setwd("C:\\Users\\gwehrm\\Documents\\Master\\FS19\\AML\\Project")
#setwd("C:/Users/Edi/Documents/Edward/Master_HSLU/2nd_semester/Machine_Learning/Group_work")
setwd("C:/Users/jonas/Dropbox/Dokumente/HSLU/FS 19/Applied Machine Learning/Project/Project_MachineLearning_original")


# libraries
library(leaps)
library(dplyr)
library(caret)
library(tidyverse)
library(pls)
library(gtools)
library(glmnet)
library(tree)
library(earth)


# load data
loan_df <- read_csv("loan.csv", n_max = Inf, guess_max = 100000, skip_empty_rows= TRUE, locale = locale(asciify = TRUE))

# group split

# problem: id is empty - take the row as new id and do the split again
loan_df <- cbind(id_2 = rownames(loan_df), loan_df)
rownames(loan_df) <- 1:nrow(loan_df)
loan_df$id_2 <- as.numeric(loan_df$id_2)
dataset_8 <- loan_df[which(loan_df$id_2%%8+1== 8),]

# remove the original file
rm(loan_df)

#For simplicity, let's save our data subset into csv format
write.csv(dataset_8, "df_subset.csv")

########         Data Preparation          #####################################

df_prepa <- read_csv("df_subset.csv")
#str(df_prepa)

# Transform var "terms" into numeric
df_prepa$term <- substr(df_prepa$term, 1,2) #extract numbers from terms 
df_prepa$term <- as.numeric(df_prepa$term)

# Set financial grades as factors, and give them an order -> Grade A is better than G
df_prepa$grade <- ordered(df_prepa$grade, levels= c("G", "F", "E", "D", "C", "B", "A"))

# Set financial grades as factors in sub_grades
df_prepa$sub_grade <- ordered(df_prepa$sub_grade, levels= c("G5", "G4","G3","G2","G1",
                                                            "F5", "F4","F3","F2","F1",
                                                            "E5", "E4","E3","E2","E1",
                                                            "D5", "D4","D3","D2","D1",
                                                            "C5", "C4","C3","C2","C1",
                                                            "B5", "B4","B3","B2","B1",
                                                            "A5", "A4","A3","A2","A1"))

#Transforming years of employment as numeric
df_prepa$emp_length <- gsub("[^0-9.]", "",  df_prepa$emp_length)
df_prepa$emp_length <- as.numeric(df_prepa$emp_length)

#Transforming verification status as factors, and giving them an order. 
#"Source Verified" has a higher level than "not verified"
df_prepa$verification_status <- ordered(df_prepa$verification_status,
                                        levels= c("Not Verified", "Verified", "Source Verified"))

#Transforming dates into Date format.     ***
#df_prepa$issue_d2 <- as.Date(df_prepa$issue_d, format = "%b-%y") 
#code doesn't work :( 
#Can anybody please have a look?          ***


#Var "emp_title" is very unstructured and not useful for analysis
#The most significant professions could be extracted and grouped together.
#1st Create a vector with some professions:
profe_list <- c("Assistant", "Manager", "Engineer", "Analyst", "Driver", "Worker",
           "Mechanic", "Officer", "Director", "Nurse", "Administrator",
           "Coordinator", "Sales", "Technician", "Teacher", "Advisor",
           "Supervisor", "Specialist", "Clerk", "Carpenter", "Scientist",
           "Secretary", "Business", "Retired", "Student", "Accountant")

#2nd. Create a function that extract the values from the var "emp_title" 
#and groups most of the repetitive professions.
#The function also accounts for public institutions, and private companies

Func_Extract <- function(col, vec){
  ifelse(grepl(vec[1], col, ignore.case = T) == 1, vec[1],
  ifelse(grepl(vec[2], col, ignore.case = T) == 1, vec[2],
  ifelse(grepl(vec[3], col, ignore.case = T) == 1, vec[3],
  ifelse(grepl(vec[4], col, ignore.case = T) == 1, vec[4],
  ifelse(grepl(vec[5], col, ignore.case = T) == 1, vec[5],
  ifelse(grepl(vec[6], col, ignore.case = T) == 1, vec[6], 
  ifelse(grepl(vec[7], col, ignore.case = T) == 1, vec[7],
  ifelse(grepl(vec[8], col, ignore.case = T) == 1, vec[8],     
  ifelse(grepl(vec[9], col, ignore.case = T) == 1, vec[9],
  ifelse(grepl(vec[10], col, ignore.case = T) == 1, vec[10],
  ifelse(grepl(vec[11], col, ignore.case = T) == 1, vec[11],
  ifelse(grepl(vec[12], col, ignore.case = T) == 1, vec[12],
  ifelse(grepl(vec[13], col, ignore.case = T) == 1, vec[13], 
  ifelse(grepl(vec[14], col, ignore.case = T) == 1, vec[14],
  ifelse(grepl(vec[15], col, ignore.case = T) == 1, vec[15],     
  ifelse(grepl(vec[16], col, ignore.case = T) == 1, vec[16],
  ifelse(grepl(vec[17], col, ignore.case = T) == 1, vec[17],
  ifelse(grepl(vec[18], col, ignore.case = T) == 1, vec[18],
  ifelse(grepl(vec[19], col, ignore.case = T) == 1, vec[19],       
  ifelse(grepl(vec[20], col, ignore.case = T) == 1, vec[20],
  ifelse(grepl(vec[21], col, ignore.case = T) == 1, vec[21],
  ifelse(grepl(vec[22], col, ignore.case = T) == 1, vec[22],
  ifelse(grepl(vec[23], col, ignore.case = T) == 1, vec[23],
  ifelse(grepl(vec[24], col, ignore.case = T) == 1, vec[24],
  ifelse(grepl(vec[25], col, ignore.case = T) == 1, vec[25],
  ifelse(grepl(vec[26], col, ignore.case = T) == 1, vec[26],       
  ifelse(grepl("President|Mgr|CEO|Mgmt|Owner", col, ignore.case = T) == 1, "Manager",       
  ifelse(grepl("Co.|Corporation|Corp|inc.|LLC|Group|Company|Department|&| s |'s| inc|LT| Service| and ", col, ignore.case = T) == 1, "Private_Inst.",
  ifelse(grepl("District|State|County|Hospital|School|Council|Health|US", col, ignore.case = T) == 1, "Public_Inst.", "Other")))))))))))))))))))))))))))))        
  
}

#Call previous function, plus using the vector with the professions' names.
#Assign results into the same variable.
df_prepa$emp_title <- Func_Extract(df_prepa$emp_title, vec= profe_list)

#Let's observe a list of the most common professions
#table(df_prepa$emp_title)
#Now the var "profession" has a structured and can be further analyzed

#Let's remove variables with too many NAs

#1st count the NAs
x <- apply(df_prepa,2,function(x)sum(is.na(x)))
#2nd remove columns with lots of NAs? (over 5%) Luca proposed this (see Ilias)
max_num = dim(df_prepa)[1]
x_remove <- names(x[x>(0.05*max_num)])
dataset_clean <- select(df_prepa,-x_remove)

## reduce the amount of "levels" in character columns 
characters <- dataset_clean[, sapply(dataset_clean, class) == 'character']
sapply(characters, function(x)length(unique(x)))
## variable "title" has 10350 different characters - reduce by renaming all levels with less than 100 occurences "other"
x <- data.frame(sort(table(characters$title),decreasing = T)) # rename all observations with less than 100 occurences with "other
replacing_vars <- filter(x,Freq < 100)[,1]
dataset_clean <- dataset_clean %>%
  mutate(title_reduced = case_when(title %in% replacing_vars ~ "other",
                           !(title %in% replacing_vars) ~ title,
                           is.na(title) ~ "other" # this doesnt work - replace later
                           ))%>%
  select(-title)



## impute/remove NAs
## count number of NAs per columns
apply(dataset_clean,2,function(x)sum(is.na(x)))## it seems that certain rows have on multiple columns na
## remove these rows that have more than 20 NAs - value has no theoretical backing. It is 
## function to delete NAs
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
dataset_clean <- delete.na(dataset_clean,n=20) # maybe check if the thrown out observations are have similar distribution as the others 

## count number of NAs agian
apply(data,2,function(x)sum(is.na(x)))
# replace categorical variables NA with "missing" - numeric NA with mean
dataset_clean <- as.data.frame(lapply(dataset_clean, function(x)if(is.numeric(x)){na.replace(x,mean(x,na.rm = T))}else{na.replace(x,"missing")}))
# no NAs...
# remove the two ID columns
dataset_clean <- select(dataset_clean,c(-X1,-id_2))

#str(dataset_clean)

# for the sake of simplicity - remove zip_code and earliest cr_line. They have too many levels 
dataset_clean <- select(dataset_clean,-zip_code, -earliest_cr_line)

## Now the dataset is clean, with a total of 85 VARs
#Let's save it to keep all changes for next run.
saveRDS(object = dataset_clean,
        file= "dataset_clean.rds")


dataset_clean <- readRDS("dataset_clean.rds")



## # ## ## # ## ## # ## ## # ##    REGRESSION    ## # ## ## # ## ## # ## ## # ##



################         Data Preparation for Regression  Problem        #####################################
# create copy without NAs in int_rate (although NAs are already removed)
table(is.na(dataset_clean$int_rate)) # check again if no NAs
dataset_clean <- dataset_clean %>% 
  filter(!is.na(int_rate))



## Validation Set 
# set the seed of the random sequence for reproducibility
set.seed(5)

# split at 70%
smp_size <- floor(0.7 * nrow(dataset_clean))
train_ind <- sample(seq_len(nrow(dataset_clean)), size = smp_size)
# create test and train dataset
train <- dataset_clean[train_ind, ]
test <- dataset_clean[-train_ind, ]

# train %>% mutate_if(is.factor, as.character) -> train
## feature selection:
## regsubset was tested, but didnt work accordingly - it took way too long to calculate.
## In the group work description it is said we could also use a base transfromation. However, first test some other options:
## MARS   Results were not statisfying
## Boruta Took way too long to calculate (estimated 3 days) there has to be a simpler way
## LASSO - Good results and quickly calculated.
## So, no base transformation is necessary

## MARS
marsModel <- earth(int_rate ~ ., data=train) # build model
ev <- evimp(marsModel)        #    estimate variable importance -> no statisfying results - not "enough"  predictors

# Boruta - as comment, otherwise blocked
# set.seed(123)
# boruta.train <- Boruta(int_rate~., data = train, doTrace = 2)   #Be careful, it may take a long time to complete
# print(boruta.train)


# LASSO

# create a matrix with the predictors for train data
predictors <- model.matrix(int_rate ~ ., train)[,-1]
# separate the outputs/dep. var
outputs <- train$int_rate
# create a matrix with the predictors for train data
predictors_t <- model.matrix(int_rate~.,test)[,-1]
# separate the outputs/dep. var
outputs_t <- test$int_rate


# select variables with lasso
cv.lasso=cv.glmnet(predictors,outputs) # default aplha = 1, so no need to write it

# save the lasso-model
saveRDS(cv.lasso,"lasso.rds")
cv.lasso <- readRDS("lasso.rds")

# see mse and seewhere the optimal model is
plot(cv.lasso)

#extract coefficients which were not 0
coef(cv.lasso)
str(train)

# coefficients:
tmp_coeffs <- coef(cv.lasso, s = "lambda.min")
x <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

# subset the predictors and work only with the ones that were not 0 in the lasso regression
predictors_subs <- predictors[,x[,1]]
train_subset <- data.frame(int_rate = outputs,predictors_subs)

predictors_subs_t <- predictors_t[,x[,1]]
test_subset <- data.frame(int_rate = outputs_t,predictors_subs_t)


## correlation graphic useless with too many predictors!
## reduce again: from the cleaned data (no var selection or train/test split)
## do it only with numerical
# transform to correlationamtrix to use corrplot

# select only numeric values
numeric_subset <- dataset_clean[, sapply(dataset_clean, class) == 'numeric']
# move int_rate to first position
numeric_subset <- numeric_subset %>%
  select(int_rate, everything())
# create correlationmatrix
numeric_subset <- cor(numeric_subset)
# plot correlationmatrix
corrplot::corrplot(numeric_subset,method="color")
# as we see on the first row - no strong correaltions with int_rate with the numerical variables 


########## maintask
# Compare three different methods to perform regression, using the cross-validation method to compute the best parameters
# this means
# build three different models with three methods:
# 1) regression tree
# 2) lasso
# 3) ridge

# for each of these models cross validation is done
# train error = error rate without cross-validation
# cv erro rate = error rate from cross validation
# finally the test error is evaluated predicting the three
# models on the initially split test_subset

# for regression tree,cv gives optimal alpha for pruning
# cv ridge gives the model with optimal regularization
# cv lasso give the model with the optimal regularization 




#### regression 1) regression tree from the lecture SW04 (package tree) - but doesnt perform that well 
## Fit regression 
set.seed(5)
fit_tree <- tree(int_rate ~ .-int_rate-sub_grade.L,method = "recursive.partition", data = train_subset)  # why only 8 nodes? and only 3 predictors??

# train error
summary(fit_tree) # mse = 1.452

# visualise the tree
plot(fit_tree)
text(fit_tree, cex=0.75) # cex: set character size to 0.75


#####  Cost-Complexity Pruning (From SW04)
#  Goal: grow smaller trees to avoid high variance and overfitting. 
#  Positive effects: 
#     - smaller *test* errors (due to less overfitting).
#     - higher interpretability (due to smaller trees).


# use cross-validation to find the optimal parameter  
set.seed (3)
cv_tree =cv.tree(fit_tree)
# Runs a K-fold cross-validation experiment to find the number of 
# misclassifications as a function of the cost-complexity parameter alpha.

cv_tree
# we see that the trees lowest cv error-rate has the full tree... this implies that no
# pruning should be done with this tree...

# plot the cross-validation error-rate
par(mfrow=c(1,2))
plot(cv_tree$size, cv_tree$dev, type="b")
plot(cv_tree$k, cv_tree$dev, type="b")
par(mfrow=c(1,1))

# pruning, but here not necessary as described above
prune_tree <- prune.tree(fit_tree, best=8) 

# Plot the regression tree 
plot(prune_tree)
text(prune_tree, cex=0.75) # cex: set character size to 0.75

# Test error
predi_tree <- predict(prune_tree,test_subset[,-1])

mean((predi_tree-test_subset$int_rate)**2)
plot(predi_tree,test_subset$int_rate)
# Test error: 1.73


## Pruning without actually pruning the tree seems a bit boring. Therefore, we decided to test another 
## package. There we want to build an initial tree with (nearly) no restrictions. 
## It seemed to us that the package tree includs already some restrictions

# the corresponding package
library(rpart)
# fit the regression tree model
fit_tree_2 <- rpart(
  formula = int_rate ~ .,
  data    = train_subset,
  method  = "anova",
  control = list(cp = 0, xval = 10) # this line says that a full tree should be built - there are no costs
)

# an enormous tree is built - with loads of nodes
summary(fit_tree_2)

plotcp(fit_tree_2)

# lets see whether the prediction are off. It is expected that the results, since overfitting is really bad
predi_fit_full <- predict(fit_tree_2,test_subset)
mean((predi_fit_full-test_subset$int_rate)**2)
## with a full tree, the validation error is 0.244! So there seems to be (with this test-set) no overfitting

## do also some pruning with this tree : 
## . Any split that does not decrease the overall lack of fit by a factor of cp is not attempted
## means: For instance, with anova splitting, this means that the overall R-squared must increase by cp at each step.
fit_tree_2_prune <- rpart(
  formula = int_rate ~ .,
  data    = train_subset,
  method  = "anova",
  control = list(cp = 0.01, xval = 10) # this line says that at each split, the R-squared must increase by 0.01
)
summary(fit_tree_2_prune)

plotcp(fit_tree_2_prune)

# prediction 
predi_fit_prune <- predict(fit_tree_2_prune,test_subset)
mean((predi_fit_prune-test_subset$int_rate)**2)
# here validation test_error of 1.47

# another tree, less pruned
fit_tree_2_prune_2 <- rpart(
  formula = int_rate ~ .,
  data    = train_subset,
  method  = "anova",
  control = list(cp = 0.001, xval = 10) # this line says that at each split, the R-squared must increase by 0.001
)
summary(fit_tree_2_prune_2)

plotcp(fit_tree_2_prune_2)

# prediction 
predi_fit_prune_2 <- predict(fit_tree_2_prune_2,test_subset)
mean((predi_fit_prune_2-test_subset$int_rate)**2)
# here validation error of 0.93
# seems that there is not that much of overfitting


### regression 2) ridge
## do standard glmnet with preselected lambdas to get the train error
## the crossvalidation to get the cv-train error
## then prediction with the optimal lambda from cross-validation the get the validation error (test error)

# ridge without cross-validation
ridge <- glmnet(predictors_subs,outputs,alpha = 0)
# glmnet returns a sequence of models to choose from.
# to select which model is the best (which lambda is the best), cross-validation can be used (is done later in the exercise)
# getting a train error here, does not make that much sense. 
# So instead I choose to get the mean of the test errors of the predictions with the different lambdas
predi_train <- predict.glmnet(ridge,predictors_subs_t, type = "link")
# predict 
mean((predi_train-outputs_t)**2) # R automatically does the calculation for each row in predi_train
# 12.45 - really bad estimation. but is of course the average over all different values of lamba. nonsense 

## cross-validation
cv_ridge=cv.glmnet(predictors_subs,outputs, alpha  = 0, nfolds = 4) # nfolds  = 4 ,since large dataset and less folds are necessary
# cross validated error rate:
# which lambda returns the minimum cv-error?
cv_ridge$lambda.min
# get the list of different lambdas fitted
cv_ridge$lambda
# we see that the last lambda is the same as lambda.min
# get the cv-error rate for all models with different lambdas
cv_ridge$cvm
# the cv-error for the model with the best lambda
cv_ridge$cvm[99]

# validation error (test error)
predicted_int <- predict.cv.glmnet(cv_ridge,predictors_subs_t,s = "lambda.min")
mean((predicted_int-outputs_t)**2)
# 0.67 
# quite good estimate!


### regression 3) lasso
lasso <-  glmnet(predictors_subs ,outputs, alpha  =1)
# glmnet returns a sequence of models to choose from.
# to select which model is the best (which lambda is the best), cross-validation can be used (is done later in the exercise)
# getting a train error here, does not make that much sense. 
# So instead I choose to get the mean of the test errors of the predictions with the different lambdas
predi_train <- predict.glmnet(lasso,predictors_subs_t, type = "link")
# predict 
mean((predi_train-outputs_t)**2)
# lasso gives on average (over all lambas) a test error of 2.22. already way better than the ridge. However. 
# This result still doesnt make much sense, since its the average over the different fits with different lambdas

# lasso regression with cross-validation
cv_lasso <- cv.glmnet(predictors_subs,outputs, alpha  =1, nfolds = 4)  # nfolds  = 4 ,since large dataset
# plot the cv-mse (cv-error)
plot(cv_lasso)

# with which lambda the model got the lowest cv-error:
cv_lasso$lambda.min

# predict the test data set with the minimum lambda
predicted_int_lasso <- predict.cv.glmnet(cv_lasso,predictors_subs_t,s = "lambda.min")
mse(predicted_int_lasso,outputs_t)
# validation error (test error) 0.52


# It can be said that actually the full regression tree with of the rpart package gives the best results in testing on the test (validation)
# However, there is a high risk of overfitting! 
# so the lasso regression might be also a solid choice


## # ## ## # ## ## # ## ## # ##    CLASSIFICATION    ## # ## ## # ## ## # ## ## # ##




################################## preparatory task classification ########################################
rm(list=ls())
dataset_clean <- readRDS("dataset_clean.rds")
#str(dataset_clean)

###preparatory task classification: 

dataset_clean$loan_status <- as.character(dataset_clean$loan_status)

dataset_clean_filtered <- filter(dataset_clean, loan_status != "Current") #Filter out all observations with loan_status CURRENT

dataset_clean_filtered$loan_status[dataset_clean_filtered$loan_status != "Fully Paid"] <- "DEFAULTED" #Change value of loan_status to "DEFAULTED", if not "Fully Paid"



####PCA and PLR


dataset_clean_filtered$loan_status <- ifelse(dataset_clean_filtered$loan_status == "Fully Paid", 1, 0) #change the type of column "loan_status" from chr to numeric

# data_df <- dataset_clean_filtered[ lapply(dataset_clean_filtered, function(x) sum(is.na(x)) / length(x) ) < 0.05 ] %>% as_tibble() #remove columns with more than 95% NA
# A lot of variables are categorical, so it is not possible to consider them for PCA/
# We can transform them to dummy variabls using an R package
#load library
library(dummies)

#create a dummy data frame
# colnames(dataset_clean_filtered)
#Save in a vector the categorical vars that we want to trasform to dummies
Vector_remove <- c("grade","home_ownership" ,"verification_status","pymnt_plan",
                   "initial_list_status","application_type","debt_settlement_flag","hardship_flag" ,"disbursement_method")

new_my_data <- dummy.data.frame(dataset_clean_filtered, names = Vector_remove)
##Double check if vars are numeric or int.
# str(new_my_data)
# is.numeric(new_my_data$loan_status)

#Categorical variables
dataset_clean_filtered_numeric <- new_my_data[,sapply(new_my_data, is.numeric)] #get only the numeric columns to run PCR properly (scaling) (agressive approach, to skip all the chr variables)

dataset <- na.replace(dataset_clean_filtered_numeric, 0) #replace na's with 0

dataset$policy_code <- NULL #remove column policy_code due to "invalid" data format for PCR

dataset$loan_status <- ifelse(dataset$loan_status==1, "Fully Paid", "DEFAULTED")

#Divide data intro Training & Test + Validation
# split 60% for Training, 25% for Test, 15% for Validation
set.seed(5)
smp_size <- floor(0.6 * nrow(dataset)) #Training
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]    #Train set ready
test_valid <- dataset[-train_ind, ]  #We still have to split this set

set.seed(5)
smp_size1 <- floor(0.625 * nrow(test_valid))   #Equal to 25% of original set
test_ind <- sample(seq_len(nrow(test_valid)), size = smp_size1)


test <- test_valid[test_ind, ]  #25% of original set
valid <- test_valid[-test_ind, ] #15% of original set



#################     PCA Var Selection      ################

#For PCA we should remove the Output variable
train_wo <- subset(train, select = -c(loan_status))
test_wo <- subset(test, select = -c(loan_status))
valid_wo <- subset(valid, select = -c(loan_status))

#principal component analysis
set.seed(5)
prin_comp <- prcomp(train_wo, scale. = T)

#prin_comp$x

#### (The next code is only for visualization & and selection of Num.of Components)

# #compute standard deviation of each principal component
# std_dev <- prin_comp$sdev
# 
# #compute variance
# pr_var <- std_dev^2
# #proportion of variance explained
# prop_varex <- pr_var/sum(pr_var)
# #cumulative scree plot
# plot(cumsum(prop_varex), xlab = "Principal Component",
#        ylab = "Cumulative Proportion of Variance Explained",
#        type = "b")

#We also need to apply the same PCA concepts for Test&Validation sets


#add sets with principal components
train.data <- data.frame(loan_status = train$loan_status, prin_comp$x)
#we are interested in first 45 PCAs
train.data <- train.data[,1:46]

#head(train.data)
# We should do exactly the same transformation to the test&Validation sets 
# just as we did to training set

#transform test into PCA
test.data <- predict(prin_comp, newdata = test_wo)
test.data <- as.data.frame(test.data)
#select the first 45 components
test.data <- test.data[,1:45]
test.data$loan_status <- test$loan_status

#transform Validation into PCA
valid.data <- predict(prin_comp, newdata = valid_wo)
valid.data <- as.data.frame(valid.data)
#select the first 45 components
valid.data <- valid.data[,1:45]   ## This will only be used on the best Model
valid.data$loan_status <- valid$loan_status

#################     PLS Var Selection      ################
train$loan_status <- ifelse(train$loan_status == "DEFAULTED", 1, 0)
library(pls)
# ?plsr
pls_select1 <- plsr(loan_status ~ ., scale= T, ncomp = 20, data = train, validation = "CV")
#summary(pls_select1)

#In General, PLS needs less components than PCA to achieve a low RMSEP (error)

#How many components do we need using Cross-validated RMSEP?
plot(RMSEP(pls_select1), legendpos = "topright")
# Observing the graph, we can see that after 10 components, the improvement is minimal
summary(pls_select1)   #Observe the variance_explained after each component
#Like this we can assume that 9 components is a good fit. The output doesn't improve after this.
#However the variance_explained is quite low: 37.09%

test.data.pls<- predict(pls_select1, ncomp = 9, newdata = test)
test.data.pls <- as.data.frame(test.data.pls)

#Only observing the % of variance explained, we can say that PCA explains 
#a higher variance ~0.85, than PLS with only 0.37
#Therefore, we will proceed with the main task, only with PCA components


###
##########  Main Task
##########  4 Machine Learning Models with PCA
### 


##############################   KNN   ###########################

## prepare the data for knn - knn needs train and test, both without dependant variable. and the dependant var of the test data seperately.
# Due to limited resources, we do not make cross validations for the knn-classification and get the optimum k 

train_data_predictors <- select(train.data,-loan_status)

test_data_predictors <- select(test.data,-loan_status)

train_status <- train.data$loan_status



knn_pred <- knn(train_data_predictors,test_data_predictors, train_status,k=1)

# Confusion Matrix
table(knn_pred, test.data$loan_status)

# Estimate the test error rate
mean(knn_pred != test.data$loan_status)

### knn with k = 5

knn_pred_5 <- knn(train_data_predictors,test_data_predictors, train_status,k=5)

# Confusion Matrix
table(knn_pred_5, test.data$loan_status)

# Estimate the test error rate
mean(knn_pred_5 != test.data$loan_status)




#########################   Logistic Regression   ###########################

glm.fit = glm(loan_status ~., data = train.data, family = binomial, control = list(maxit = 80)) #fit the Logistic Regression model

summary(glm.fit)

#Predicting probabilities of the training data
glm.probs.train = predict(glm.fit, type = "response") #calculate the probabilities of DEFAULT / Fully Paid

#Predicting the training data classes
glm.pred.train = rep("DEFAULTED", nrow(train.data))

glm.pred.train[glm.probs.train > 0.5] = "Fully Paid"

table(train.data$loan_status, glm.pred.train)

#Calculating the training error rate 
mean(glm.pred.train !=train.data$loan_status) #only 0.69% are wrongly classified

#contrasts(loan_status) #does not work yet

#Predicting the test data probabilities
glm.probs.test = predict(glm.fit, test.data, type = "response")

#Predicting the test data classes
glm.pred.test = rep("DEFAULTED", nrow(test.data)) #The following two commands create a vector of class predictions based 
                                                    #on whether the predicted probability of a market increase is greater than or less than 0.5.
glm.pred.test[glm.probs.test > 0.5] = "Fully Paid"

#Confusion matrix
table(test.data$loan_status, glm.pred.test)

#Calculating the test error rate 
mean(glm.pred.test !=test.data$loan_status) #excellent result. only 0.71% are wrongly classified. as the 
                                            #difference between train and test error are very small, the model seems to be very good

##############################   Decision Tree   ###########################

#run a decision tree
## Using the first 45 PCAs (They are not variables)
tree.model <- tree(loan_status ~ .,data = train.data, method = "anova")
plot(tree.model)
text(tree.model, cex=0.75, pretty=0)
tree.model
summary(tree.model) #train error = 11.8%

#make prediction on test data
tree.model.predict <- predict(tree.model, test.data, type="class")


#confusion matrix
(tree.model.predict.ct <- table(tree.model.predict, test.data$loan_status))
(tree.model.correct <- (tree.model.predict.ct[1,1] + tree.model.predict.ct[2,2])/sum(tree.model.predict.ct)) # portion of correctly classified observations: 70.3%
(tree.model.testError <- 1 - tree.model.correct) # test error: 11.9%: not much higher than train error -> excellent result
                                              #-> pruning the tree doesn't seem to be necessary as the train and test error
                                              # are already very close and there are only 6 nodes

#########################   Random Forest   ###########################

#packages
install.packages("randomForest")
install.packages("MASS")
library(MASS)
library(randomForest)

#growing a random forest
set.seed(1)
rf.default=randomForest(loan_status~.,data = train.data, mtry = 6, importance =TRUE) 
rf.predict = predict(rf.default, newdata = test.data)
str(rf.predict)
summary(rf.default)
str(test.data$loan_status)


(rf.model.predict.ct <- table(rf.predict, test.data$loan_status))
(rf.model.correct <- (rf.model.predict.ct[1,1] + rf.model.predict.ct[2,2])/sum(rf.model.predict.ct)) # portion of correctly classified observations: 70.3%
(rf.model.testError <- 1 - rf.model.correct) #test error: 6.7% much better than the decision tree model 
                                            #As I'm a little bit suspicious about the results from the Logistic Regression model
                                            #I would probably take the Random Forest as final model and run it with the validation set


## prediction on validation set
rf.predict.val <- predict(rf.default,newdata = valid.data)

(rf.model.predict.ct <- table(rf.predict.val, valid.data$loan_status))
(rf.model.correct <- (rf.model.predict.ct[1,1] + rf.model.predict.ct[2,2])/sum(rf.model.predict.ct)) # portion of correctly classified observations: 70.3%
(rf.model.testError <- 1 - rf.model.correct)



################# Conceptual Comparison with Existing Solutions #######################

#Source: https://medium.com/@vijaya.beeravalli/comparison-of-machine-learning-classification-models-for-credit-card-default-data-c3cf805c9a5a

#There are lots of different analysis in the internet about which classification model performs best 
#in predicting default rate.
#Beside fancy deep learning models it's mostly random forest which performs best. 
#As it is supervised learning it's quite easy to interpret the results, underlining its superiority to the other models. 
#In the article above they mention that all the models perform quite equal and hence, 
#it's difficult to choose only one. They stress the point that it is especially important
#to reduce the false negative rate (Type II error: predicting "no default" but then defaulting in real life)
#as this is the costliest case for a potential lender. In their analysis "Binary Logisitc Regression" 
#did perform best regarding the Type II error, with Random Forest on 5th place.
#


#####################

