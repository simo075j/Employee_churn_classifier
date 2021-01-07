## Package and working directory
setwd("C:/Users/siras/Desktop/Uni/Datamining/")

packages_needed <- c('dplyr','tidyverse', 'skimr',
                     'caret','recipes','rsample','visdat',
                     'ggplot2','RCurl','datasets','skimr','glmnet','MASS',
                     'lubridate','corrplot','ROCR','corrr','ROCit','DataExplorer')

for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}

## load dataset from local csv file

real <- read.csv("examset2.csv")
tjek <- read.csv("examset2.csv")



## Feature engineering
#create age column:
#changing DOB to AGE variable:
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#changing DOB to AGE variable:
dataset$DOB <- as.character(dataset$DOB)
dataset$DOB <- as.Date(dataset$DOB)
dataset$DateofHire <- as.Date(dataset$DateofHire)

my.dob <- as.Date(dataset$DOB)
age(my.dob, age.day = as.Date(dataset$DateofHire))
real1$age <- age(my.dob, age.day = as.Date(dataset$DateofHire))


## looking at time in the firm seniority
dataset$DateofTermination[is.na(dataset$DateofTermination)] <- as.Date(today())


dataset$DateofHire <- as.Date(dataset$DateofHire)
dataset$DateofTermination <- as.Date(dataset$DateofTermination)
my.dob1 <- as.Date(dataset$DateofHire)
dateset$sen <- age(my.dob1, age.day = as.Date(dataset$DateofTermination))

## looking at days since last performance review
dataset$LastPerformanceReview_Date <- as.Date(dataset$LastPerformanceReview_Date)
my.dob2 <- as.Date(dataset$LastPerformanceReview_Date)
age(my.dob2, units = "days")
dateset$daycountpr <- age(my.dob2, units = "days")

#create a whole new dataset
real$age <- dateset$DOB
real$yearsemp <- dateset$sen
real$daycountpr <- dateset$daycountpr

write.csv(real1, "examset2.csv", row.names = FALSE)


## Data exploration
skim(real1)
summary(data)

# look at categoricals
plot_bar(real)

# Look at numeric & outliers
plot_histogram(real)
plot_histogram(tjek)
plot_histogram(log(real$PayRate))
plot(density(real$age))
plot(density(real$EngagementSurvey))
plot(density(real$PayRate))
summary(real$EngagementSurvey)

plot(real$PayRate)
plot(table(real$PayRate) %>% 
  prop.table())

# Grouping by the target variable ## EXTRA ##
data %>% 
  dplyr::group_by(TermReason) %>% 
  skim()

# look at the target variable (distribution)

table(real$target) %>% 
  prop.table()


table <- table(data1$EmploymentStatus,data1$target) %>% 
  prop.table()
plot_bar(data$TermReason)
plot_bar(data$EmploymentStatus)

# Creating the target variable
real <- real %>% 
  mutate(target=ifelse(real$EmploymentStatus =="Voluntarily Terminated","yes","no"))

real$target <- as.factor(real$target)

real <- real %>% 
  filter(EmploymentStatus != "Terminated for Cause")

#look at missing values
vis_miss(real, cluster = TRUE, sort_miss=TRUE)
plot_missing(real)

# Anything that needs to be changed?
real <- real[,-c(1,2,3,4,5,6,7,8,9,11,12,15,16,20,22,23,25,28,34,37)]
real <- real[,-c(16,18)]
real <- real[,-c(8)]

real$EmpSatisfaction <- as.factor(real$EmpSatisfaction)

tjek$EmpSatisfaction <- as.factor(tjek$EmpSatisfaction)

real$SpecialProjectsCount <- as.factor(real$SpecialProjectsCount)

real$target <- factor(real$target, levels = c("yes", "no"))


# Feature engineering on state variable
real <- real %>% 
  mutate(State = ifelse(State == "MA", "MA", "Other"))

summary(data)
data$price <- log(data$price)
data$best_seller <- as.factor(data$best_seller)

# Splitting the data set.
#Stratified sampling
set.seed(23)
split_strat  <- initial_split(real, prop = 0.7, strata = "target")
train_strat  <- training(split_strat )
test_strat   <- testing(split_strat )

#Check for NZV
caret::nearZeroVar(train_strat, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)

## pre-processing with blueprints

blueprint3 <- recipe(target ~ ., data = train_strat) %>%
  step_zv(all_predictors()) %>% #Both numeric and categorical
  step_nzv(all_predictors()) %>% #Both numeric and categorical
  step_mutate(PayRate = ifelse(PayRate >mean(PayRate), "Below average", "Above average")) %>%
  step_mutate(State = ifelse(State  == "MA", "MA", "other")) %>%
  step_mutate(age = ifelse(age > 35, "Below 35", "Above 35")) %>%
  step_mutate(EngagementSurvey = ifelse(EngagementSurvey > 3, "Below 3", "Above 3")) %>%
  step_string2factor(EngagementSurvey) %>% 
  step_string2factor(age) %>% 
  step_string2factor(PayRate) %>% 
  step_string2factor(State) %>% 
  step_other(Position, threshold = 0.05,other = "other") %>%
  step_other(RaceDesc, threshold = 0.05, other = "other") %>% 
  step_other(PerformanceScore, threshold = 0.08, other = "Below average") %>% 
  step_other(RecruitmentSource, threshold = 0.05, other = "other") %>%
  step_other(SpecialProjectsCount, threshold = 0.1, other = "1 or more") %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)  #one_hot = F for dummy encoding


# Train the blueprint on training data

prepare3 <- prep(blueprint3, training = train_strat)

view(prepare3$term_info)


# Lastly, apply the blueprint to new data
baked_train3 <- bake(prepare3, new_data = train_strat)

baked_test <- bake(prepare, new_data = test_strat)
view(baked_train)

# TRAINING THE MODEL with cross validation

cv <- trainControl(method = "repeatedcv", number =10 , repeats = 5,
                classProbs = TRUE,                 
                summaryFunction = prSummary)

# Logistic regression
set.seed(23)
logModel <- train(
  blueprint3, 
  data = train_strat, 
  method = "glm",
  family= "binomial",
  trControl = cv,
  metric = "AUC"
  #preProcess = c("zv", "center", "scale"),
  #tuneLength = 16
)

warnings()

#elastic net
set.seed(23)
penalized_mod <- train(
  blueprint3, 
  data = train_strat, 
  method = "glmnet",
  family = "binomial",
  trControl = cv, 
  tuneLength = 10,
  metric = "AUC"
)

penalized_mod$bestTune

plot(penalized_mod)


# look at the results
logModel$results
penalized_mod$results

summary(logModel)

pred_class <- predict(logModel, train_strat)
confusionMatrix(
  data = relevel(pred_class, ref = "yes"), # choose 1 if its a binary
  reference = relevel(train_strat$target, ref = "yes")
)

pred_class1 <- predict(penalized_mod, train_strat)
confusionMatrix(
  data = relevel(pred_class1, ref = "yes"), # choose 1 if its a binary
  reference = relevel(train_strat$target, ref = "yes")
)




### most important features
#logistic regression
vip(logModel, num_features = 70)
numvar_log <- coef(logModel$finalModel)
view(numvar_log)
print(numvar_log)
summary(logModel)



#penalized model
vip(penalized_mod, num_features = 70)
numvar_pen <- data.frame(coef(penalized_mod$finalModel))
print(numvar_pen)

diff(coef(penalized_mod$finalModel, penalized_mod$finalModel$lambdaOpt)@p)
numvar_best <- coef(penalized_mod$finalModel, penalized_mod$finalModel$lambdaOpt)
diff(numvar_best@p) #best
print(numvar_best)


#### training on entire dataset
fitControl <- trainControl(method = "none",
                           classProbs = TRUE,                 
                           summaryFunction = prSummary)

eln_full <- train(
  blueprint3, 
  data = train_strat, 
  method = "glmnet", 
  family = "binomial",
  trControl = fitControl, 
  tuneGrid = data.frame(alpha = penalized_mod$bestTune$alpha,
                        lambda = penalized_mod$bestTune$lambda),
  metric = "AUC"
)

log_full <- train(
  blueprint3, 
  data = train_strat, 
  method = "glm", 
  family = "binomial",
  trControl = fitControl, 
  metric = "AUC"
)

log_full$results
vip(eln_full, num_features = 70)
vip(log_full, num_features = 25)
diff(coef(eln_full$finalModel, eln_full$finalModel$lambdaOpt)@p)

test_pred1 <- predict(eln_full, newdata = test_strat)
test_pred2 <- predict(log_full, newdata = test_strat)

confusionMatrix(test_strat$target, data = test_pred1, positive = "yes", mode = "everything")
confusionMatrix(test_strat$target, data = test_pred2, positive = "yes", mode = "everything")

vip(log_full, num_features = 70)
numvar_log <- coef(log$finalModel)
view(numvar_log)
print(numvar_log)
summary(logModel)


# ROC AND AUC

library(pROC)

result.predicted.prob <- predict(log_full, test_strat, type="prob") # Prediction

result.roc <- roc(test_strat$target, result.predicted.prob$yes)# Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("accuracy"))
print(result.coords)#to get threshold and accuracy

