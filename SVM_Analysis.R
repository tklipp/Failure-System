### SVM Analysis


# Load libraries
library(tidyverse)
library(e1071)

# Load data, set seed
stats <- complete_data
seed <- 66

# Make binary classificatin (pass/fail)
failingGrade <- c("C-", "D+", "D", "D-", "F", "WU", "I", "NC")

stats$pass <- rep(NA, nrow(stats))
stats$pass[stats$grade %in% failingGrade] <- "fail"
stats$pass[!(stats$grade %in% failingGrade)] <- "pass"
stats$pass <- as.factor(stats$pass)

# Make new variable called class
stats$matric.period <- as.numeric(as.character(stats$matric.period))
stats$period <- as.numeric(as.character(stats$period))
stats$class <- case_when(
  ((stats$period-stats$matric.period) %in% c(0,8) & stats$enrollment.sts.desc=="1ST TIME STUDENT")~"1st-Year",
  ((stats$period-stats$matric.period) %in% c(2, 10, 18) & stats$enrollment.sts.desc=="1ST TIME STUDENT") ~ "2nd-Year",
  ((stats$period-stats$matric.period) %in% c(12, 20, 28) & stats$enrollment.sts.desc=="1ST TIME STUDENT") ~ "3rd-Year",
  ((stats$period-stats$matric.period) %in% c(22, 30, 38) & stats$enrollment.sts.desc=="1ST TIME STUDENT") ~ "4th-Year",
  ((stats$period-stats$matric.period) >39 & stats$enrollment.sts.desc=="1ST TIME STUDENT") ~ "5th Year+",
  ((stats$period-stats$matric.period) %in% c(0,8) & stats$enrollment.sts.desc=="TRANSFER") ~ "3rd-Year",
  ((stats$period-stats$matric.period) %in% c(2,10,18) & stats$enrollment.sts.desc=="TRANSFER") ~ "4th-Year",
  (TRUE~"5th Year+"))
stats$class <- factor(stats$class)



### SVM model on before_data


# Grab the important variables for analysis 
before_data <- stats %>% select(ap.credits, birth.year, college, enrollment.sts.desc,
                                ethnicity.desc, hsgpa, class, period, satrd.composite,
                                satrd.math, ed.father.parent.guardian1, ed.mother.parent.guardian2,
                                total.units.earned.before, trans.units.acc, gender, pass)

before_data$ed.mother.parent.guardian2 <- factor(before_data$ed.mother.parent.guardian2,
                                                 levels = c("No high school","Some high school","High school grad",
                                                            "Some college","2 yr college grad","4 yr college grad",
                                                            "Post Graduate"), ordered = T)
before_data$ed.father.parent.guardian1 <- factor(before_data$ed.father.parent.guardian1,
                                                 levels = c("No high school","Some high school","High school grad",
                                                            "Some college","2 yr college grad","4 yr college grad",
                                                            "Post Graduate"), ordered = T)

# Randomize data
before_data <- before_data[sample(nrow(before_data)), ]

# Split the data into training and testing sets
sample_size <- floor((2/3)*nrow(before_data))
sample_size
set.seed(seed)
train_ind <- sample(seq_len(nrow(before_data)), size=sample_size)
train <- before_data[train_ind,]
test <- before_data[-train_ind,]


# Train the model
set.seed(seed)
tc <- tune.control(cross = 10)
tune <- tune(svm, pass~., data=train, ranges=list(gamma=10^(-5:-1), cost=10^(-3:1)), tunecontrol = tc)
summary(tune)
plot(tune)
svm_model <- svm(pass ~ ., data=train, cost=1, gamma=0.1, kernel='radial', scale=T)
summary(svm_model)

# Model prediction

# The testing set
prediction_test <- predict(svm_model, newdata=test[,-16]) 
# test[,-16] corresponds to response (pass) column in test dataframe
print(prediction_test)
summary(prediction_test)

# Confusion matrix output for the test set
CM_test <- table(pred = prediction_test, true = test[,16])
CM_test # Misclassifiction rate of 0.2371245 (221/932)


### SVM analysis on after_data

# Grab important variables for analysis
after_data <- stats %>% select(hwk1, hwk2, hwk3, hwk4, quiz1chapters28, quiz2chapter3, quiz3chapter4,
                               quiz4chapter5, ap.credits, birth.year, college, enrollment.sts.desc,
                                ethnicity.desc, hsgpa, class, period, satrd.composite,
                                satrd.math, ed.father.parent.guardian1, ed.mother.parent.guardian2,
                                total.units.earned.before, trans.units.acc, gender, pass)

after_data$ed.mother.parent.guardian2 <- factor(after_data$ed.mother.parent.guardian2,
                                                 levels = c("No high school","Some high school","High school grad",
                                                            "Some college","2 yr college grad","4 yr college grad",
                                                            "Post Graduate"), ordered = T)
after_data$ed.father.parent.guardian1 <- factor(after_data$ed.father.parent.guardian1,
                                                 levels = c("No high school","Some high school","High school grad",
                                                            "Some college","2 yr college grad","4 yr college grad",
                                                            "Post Graduate"), ordered = T)


# Randomize data
after_data <- after_data[sample(nrow(after_data)), ]


# Split the data into training and testing sets
sample_size_2 <- floor((2/3)*nrow(after_data))
sample_size_2
set.seed(seed)
train_ind_2 <- sample(seq_len(nrow(after_data)), size=sample_size_2)
train_2 <- after_data[train_ind_2,]
test_2 <- after_data[-train_ind_2,]


# Train the model
set.seed(seed)
tc <- tune.control(cross = 10)
tune_2 <- tune(svm, pass~., data=train_2, ranges=list(gamma=10^(-5:-1), cost=10^(-3:1)), tunecontrol = tc)
summary(tune_2)
plot(tune_2)
svm_model_2 <- svm(pass ~ ., data=train_2, cost=1, gamma=0.01, kernel='radial', scale=T)
summary(svm_model_2)

# Model prediction

# The testing set
prediction_test_2 <- predict(svm_model_2, newdata=test_2[,-24]) 
# test_2[,-25] corresponds to response (pass) column in test dataframe
print(prediction_test_2)
summary(prediction_test_2)

# Confusion matrix output for the test set
CM_test_2 <- table(pred = prediction_test_2, true = test_2[,24])
CM_test_2 # Misclassifiction rate of 0.1695279 (158/932)












