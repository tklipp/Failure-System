# some libraries
library(tidyverse)
library(randomForest)

## global variables
seed <- 66

## some functions
bestRandomForest <- function(df, mtrys = 1:(ncol(df)-1), kfolds = 10) 
{
    df <- as.data.frame(df)

    # randomly shuffle
    set.seed(seed)
    before_class_data <- df[sample(nrow(df)), ]
    
    # split into training and test data
    tt_cut <- cut(seq(1:nrow(df)), breaks = 3, labels = F)
    
    set.seed(seed)
    idx <- sample(1:3, 1)
    
    # split it up!
    testCut_idx <- which( tt_cut == idx )
    testing_data <- df[testCut_idx, ]
    training_data <- df[-testCut_idx, ]
    
    # train our model with cross validation using the training_data
    cut_data <- cut(seq(1:nrow(training_data)), breaks = kfolds, labels = F)
    
    cross_val_accuracy <- c()
    
    tstart <- proc.time()
    for( m in mtrys ) {
        accuracies <- c()
        for ( k in 1:kfolds ) {
            test_idx <- which( cut_data == k )
            temp_test_data <- training_data[test_idx, ]
            temp_training_data <- training_data[-test_idx, ]
            
            set.seed(seed)
            temp_rf <- randomForest(pass ~ ., data = temp_training_data, ntree = 500, mtry = m, importance = T)
            rf_pred <- predict(temp_rf, newdata = temp_test_data)
            cmat <- table("true" = temp_test_data%>%pull(pass), "pred" = rf_pred)
            
            accuracies <- c(accuracies, sum(diag(cmat))/sum(cmat) )
            print(sprintf("m = %d   k = %d", m, k))
        }
        cross_val_accuracy <- c(cross_val_accuracy, mean(accuracies))
    }
    tend <- proc.time()
    print(tend - tstart)
        
    #### plot mtrys
    g <- qplot(mtrys, cross_val_accuracy, geom = "line") + theme_bw() + 
        ggtitle("Hyper Parameter Tuning") +
        xlab("mtry") +
        ylab("Prediction Accuracy")
    print(g)
    #### Run the prediction
    set.seed(seed)
    final_rf_model <- randomForest(pass ~ ., data = training_data, ntree = 500, 
                                   mtry = which.max(cross_val_accuracy))

    return(list("training" = training_data, "testing" = testing_data, "model" = final_rf_model))
}

plotImportanceRF<- function(imp, title = "", ylab = "Variable", 
                            xlab = "Importance (MDG)")
{
    imp_df <- data.frame(var = rownames(imp), importance = imp[,1])
    ggplot(imp_df, aes(importance, fct_reorder(var, importance))) +
        geom_point() + theme_bw() + ggtitle(title) + xlab(xlab) +
        ylab(ylab)
}


#####
data <- read_csv("data_stat119_clean.csv")

### make binary classification
failingGrade <- c("C-", "D+", "D", "D-", "F", "WU", "I", "NC")

data <- data %>% add_column( pass = rep(NA, nrow(data)) )
data$pass[data$grade %in% failingGrade] <- "fail"
data$pass[!(data$grade %in% failingGrade)] <- "pass"
data$pass <- as.factor(data$pass)

#data_raw <- data_raw %>% select(-grade, -term.gpa)

#####
##### make our different data sets
#####

# grab only data before the class starts
before_class_data <- data %>% select(period, enrollment.sts.desc, gender, birth.year, 
                                     ethnicity.desc, hsgpa, ap.credits, trans.units.acc, ed.father.parent.guardian1,
                                     ed.mother.parent.guardian2, satrd.composite, satrd.math,
                                     pass, college, total.units.earned.before, class)
makeFactor <- c("period", "enrollment.sts.desc", "ethnicity.desc", "college", "birth.year",
                "gender", "class")
before_class_data[, makeFactor] <- lapply(before_class_data[, makeFactor], as.factor)
before_class_data <- as_tibble(before_class_data)
before_class_data$ed.father.parent.guardian1 <- factor(before_class_data$ed.father.parent.guardian1, 
                                                            levels = c("No high school","Some high school","High school grad",
                                                                       "Some college","2 yr college grad","4 yr college grad",
                                                                       "Post Graduate"), ordered = T)
before_class_data$ed.mother.parent.guardian2 <- factor(before_class_data$ed.mother.parent.guardian2, 
                                                       levels = c("No high school","Some high school","High school grad",
                                                                  "Some college","2 yr college grad","4 yr college grad",
                                                                  "Post Graduate"), ordered = T)
######
######
# four weeks into class
four_week_data <- bind_cols(before_class_data, data%>%select(hwk1, hwk2, hwk3, hwk4, quiz1chapters28, quiz2chapter3, quiz4chapter5))


###### Run analysis of before_class_data
########################################
before_class_run <- bestRandomForest(before_class_data)

before_class_pred <- predict(before_class_run$model, newdata = before_class_run$testing)
before_class_cmat <- table("true" = before_class_run$testing$pass, "pred" = before_class_pred )

before_class_accuracy <- sum(diag(before_class_cmat)) / sum(before_class_cmat)
before_class_missclass <- 1 - before_class_accuracy

plotImportanceRF(importance(before_class_run$model))

print(sprintf( "Before Class Start Missclassification: %.2f%% ...", before_class_missclass*100 ))

###### Run analysis of four_week_data
#####################################
four_week_run <- bestRandomForest(four_week_data)
four_week_pred <- predict(four_week_run$model, newdata = four_week_run$testing)
four_week_cmat <- table("true" = four_week_run$testing$pass, "pred" = four_week_pred )

four_week_accuracy <- sum(diag(four_week_cmat)) / sum(four_week_cmat)
four_week_missclass <- 1 - four_week_accuracy

plotImportanceRF(importance(four_week_run$model))

print(sprintf( "Four Weeks in Start Missclassification: %.2f%% ...", four_week_missclass*100 ))

