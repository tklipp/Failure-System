    # some libraries
library(tidyverse)
library(randomForest)

seed <- 66

# some functions
plotImportanceRF<- function(imp, title = "", ylab = "Variable", 
                            xlab = "Importance (MDA)")
{
    imp_df <- data.frame(var = rownames(imp), importance = imp[,3])
    ggplot(imp_df, aes(importance, fct_reorder(var, importance))) +
        geom_point() + theme_bw() + ggtitle(title) + xlab(xlab) +
        ylab(ylab)
}


# read in data
data_raw <- read_csv("stat119_clean_data.csv")

### make some columns into factored data
makeFactor <- c("period", "matric.period", "enrollment.sts.desc", "gender", "birth.year", "ethnicity.desc",
                "ed.father.parent.guardian1", "ed.mother.parent.guardian2", "major.name", "acad.sts.desc",
                "grade", "college")
#makeFactor <- c("period", "matric.period", "enrollment.sts.desc", "gender", "birth.year", "ethnicity.desc",
#                "major.name", "acad.sts.desc", "grade", "college")


data_raw[, makeFactor] <- lapply(data_raw[, makeFactor], as.factor)

# drop period
data_raw <- data_raw %>% select(-period, -uuid, -major.name)

### make binary classification
failingGrade <- c("C-", "D+", "D", "D-", "F", "WU", "I", "NC")

data_raw$pass <- rep(NA, nrow(data_raw))
data_raw$pass[data_raw$grade %in% failingGrade] <- "fail"
data_raw$pass[!(data_raw$grade %in% failingGrade)] <- "pass"
data_raw$pass <- as.factor(data_raw$pass)

data_raw <- data_raw %>% select(-grade, -term.gpa)


# get complete data since randomForest can't handle it
data_complete <- data_raw[complete.cases(data_raw),]

############################################
## Run Cross-validation To Find Best mtry ##
############################################
# shuffle the data
set.seed(seed)
data_complete <- data_complete[sample(nrow(data_complete)),]

mtrys <- 1:25
kfolds <- 10

cut_data <- cut(seq(1:nrow(data_complete)), breaks = kfolds, labels = F)

cross_val_accuracy <- c()

tstart <- proc.time()
for( m in mtrys ) {
    accuracies <- c()
    for ( k in 1:kfolds ) {
        test_idx <- which( cut_data == k )
        test_data <- data_complete[test_idx, ]
        training_data <- data_complete[-test_idx, ]
        
        set.seed(seed)
        temp_rf <- randomForest(pass ~ ., data = training_data, ntree = 500, mtry = m)
        rf_pred <- predict(temp_rf, newdata = test_data)
        cmat <- table("pred" = rf_pred, "true" = test_data%>%pull(pass))
        
        accuracies <- c(accuracies, sum(diag(cmat))/sum(cmat) )
        print(sprintf("m = %d   k = %d", m, k))
    }
    cross_val_accuracy <- c(cross_val_accuracy, mean(accuracies))
}
tend <- proc.time()
print(tend - tstart)

qplot(mtrys, cross_val_accuracy, geom = "line") + theme_bw() + 
    ggtitle("Hyper Parameter Tuning") +
    xlab("mtry") +
    ylab("Prediction Accuracy")

#####################################
#### run randomForest importance ####
#####################################

set.seed(seed)
rf_all_vars <- randomForest(pass ~ ., data = data_complete, ntree = 500L,
                            mtry = which.max(cross_val_accuracy), importance = T)

# plot errors
plot(rf_all_vars)
rf_all_vars_imp <- importance(rf_all_vars)
# plot shows grades predict grades
plotImportanceRF(rf_all_vars_imp, title = "All Variables Importance")

### break demographic and assignements up
demographic_data <- data_complete %>% select(pass, matric.period, enrollment.sts.desc, gender, birth.year, ethnicity.desc,
                                        hsgpa, ap.credits, trans.units.acc, ed.father.parent.guardian1,
                                        ed.mother.parent.guardian2, satrd.math, satrd.composite,
                                        total.units.earned, total.gpa, acad.sts.desc, college,
                                        si.attendance)
class_data <- data_complete %>% select(pass, exam1, exam2, grep("^(hwk|quiz)", names(data_raw)), clickertotal)

## run demographic importance
set.seed(seed)
rf_demo <- randomForest(pass ~ ., data = demographic_data, ntree = 500L, 
                        mtry = which.max(cross_val_accuracy), importance = T)
plot(rf_demo)
rf_demo_imp <- importance(rf_demo)
plotImportanceRF(rf_demo_imp, title = "Demographic Importance")

## run class importance
set.seed(seed)
rf_class <- randomForest(pass ~ ., data = class_data, ntree = 500L, 
                         mtry = which.max(cross_val_accuracy), importance = T)
rf_class_imp <- importance(rf_class)
plotImportanceRF(rf_class_imp, title = "Class Material Importance")
