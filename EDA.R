#####EDA#####
library(tidyverse)
library(ggplot2)
library(scales)
complete<-read_csv(file.choose())
data<-complete %>% select (acad.sts.desc, ap.credits, birth.year, college, enrollment.sts.desc, ethnicity.desc, gender, grade, hsgpa, matric.period, period, satrd.composite, satrd.math, ed.father.parent.guardian1, ed.mother.parent.guardian2, total.units.earned.before, trans.units.acc, hwk1, hwk2, hwk3, hwk4, quiz1chapters28, quiz2chapter3, quiz3chapter4, quiz4chapter5)
### make binary classification
failingGrade <- c("C-", "D+", "D", "D-", "F", "WU", "I", "NC")

data$Result <- rep(NA, nrow(data))
data$Result[data$grade %in% failingGrade] <- "fail"
data$Result[!(data$grade %in% failingGrade)] <- "pass"
data$Result <- as.factor(data$Result)
demo<-data %>% select (acad.sts.desc, ap.credits, birth.year, college, enrollment.sts.desc, ethnicity.desc, gender, Result, hsgpa, matric.period, period, satrd.composite, satrd.math, ed.father.parent.guardian1, ed.mother.parent.guardian2, total.units.earned.before, trans.units.acc)
classdat<-data %>% select (Result,hwk1, hwk2, hwk3, hwk4, quiz1chapters28, quiz2chapter3, quiz3chapter4, quiz4chapter5)
prior<-data %>% select(ap.credits, hsgpa, satrd.composite, satrd.math, total.units.earned.before, trans.units.acc, Result)
### make binary classification )

##Descriptive Statistics##

#Grade Distribution Before Converting to P/F
ggplot(data,aes(grade))+geom_bar(aes(fill=Result))+xlab("Grade")+theme_classic()+theme(legend.position = "none")+scale_x_discrete(limits=c("A","A-","B+","B","B-","C+","C","CR","C-","D+","D","D-","F","WU","I"))
                                                                                                                                                                    
table(data$gender,data$Result)
#First Time Students and Transfers
data$firsttime<-case_when(
  ((data$period-data$matric.period)==0 & data$enrollment.sts.desc=="1ST TIME STUDENT")~"Freshman",
  ((data$period-data$matric.period)==0 & data$enrollment.sts.desc=="TRANSFER")~"Transfer",
  (TRUE~"Not First Semester"))
ggplot(data,aes(fill=Result, x=firsttime))+geom_bar(position="dodge")+theme_classic()+xlab("New Student Status")+theme(legend.position="none")
table(data$firsttime,data$Result)
#Class standing
data$matric.period<-as.numeric(as.character(data$matric.period))
data$period<-as.numeric(as.character(data$period))
data$class<-case_when(
  ((data$period-data$matric.period) %in% c(0,8) & data$enrollment.sts.desc=="1ST TIME STUDENT")~"1st-Year",
  ((data$period-data$matric.period) %in% c(2, 10, 18) & data$enrollment.sts.desc=="1ST TIME STUDENT") ~ "2nd-Year",
  ((data$period-data$matric.period) %in% c(12, 20, 28) & data$enrollment.sts.desc=="1ST TIME STUDENT") ~ "3rd-Year",
  ((data$period-data$matric.period) %in% c(22, 30, 38) & data$enrollment.sts.desc=="1ST TIME STUDENT") ~ "4th-Year",
  ((data$period-data$matric.period) >39 & data$enrollment.sts.desc=="1ST TIME STUDENT") ~ "5th Year+",
  ((data$period-data$matric.period) %in% c(0,8) & data$enrollment.sts.desc=="TRANSFER") ~ "3rd-Year",
  ((data$period-data$matric.period) %in% c(2,10,18) & data$enrollment.sts.desc=="TRANSFER") ~ "4th-Year",
   (TRUE~"5th Year+"))
ggplot(data,aes(fill=Result, x=class))+geom_bar(aes(fill=Result),position="dodge")+theme_classic()+theme(legend.position = "none")+scale_x_discrete(limits=c("1st-Year","2nd-Year","3rd-Year","4th-Year","5th Year+"))

write_csv(data, "new.csv")
#HSGPA
ggplot(data, aes(x=class, y=hsgpa, fill=Result)) +geom_boxplot(aes(fill=Result))+labs(x="Year", y = "High School GPA")+
  theme_classic()+theme(legend.position="none")+scale_x_discrete(limits=c("1st-Year","2nd-Year","3rd-Year","4th-Year","5th Year+"))

#MathSAT
ggplot(data, aes(x=class, y=satrd.math, fill=Result)) +geom_boxplot(aes(fill=Result))+labs(x="Year", y = "Math SAT")+
  theme_classic()+theme(legend.position="none")+scale_x_discrete(limits=c("1st-Year","2nd-Year","3rd-Year","4th-Year","5th Year+"))

#Per Period
data$period<-as.factor(case_when(
  data$period==20174 ~ "Fall17",
  data$period==20182 ~ "Spring18",
  data$period==20184 ~ "Fall18"
))
ggplot(data,aes(fill=Result, x=period))+geom_bar(position="dodge")+theme_classic()+coord_flip()+theme(legend.position = "none")+xlab("Period")
table(data$period,data$Result)


#HSGPA, SATCOMP, Grade
ggplot(data, aes(hsgpa,satrd.composite))+geom_point(aes(colour=factor(Result)))+theme_classic()+theme(legend.position="none")+xlab("High School GPA")+ylab("SAT Composite")
#College and Grade
ggplot(data,aes(fill=Result, x=college))+geom_bar(position="dodge")+theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")+xlab("College")+scale_x_discrete(limits=c("Business","Health & Human Services","undeclared","Arts & Letters","Professional Studies & Fine Arts","Sciences","Engineering","Education"))
table(data$college,data$Result)

#Correlation plot demographics
library(GGally)
library(ggcorrplot)
library(reshape2)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggcorrplot")
#my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
#  ggplot(data = data, mapping = mapping) +
#    geom_bin2d(...) +
#    scale_fill_gradient(low = low, high = high)
#}
#ggpairs(prior, lower=list(combo=wrap("facethist", binwidth=1), 
#                       continuous=wrap(my_bin, binwidth=0.25)))
prior$Result<-ifelse(prior$Result=="pass",1,0)
#corr <- round(cor(prior), 1)
#p.mat <- cor_pmat(prior)
#ggcorrplot(prior,method="circle")
cormat <- round(cor(prior),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

# Heatmap
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
