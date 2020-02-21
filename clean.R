# this cleans and imputes the data
library(tidyverse)
library(mice)

theme_set(theme_bw())

# some useful functions
plotMissing <- function(df, title = "Plot of Missingness", xlab = "Var", ylab = "Count")
{
    df_count <- sapply(df, function (x) sum(is.na(x)))
    p <- qplot(fct_reorder(names(df_count), df_count), df_count, geom="col") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = xlab, y = ylab, title = title)
    print(p)
    df_count
}


# import the data
raw_data <- read_csv("~/Dropbox/class/fanResearch/data/stat119_data.csv")

# drop the obvious variables/ones that don't make sense
dropped_data <- raw_data %>% select( names(raw_data)[ !grepl(".possible$", names(raw_data)) ])
dropped_data <- dropped_data %>% select( -aggregatetotal, -courselettergrade, -final, -gradewith119a,
                                         -worksheet, -enrollment.sts, -gender.desc, -hegis.major, -cipcode,
                                         -attend.sts, -second.cip.code, -second.hegis.major, -second.major,
                                         -second.major.name, -third.major, -third.hegis.major, -third.cip.code,
                                         -third.major.name, -monday827, -quiz0, -decline.to.state, -hispanic.latino,
                                         -hispanic.latino.bckgrnd, -hispanic.latino.bckgrnd.desc, -hispanic.latino.other,
                                         -amer.indian.alaska.native, -amer.indian.alaska.native.desc, -amer.indian.other,
                                         -alaska.native.other, -asian, -asian.desc, -asian.other, -black, -black.desc,
                                         -black.other, -pacific, -pacific.desc, -pacific.other, -white, -white.desc,
                                         -white.other, -acad.sts, -ed.level.father, -ed.level.mother, -primary.major)

#### fixes dashes
for (var in names(dropped_data)) {
    col <- dropped_data[[var]]
    col[col == "-"] <- NA
    dropped_data[[var]] <- col
}

# take advantage of read_csv automatically detecting column type
write_csv(dropped_data, "temp.csv")
dropped_data <- read_csv("temp.csv")
#dropped_data <- as_tibble(apply(dropped_data, 2, function(x) { x[x == "-"] <- NA; x; }))

data <- dropped_data
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
dropped_data <- data

#### show missingness on demographic and homeworks separately
demo_vars <- c("matric.period", "enrollment.sts.desc", "gender", "birth.year",
               "ethnicity.desc", "hsgpa", "ap.credits", "trans.units.acc",
               "ed.father.parent.guardian1", "ed.mother.parent.guardian2",
               "major.name", "sat.verbal", "satmath", "sat.composite", "satrd.verbal",
               "satrd.math", "satrd.composite", "act.eng", "act.math", "act.composite",
               "term.unit.enrolled", "termunits.earned", "total.units.earned",
               "term.gpa", "total.gpa", "acad.sts.desc", "grade", "college", "si.attendance",
               "class", "uuid")
demographic_data <- dropped_data %>% select(demo_vars)
hw_data <- dropped_data %>% select(exam1, exam2, names(dropped_data)[grep("^(hwk|quiz)", names(dropped_data))],
                                   clickertotal)

plotMissing(hw_data)
demo_count <- plotMissing(demographic_data)
demo_count

##### impute variables 
# missing ap credits and transfer credits are 0
dropped_data$ap.credits[is.na(dropped_data$ap.credits)] <- 0
dropped_data$trans.units.acc[is.na(dropped_data$trans.units.acc)] <- 0

# missing academic status is a student in good standing
dropped_data$acad.sts.desc[is.na(dropped_data$acad.sts.desc)] <- "GOOD STANDING"
unique(dropped_data$acad.sts.desc)

#### handle missingness in the standardized testing
act_to_sat <- read_csv("act_to_sat.csv")
act_to_sat_math <- read_csv("act_to_sat_math.csv")

# impute act.composite to satrd.composite
mask_act_to_sat <- ( !is.na(dropped_data$act.composite) & is.na(dropped_data$satrd.composite) )
dropped_data$satrd.composite[mask_act_to_sat] <- act_to_sat$SAT[ match(dropped_data$act.composite[mask_act_to_sat], act_to_sat$ACT) ]

# take the max between satrd.composite and sat.composite
mask_max_sat_satrd <- ( !is.na(dropped_data$satrd.composite) & !is.na(dropped_data$sat.composite) )
dropped_data$satrd.composite[mask_max_sat_satrd] <- pmax(dropped_data$sat.composite[mask_max_sat_satrd],
                                                        dropped_data$satrd.composite[mask_max_sat_satrd])
# merge sat.composite into missing satrd.composite
mask_merge_sat_to_satrd <- ( !is.na(dropped_data$sat.composite) & is.na(dropped_data$satrd.composite) )
dropped_data$satrd.composite[mask_merge_sat_to_satrd] <- dropped_data$sat.composite[mask_merge_sat_to_satrd]

### do the same for just math scores
# impute act.math to satrd.math
mask_act_to_sat <- ( !is.na(dropped_data$act.math) & is.na(dropped_data$satrd.math) )
dropped_data$satrd.math[mask_act_to_sat] <- act_to_sat_math$SAT[ match(dropped_data$act.math[mask_act_to_sat], act_to_sat_math$ACT) ]

# take the max between satrd and sat
mask_max_sat_satrd <- ( !is.na(dropped_data$satrd.math) & !is.na(dropped_data$satmath) )
dropped_data$satrd.math[mask_max_sat_satrd] <- pmax(dropped_data$satmath[mask_max_sat_satrd],
                                                         dropped_data$satrd.math[mask_max_sat_satrd])
# merge sat into missing satrd
mask_merge_sat_to_satrd <- ( !is.na(dropped_data$satmath) & is.na(dropped_data$satrd.math) )
dropped_data$satrd.math[mask_merge_sat_to_satrd] <- dropped_data$satmath[mask_merge_sat_to_satrd]

# drop sat.XX and act.XXX
dropped_data <- dropped_data %>% select(-satmath, -sat.verbal, -sat.composite, -act.math, 
                                        -act.eng, -act.composite, -satrd.verbal)

plotMissing(dropped_data)

# fill in missing class assignment scores with 0
dropped_data <- dropped_data %>% replace_na(list(exam1=0,exam2=0,hwk1=0,hwk2=0,hwk3=0,hwk4=0,hwk5=0,
                                                 hwk6=0,hwk7=0,hwk8=0,hwk9=0,hwk10=0,hwk11=0,
                                                 quiz1chapters28=0,quiz2chapter3=0,quiz3chapter4=0,
                                                 quiz4chapter5=0,quiz5chapter6=0,quiz6chapter7=0,
                                                 quiz7chapter9=0,quiz7chapter9=0,quiz8chapter10=0,
                                                 quiz9chapter11a=0,quiz10chapter11b=0)) 

# drop parent's education becuase too much missing data to impute
#dropped_data <- dropped_data %>% select(-ed.father.parent.guardian1, -ed.mother.parent.guardian2,
#                                        -term.unit.enrolled, -termunits.earned)

#### Dropping missing values
# start dropping those missing that can't be easily imputed
dropped_data <- dropped_data %>% drop_na(grade, college)

plotMissing(dropped_data)

#####




### make some columns into factored data
makeFactor <- c("period", "matric.period", "enrollment.sts.desc", "gender", "birth.year", "ethnicity.desc",
                "major.name", "acad.sts.desc","grade", "college", "ed.father.parent.guardian1", "ed.mother.parent.guardian2")

dropped_data[, makeFactor] <- lapply(dropped_data[, makeFactor], as.factor)
dropped_data <- as.data.frame(dropped_data)

#### lets imput hsgpa, satrd.composite, and satrd.math
for_mice_data <- dropped_data %>% select(-uuid)

# we will only keep the imputed hsgpa and satrd scores
imp <- mice(for_mice_data, m = 5, method = "rf", maxit = 5, seed = 42)
data_imputed <- complete(imp)

# grab imputed values
dropped_data$hsgpa <- data_imputed$hsgpa
dropped_data$satrd.composite <- data_imputed$satrd.composite
dropped_data$satrd.math <- data_imputed$satrd.math

plotMissing(dropped_data)

# make new variable of units before class starts
dropped_data$total.units.earned.before <- dropped_data$total.units.earned - dropped_data$termunits.earned

complete_data <- dropped_data %>% drop_na() # still a good amount of data
