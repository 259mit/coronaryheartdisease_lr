# DATASET DESCRIPTION ####

# The data has 4238 rows and 16 columns

#male 0 = Female; 1 = Male
#age Age at exam time.
#education 1 = Some High School; 2 = High School or GED; 3 = Some College or Vocational School; 4 = college
#currentSmoker 0 = nonsmoker; 1 = smoker
#cigsPerDaynumber of cigarettes smoked per day (estimated average)
#BPMeds0 = Not on Blood Pressure medications; 1 = Is on Blood Pressure medications
#prevalent Stroke
#prevalent Hyp
#diabetes 0 = No; 1 = Yes
#totChol mg/dL
#sysBP mmHg
#diaBP mmHg
#BMIBody Mass Index calculated as: Weight (kg) / Height(meter-squared)
#heartRate Beats/Min (Ventricular)
#glucose mg/dL
#TenYearCHD 10 year risk of coronary heart disease CHD; 0 = No; 1 = Yes
data<-framingham
View(data)
# LIBRARIES USED IN THE PROJECT ####

install.packages("corrplot")
install.packages("Amelia")
library(corrplot)
library(Amelia)
library(ISLR)
library(MASS)
library(leaps)
library(caret)
library(dplyr)
library(caTools)

# FUNCTIONS USED IN THE PROJECT ####

replacenavalueswithmean<-function(variablex){
  bogx<-is.na(variablex);
  cleanvariablex<-variablex[!bogx];
  variablex[bogx]<-mean(cleanvariablex);
  variablex
}

getmode<-function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# DATA CLEANING NA ########
# dimensions
dim(data)
# [1] 4238   16

#structure
str(data)

# step1: FIND NA VALUES

#how many?
sum(is.na(data))
# [1]645

# which columns have NA Values?
colnames(data)[colSums(is.na(data))>0]
missmap(data)
for(i in 1:16){
  paste(i, names(data)[i], sum(is.na(data[,i]))) %>% print()
}
# 
# [1] "education=105, factor"  "cigsPerDay=29, num" "BPMeds=53, binary"    
# [4] "totChol=50, num"    "BMI=19, num"        "heartRate=1, num" 
# [7] "glucose=388, num"

#Inference:

# EDUCATION shows levels, i.e. 1,2,3,4; not sufficient information to 
# support any relation with other variables; replaced with mode;
# Missing completely at Random
k<-na.omit(data$education)
mk<-getmode(k)
en<-is.na(data$education)
data$education[en]<-mk
data$education<-as.factor(data$education)
  
  
# CIGSPERDAY shows 29 NAs, mean of male smokers is 22, whilst mean of female smokers is 14
sum(is.na(data$cigsPerDay))
cma<-which(is.na(data$cigsPerDay)&data$currentSmoker==1&data$male==1)
cfe<-which(is.na(data$cigsPerDay)&data$currentSmoker==1&data$male==0)
sma<-data[data$currentSmoker==1&data$male==1,]
sfe<-data[data$currentSmoker==1&data$male==0,]
# mean observed using summary()
summary(sma$cigsPerDay); summary(sfe$cigsPerDay)
data$cigsPerDay[cma]<-c(22.11)
data$cigsPerDay[cfe]<-c(14.13)

#BPMEDS is an intersting variable. Here we would need the variable PrevalentHyp to 
# draw some inference. so we see that those who don't have prevalent hypertension they dont
# take any BP medications.
table(data$BPMeds, data$prevalentHyp)
# So, we can say out of the NAs in BPMeds, those having PrevalentHyp
# as 1, might be omitted. as we see that only 10% of those having Hypertension take medicines.
# 31 values are replaced, remaining 22 will be omitted
gbp<-which(is.na(data$BPMeds)&data$prevalentHyp==0)
data$BPMeds[gbp]<-c(0)

#TOTCHOL is a variable having numeric values, trends between gender show some
# significant difference as compared to its overall mean. so replaced by mean wrt gender
dmale<-subset(data, data$male==1)
dfem<-subset(data, data$male==0)
aov(data$totChol ~ data$male) %>% summary()
# mean observed using summary() command
gTC<-which(is.na(data$totChol)&data$male==1)
data$totChol[gTC]<-c(233.1)
fTC<-which(is.na(data$totChol)&data$male==0)
data$totChol[fTC]<-c(239.5)

#HEARTRATE is a numeric variable and NA is just one value, negligible.
# so the NA value has been omitted.

#Glucose is a numeric variable. there is negligible difference wrt gender.
# It has high correlation with another variable, diabetes.
# so we use that to replace the NA values with mean.
ddia<-subset(data,data$diabetes==1)
dndia<-subset(data,data$diabetes==0)
# mean observed using summary() command
gD<-which(is.na(data$glucose)&data$diabetes==1)
data$glucose[gD]<-c(170.3)
data$glucose[which(is.na(data$glucose))]<-c(79.49)

#BMI is at present a numeric variable.
# Generally BMI is a parameter assessed in ranges, i.e. "normal" or "overweight" or "obese"
# so we use the same here, after converting to factors
# the NA values are replaced with the mode of the variable.

# Now only for this column we will be eliminating the outliers 
# before other columns as once we factor it
# we cant remove outliers
bmh<-which(data$BMI>46)
# The next values after 46 happen to be 51.28 & 56.8 
# which are unusually large values for BMI
data$BMI[bmh]
bmh
outbmi1<-which(data$BMI==56.8)
outbmi2<-which(data$BMI==51.28)
outbmi<-c(outbmi1,outbmi2)

# As factor
data$BMI[data$BMI>15&data$BMI<18.5]<-"Underweight"
data$BMI[data$BMI>=18.5&data$BMI<25]<-"Normal"
data$BMI[data$BMI>=25&data$BMI<30]<-"Overweight"
data$BMI[data$BMI>=30&data$BMI<40]<-"Obese"
data$BMI[data$BMI>=40&data$BMI<50]<-"HighlyObese"

# Replacing the NA
f<-which(is.na(data$BMI))
data$BMI[f]<-getmode(data$BMI)

# Outliers as NA
data$BMI[outbmi]<-NA

# AT THE END OF THIS STEP, THERE WILL BE 25 NAs, WHICH
# WILL BE OMITTED
# 22- BPMeds
# 1- heartRate
# 2- BMI (OUTLIERS)

data<-na.omit(data)

# AT THE END OF THIS STEP,
# ALL THE BINARY VARIABLES REMAIN AS NUMERIC DATATYPE

# DATA CLEANING DATATYPE CONVERSION ####

# CHECK STRUCTURE
str(data)

# Changing heartRate, age to numeric and BPMeds to integer
data$heartRate<-as.numeric(data$heartRate)
data$age<-as.numeric(data$age)
data$BPMeds<-as.integer(data$BPMeds)

# Changing all the integer datatype columns to factor
data$male<-as.factor(data$male)
data$currentSmoker<-as.factor(data$currentSmoker)
data$BPMeds<-as.factor(data$BPMeds)
data$prevalentStroke<-as.factor(data$prevalentStroke)
data$prevalentHyp<-as.factor(data$prevalentHyp)
data$diabetes<-as.factor(data$diabetes)
data$BMI<-as.factor(data$BMI)
data$TenYearCHD<-as.factor(data$TenYearCHD)

# Check any error or NAs produced till now
sum(is.na(data))
#[1] 0 ; we are good to proceed ahead!

# DATA CLEANING OUTLIERS ####
# step1: FIND outliers

#SBP: 295, INDEX=479
# THE PERSON HERE IS OBESE
# HERE THIS IS A HIGH VALUE BUT NOT WAY TOO EXTREME
# HIGH LEVERAGE POINT
sh<-which(data$sysBP>250)
sh
data$sysBP[sh]

#BUT HERE THE DIFFERENCE IS HUGE, THEY SEEM TO
#BE WRONGLY ENTERED VALUES
#totChol:600 696, INDEX=1104 ,3140
th<-which(data$totChol>464)
th
data$totChol[th]
###
outliers<-c(th)
###

# REMOVING OUTLIERS
data[outliers,]<-NA
sum(is.na(data))
data<-na.omit(data)

# THE DATA IS CLEANED
# DIMENSIONS
dim(data)
# Rows: 4211 ; Columns: 16

# VARIABLE ANALYSIS ####

# First we check the correlation between variables
# since we introduced factors
# we will take raw data again
f<-framingham
f<-na.omit(f)
Q<-cor(f)
corrplot(Q, method = "color")
# for model the more significant of the other will be chosen
# we see that there is very high correlation between
# sysBP:diaBP
# currentSmoker:cigsPerDay; 
# moderate-high correlation between
# diabetes:glucose
# moderate correlation between
# sysBP,diaBP:prevalentHyp

# SUBSETTING ####

# 80:20 ratio of Train:Test
set.seed(123)
split<-sample.split(data$TenYearCHD, SplitRatio = 0.8)
train<-subset(data, split==T)
test<-subset(data, split==F)

# SET REFERENCE LEVELS FOR FACTORS
data$education<-relevel(data$education,"1")
data$BMI<-relevel(data$BMI,"Normal")

# LOGISTIC REGRESSION
# BACKWARD SELECTION
#step1: select all features;
 m1<- glm(TenYearCHD~., data = train, family = "binomial")
 summary(m1)
#remove lesser significant variable that is  correlated
#with the other variable
 m2<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes, data = train, family = "binomial")
 summary(m2)
#remove lesser significant variables (heartRate)
 m3<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes-heartRate, data = train, family = "binomial")
 summary(m3)
#remove lesser significant variables (BMI)
 m4<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes-heartRate-BMI, data = train, family = "binomial")
 summary(m4)
#remove lesser significant variables (BPMeds)
 m5<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes-heartRate-BMI-BPMeds, data = train, family = "binomial")
 summary(m5)
#remove lesser significant variables (education)
 m6<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes-heartRate-BMI-BPMeds-education, data = train, family = "binomial")
 summary(m6)
#remove lesser significant variables (totChol)
 m7<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes-heartRate-BMI-BPMeds-education-totChol, data = train, family = "binomial")
 summary(m7)
#remove lesser significant variables (prevalentStroke)
 m8<- glm(TenYearCHD~.-currentSmoker-diaBP-prevalentHyp-diabetes-heartRate-BMI-BPMeds-education-totChol-prevalentStroke, data = train, family = "binomial")
 summary(m8)
 #ALL SIGNIFICANT
#CHECK CORRELATION IF()
 f<-na.omit(framingham)
 sda<-data.frame(f$male,f$age,f$cigsPerDay,f$sysBP,f$glucose)
 Q1<-cor(sda)
 corrplot(Q1)
#VERIFIED NO STRONG CORRELATION BETWEEN VARIABLES

# MODEL ####
 model1<-glm(TenYearCHD~male+age+cigsPerDay+sysBP+glucose, data = train, family = "binomial")
 summary(model1)
 #AIC=2574.7
 
# PREDICTION ####
 #model1
 p1<-predict(model1, newdata = test, type = "response")
 p1<-ifelse(p1>0.5,"1","0")
 f1<-as.character(test$TenYearCHD)
 
# CONFUSION MATRIX ####
 #model1
 t1<-table(f1,p1)
 t1
 #      p1
 #f1    0   1
 #0    712   3
 #1    117  10
 #ACCURACY1= 0.8574822
 a1<-(t1[1,1]+t1[2,2])/sum(t1);a1

# CROSS VALIDATION ON RAW DATASET ####
 trainc<-trainControl(method="repeatedcv",number = 10,savePredictions = TRUE)
 cv.mod1<-train(TenYearCHD~male+age+cigsPerDay+sysBP+glucose, data = data, family = "binomial", method="glm", trControl=trainc)
 cv.mod1
 #model1 accuracy:0.854193 (5 predictors)
 
# BEST MODEL ####
 #MODEL 1
 model1<-glm(TenYearCHD~male+age+cigsPerDay+sysBP+glucose, data = train, family = "binomial")
 summary(model1)
 
# INFERENCE ####
 #Lets have a look at the confusion matrix again
 #      p1
 #f1    0   1
 #0    712   3
 #1    117  10
 
 # True Positives:10
 # True Negatives:712
 # False Positives:3
 # False Negatives:117

 # The model predicts 99.6% of the negatives correctly

 
 
