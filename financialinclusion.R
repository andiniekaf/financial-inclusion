##SETTING A WORKING DIRECTORY##
setwd("*your working directory here")

##LOADING THE NECESSARY PACKAGES FROM THE LIBRARY##
library(boot) #needed for the bootstrapping process I'm going to run later#
library(readxl) #because the raw dataset is in the Ms. Excel format#
library(margins)
library(caret) #needed for the confusion matrix calculation I'm going to run later#
library(pR0C) #needed for the confusion matrix calculation I'm going to run later#
library(e1071) #needed for the confusion matrix calculation I'm going to run later#

##RETRIEVING THE DATASET##
fin_inc <- read_excel("micro_idn_varname.xls")[,c("wpid_random","female","age","educ","inc_q","emp_in","fin2","fin17a","fin22a")]

##RENAMING THE COLUMNS##
colnames(fin_inc)[colnames(fin_inc) == "fin2"] <- "formal_account" 
colnames(fin_inc)[colnames(fin_inc) == "fin17a"] <- "formal_saving"
colnames(fin_inc)[colnames(fin_inc) == "fin22a"] <- "formal_borrow"
colnames(fin_inc)[colnames(fin_inc) == "female"] <- "gender"

##CHECK MISSING VALUES#
colSums(is.na(fin_inc)) #result: No missing value is found for all the variables#

##CHECK SUMMARY STATISTICS AND DATA TYPES FOR EACH OF THE VARIABLE##
summary(fin_inc)
str(fin_inc)
#takeaways: since the variables are mostly non-numeric, variable transformation (and generation) will be necessary#

##VARIABLE TRANSFORMATION AND GENERATION##
#Variable Transformation - "age"#
fin_inc$age <- is.numeric(fin_inc$age) #Variable "age" was previously non-numeric, this code is meant to transform age into a numeric variable#
str(fin_inc$age) 
#"age" is now already a numeric variable

fin_inc$age2 <- (fin_inc$age)^2 #generate a new variable (age squared) to look at whether there are non-linearistic effects of age on the Y variables

#Variable Transformation - "gender#
#Variable "gender" will be transformed into a dummy variable where if respondent is a female, it will be filled with "1", and "0" otherwise#
fin_inc$gender <- ifelse(fin_inc$gender == "Female",1,0)

#Variable Transformation - "educ" (Education)#
#Variable "educ" will be transformed into a dummy variable for each of the (n-1) category of "educ" inferred from the dataset#

unique(fin_inc$educ) #checking the categories of the "educ" (education) variable

#There are 3 categories of "educ" (education): "completed primary or less", "secondary", and "completed tertiary or more". The categorical baseline will be "completed primary or less"#
#Given the categorical baseline, two new dummy variables will be created: One variable for categorizing the "secondary" as 1, 0 otherwise, and the other for categorizing the "completed tertiary or more" as 1, 0 otherwise.

fin_inc$educ_secondary_d <- ifelse(fin_inc$educ == "secondary",1,0)
fin_inc$educ_tertiary_d <- ifelse(fin_inc$educ == "completed tertiary or more",1,0)
#After generating two new dummy variables out of the original "educ" variable, the original "educ" variable will be deleted#
fin_inc$educ <- NULL

#Variable Transformation - "inc_q" (Income Quintiles)#
#Variable "inc_q" will be transformed into a dummy variable for each of the (n-1) category of "inc_q" inferred from the dataset#

unique(fin_inc$inc_q) #checking the categories of the "inc_q" (Income Quintiles) variable

#There are 5 categories of "inc_q" variable: "Middle 20%", "Fourth 20%", "Richest 20%", "Poorest 20%", and "Second 20%". The categorical baseline will be the "Richest 20%"
#Given the categorical baseline, four new dummy variables will be created
fin_inc$inc_1st_quint_d <- ifelse(fin_inc$inc_q == "Poorest 20%",1,0)
fin_inc$inc_2nd_quint_d <- ifelse(fin_inc$inc_q == "Second 20%",1,0)
fin_inc$inc_3rd_quint_d <- ifelse(fin_inc$inc_q == "Middle 20%",1,0)
fin_inc$inc_4th_quint_d <- ifelse(fin_inc$inc_q == "Fourth 20%",1,0)
#After generating two new dummy variables out of the original "educ" variable, the original "educ" variable will be deleted#
fin_inc$inc_q <- NULL

#Variable Transformation - "emp_in" (Workforce Status)#
#Variable "emp_in" will be transfromed into a dummy variable where if employee is "in workforce", it will be coded as 1, and 0 otherwise
fin_inc$emp_in <- ifelse(fin_inc$emp_in == "in workforce",1,0)

#Variable Transformation -- "formal_account"
#Variable "formal_account" will be transformed into a dummy variable where if the respondent owns a formal account (debit card), it will be coded as 1, and 0 otherwise
fin_inc$formal_account <- ifelse(fin_inc$formal_account == "yes",1,0)

#Variable Transformation -- "formal_saving"
#Variable "formal_saving" will be transformed into a dummy variable where if the respondent has saved in the past 12 months, it will be coded as 1, and 0 otherwise
fin_inc$formal_saving <- ifelse(fin_inc$formal_saving == "yes",1,0)

#Variable Transformation -- "formal_borrow"
#Variable "formal_borrow" will be transformed into a dummy variable where if the respondent has borrowed money from bank/other type of financial institution in the past 12 months, it will be coded as 1, and 0 otherwise
fin_inc$formal_borrow <- ifelse(fin_inc$formal_borrow == "yes",1,0)

##RECHECK SUMMARY STATISTICs FOR EACH OF THE VARIABLE POST-TRANSFORMATION##
summary(fin_inc)

##CHECK CORRELATION MATRIX TO DETECT MULTICOLLINEARITY##
corr <- data.frame(round(cor(fin_inc),2))

##SAVING THE TRANSFORMED DATASET##
save(fin_inc, file = "fin_inc.RData")


##DATA ANALYSIS: ANALYSING THE EFFECT OF AGE, EDUCATION, INCOME, AND WORKFORCE STATUS ON FINANCIAL INCLUSION##

#Model 1: Analysing the effect of age, education, income, and workforce status on formal account ownership#
glm.formalaccount <- glm(formal_account ~ age + age2 + gender + educ_tertiary_d + educ_secondary_d + inc_1st_quint_d + inc_2nd_quint_d + inc_3rd_quint_d + inc_4th_quint_d + emp_in, data = fin_inc, family ="binomial")
summary(glm.formalaccount) #checking the result of the logistic regression#
margins.formalaccount <- summary(margins(glm.formalaccount))

#Model 2: Analysing the effect of age, education, income, and workforce status on formal saving behavior#
glm.formalsaving <- glm(formal_saving ~ age + age2 + gender + educ_tertiary_d + educ_secondary_d + inc_1st_quint_d + inc_2nd_quint_d + inc_3rd_quint_d + inc_4th_quint_d + emp_in, data = fin_inc, family ="binomial")
summary(glm.formalsaving) #checking the result of the logistic regression#
margins.formalsaving <- summary(margins(glm.formalsaving))

#Model 3: Analysing the effect of age, education, income, and workforce status on formal borrowing behavior#
glm.formalborrow <- glm(formal_borrow ~ age + age2 + gender + educ_tertiary_d + educ_secondary_d + inc_1st_quint_d + inc_2nd_quint_d + inc_3rd_quint_d + inc_4th_quint_d + emp_in, data = fin_inc, family ="binomial")
summary(glm.formalborrow) #checking the result of the logistic regression#
margins.formalborrow <- summary(margins(glm.formalborrow))

##ROBUSTNESS VALIDATION: CONFUSION MATRIX BETWEEN PREDICTED PROBABILITY AND ACTUAL FORMAL ACCOUNT OWNERSHIP, FORMAL SAVING BEHAVIOR, AND FORMAL BORROWING BEHAVIOR
#MODEL 1
fin_inc$prob_formal_account <- predict(glm.formalaccount, type="response")

#If an individual’s predicted base probability is greater than 0.5, we predict he/she will own a formal account
fin_inc$pred_formal_account <- ifelse(fin_inc$prob_formal_account > 0.5,1,0)

#Now we compute a confusion matrix between predicted probability and actual formal account ownership
confusionMatrix(table(fin_inc$pred_formal_account,fin_inc$formal_account),positive = "1")
#Based on the calculation of the confusion matrix, Model 1 has an overall accuracy rate of 71%

#MODEL 2
fin_inc$prob_formal_saving <- predict(glm.formalsaving, type="response")

#If an individual’s predicted base probability is greater than 0.5, we predict he/she will have saved in the past 12 months
fin_inc$pred_formal_saving <- ifelse(fin_inc$prob_formal_saving > 0.5,1,0)

#Now we compute a confusion matrix between predicted probability and actual formal saving behavior
confusionMatrix(table(fin_inc$pred_formal_saving,fin_inc$formal_saving),positive = "1")
#Based on the calculation of the confusion matrix, Model 2 has an overall accuracy rate of 75%

#MODEL 3
fin_inc$prob_formal_borrow <- predict(glm.formalborrow, type="response")

#If an individual’s predicted base probability is greater than 0.4, we predict he/she will have borrowed from bank/other types of fin institution in the past 12 months
fin_inc$pred_formal_borrow <- ifelse(fin_inc$prob_formal_borrow > 0.4,1,0)

#Now we compute a confusion matrix between predicted probability and actual formal borrowing behavior
confusionMatrix(table(fin_inc$pred_formal_borrow,fin_inc$formal_borrow),positive = "1")
#Based on the calculation of the confusion matrix, Model 3 has an overall accuracy rate of 81.3%

##ROBUSTNESS VALIDATION: USING BOOTSTRAPPING METHOD TO CHECK THE CONSISTENCY OF OVERALL ACCURACY RATE
#Bootstrapping method: replicate the calculation of accuracy rate (confusion matrix) for 400 times using a randomized smaller set of sample
#Sample: 700

#MODEL 1
conf_formalaccount_boot <- rep(NA, times=400)
for (k in 1:400){fin_inc_boot <- fin_inc[sample(nrow(fin_inc), 700), ]
glm.formalaccount.boot <- glm(formal_account ~ age + age2 + gender + emp_in + educ_tertiary_d + educ_secondary_d + inc_4th_quint_d + inc_3rd_quint_d + inc_2nd_quint_d + inc_1st_quint_d, data = fin_inc_boot, family = "binomial")
prob_formalaccount_boot <- predict(glm.formalaccount.boot, type = "response")
pred_formalaccount_boot <- ifelse(prob_formalaccount_boot > 0.5,1,0)
conf_formalaccount_boot[k] <- mean(fin_inc_boot$formal_account == pred_formalaccount_boot)}

mean(conf_formalaccount_boot)
#The accuracy rate mean of the bootstrapped sample is 71.08%#
hist(conf_formalaccount_boot) #checking the deviation of the accuracy rate

#MODEL 2
conf_formalsaving_boot <- rep(NA, times=400)
for (k in 1:400){fin_inc_boot <- fin_inc[sample(nrow(fin_inc), 700), ]
glm.formalsaving.boot <- glm(formal_saving ~ age + age2 + gender + emp_in + educ_tertiary_d + educ_secondary_d + inc_4th_quint_d + inc_3rd_quint_d + inc_2nd_quint_d + inc_1st_quint_d, data = fin_inc_boot, family = "binomial")
prob_formalsaving_boot <- predict(glm.formalsaving.boot, type = "response")
pred_formalsaving_boot <- ifelse(prob_formalsaving_boot > 0.5,1,0)
conf_formalsaving_boot[k] <- mean(fin_inc_boot$formal_saving == pred_formalsaving_boot)}

mean(conf_formalsaving_boot)
#The accuracy rate mean of the bootstrapped sample is 76.19%#
hist(conf_formalsaving_boot) #checking the deviation of the accuracy rate

#MODEL 3
conf_formalborrow_boot <- rep(NA, times=400)
for (k in 1:400){fin_inc_boot <- fin_inc[sample(nrow(fin_inc), 700), ]
glm.formalborrow.boot <- glm(formal_borrow ~ age + age2 + gender + emp_in + educ_tertiary_d + educ_secondary_d + inc_4th_quint_d + inc_3rd_quint_d + inc_2nd_quint_d + inc_1st_quint_d, data = fin_inc_boot, family = "binomial")
prob_formalborrow_boot <- predict(glm.formalborrow.boot, type = "response")
pred_formalborrow_boot <- ifelse(prob_formalborrow_boot > 0.4,1,0)
conf_formalborrow_boot[k] <- mean(fin_inc_boot$formal_borrow == pred_formalsaving_boot)}

mean(conf_formalborrow_boot)
#The accuracy rate mean of the bootstrapped sample is 75.94%#
hist(conf_formalborrow_boot) #checking the deviation of the accuracy rate

