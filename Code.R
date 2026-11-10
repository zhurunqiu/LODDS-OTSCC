# Set up the environment
setwd('D:/Sample')
getwd()


1.0 ### Random split(7:3) 
library(caret)                
mm<- read.csv('SEER.csv')    
set.seed(1234)      
tr1<-createDataPartition(y=mm$lodds,           
                         p=0.9,              
                         list=FALSE,
                         times = 1) 
train<- mm[tr1,]   #70%
test<-mm[-tr1,]   #30%
write.csv(train,file= "train.csv")
write.csv(test,file ="test.csv")


2.0 ### Baseline characteristics（Table 1）
library(foreign)         
library(compareGroups)
library(glue)
library(Hmisc)
rm(list = ls()) 
aa<- read.csv('group.csv')
str(aa)
aa$time<-as.numeric(aa$time)    
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" ≤ 60 years ","> 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
aa$css<-factor(aa$css,levels = c(0,1),
               labels = c("Alive","Death"))
# Label
label(aa$age)<-"Age at diagnosis "
label(aa$sex)<-"Gender "
label(aa$race)<-"Race "
label(aa$grade)<-"Grade "
label(aa$t)<-"T_stage "
label(aa$n)<-"N_stage "
label(aa$rad)<-"Radiotherapy "

# Normal distribution
library(ggpubr)
ggqqplot(aa$lnr, color = "blue") + 
  ggtitle("QQ plot")

# Analysis
dd <- as.data.frame(aa)
restab <- descrTable(group ~ .- X - id - Age - m - Lnr - Lodds 
                     - sur - status,    
                     data = dd,
                     method = 2,
                     show.all=T)
restab
# Table
summary(dd)
export2word(restab, file='table1.docx')   # 保存Table 1 的数据


# Baseline characteristics (Europe)
library(dplyr)
rm(list = ls()) 
aa<- read.csv('Europe.csv')
str(aa)
aa$time<-as.numeric(aa$time)    
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" ≤ 60 years ","> 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
# Age
aa %>%
  count(age, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Sex
aa %>%
  count(sex, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Grade
aa %>%
  count(grade, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# T
aa %>%
  count(t, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# N
aa %>%
  count(n, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Stage
aa %>%
  count(stage, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# LNR
aa %>%
  count(lnr, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# LODDS
aa %>%
  count(lodds, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# PORT
aa %>%
  count(rad, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# ELN
aa$eln <- as.integer(aa$eln)
summary(round(aa$eln),0)

# PLN
aa$pln <- as.integer(aa$Pln)
summary(round(aa$Pln),0)

# Time
summary(aa$time)


# Baseline characteristics (China)
library(dplyr)
rm(list = ls()) 
aa<- read.csv('Chinese.csv')
str(aa)
aa$time<-as.numeric(aa$time)    
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" ≤ 60 years ","> 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
# Age
aa %>%
  count(age, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Sex
aa %>%
  count(sex, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Grade
aa %>%
  count(grade, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# T
aa %>%
  count(t, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# N
aa %>%
  count(n, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# Stage
aa %>%
  count(stage, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# LNR
aa %>%
  count(lnr, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# LODDS
aa %>%
  count(lodds, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# PORT
aa %>%
  count(rad, name = "Count") %>%         
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# ELN
aa$eln <- as.integer(aa$eln)
summary(round(aa$eln),0)

# PLN
aa$pln <- as.integer(aa$Pln)
summary(round(aa$Pln),0)

# Time
summary(aa$time)

# Validation cohort
aa<- read.csv('validation.csv')
# Time
summary(aa$time)


# Packages
library(survival)
library(survminer)
# Data
aa<-read.csv("SEER.csv")
aa$n<-factor(aa$n,levels = c(1,2,3,4),
                 labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
               labels = c("I","II","III","IV","V","VI"))

### Construct the model
fit_n <- coxph(Surv(time, css=="1") ~aa$n, data = aa)
fit_lnr <- coxph(Surv(time, css=="1") ~aa$lnr, data = aa)
fit_lodds <- coxph(Surv(time, css=="1") ~aa$lodds, data = aa)

# (R²)
null_fit <- coxph(Surv(time, css=="1") ~ 1, data = aa)
ll_null <- logLik(null_fit)[1]
N        <- nrow(aa)

# N stage
ll_n <- logLik(fit_n)[1]
r_squared <- 1 - exp((2 * (ll_null - ll_n)) / N)
aic_n    <- AIC(fit_n)
cindex_n <- concordance(fit_n)$concordance
# Print
cat("R²:", round(r_squared, 3), "\n")
cat("AIC:", round(aic_n, 1), "\n")
cat("C-index:", round(cindex_n, 3), "\n")
lower <- 0.634 - 0.007 * 1.96
upper <- 0.634 + 0.007 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# LNR
ll_lnr <- logLik(fit_lnr)[1]
r_squared <- 1 - exp((2 * (ll_null - ll_lnr)) / N)
aic_lnr    <- AIC(fit_lnr)
cindex_lnr <- concordance(fit_lnr)$concordance
# Print
cat("R²:", round(r_squared, 3), "\n")
cat("AIC:", round(aic_lnr, 1), "\n")
cat("C-index:", round(cindex_lnr, 3), "\n")
lower <- 0.667 - 0.007 * 1.96
upper <- 0.667 + 0.007 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# LODDS
ll_lodds <- logLik(fit_lodds)[1]
r_squared <- 1 - exp((2 * (ll_null - ll_lodds)) / N)
aic_lodds    <- AIC(fit_lodds)
cindex_lodds <- concordance(fit_lodds)$concordance
# Print
cat("R²:", round(r_squared, 3), "\n")
cat("AIC:", round(aic_lodds, 1), "\n")
cat("C-index:", round(cindex_lodds, 3), "\n")
lower <- 0.680 - 0.007 * 1.96
upper <- 0.680 + 0.007 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# ROC 
library(survivalROC)
library(boot)
### N stage
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
aa$cox_score <- predict(cox)
calc_auc <- function(data, indices, predict_time) {
  d <- data[indices, ]  
  roc <- survivalROC(
    Stime = d$time,
    status = d$css,
    marker = d$cox_score,
    predict.time = predict_time,
    method = "KM"
  )
  return(roc$AUC)
}
# 
set.seed(123)

# 3-year AUC
# Bootstrap（R=1000）
boot_results <- boot(
  data = aa,
  statistic = calc_auc,
  R = 1000,
  predict_time = 36  
)
#  95% CI
boot_ci <- boot.ci(boot_results, type = "perc")
# AUC AND 95% CI
cat("AUC at 3-year:", round(boot_results$t0, 3), "\n")
cat("95% CI:", round(boot_ci$percent[4:5], 3), "\n")


### LNR
cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
aa$cox_score <- predict(cox)
calc_auc <- function(data, indices, predict_time) {
  d <- data[indices, ]  
  roc <- survivalROC(
    Stime = d$time,
    status = d$css,
    marker = d$cox_score,
    predict.time = predict_time,
    method = "KM"
  )
  return(roc$AUC)
}
# 
set.seed(123)

# 3-year AUC
library(survivalROC)
library(boot)
# Bootstrap（R=1000 ）
boot_results <- boot(
  data = aa,
  statistic = calc_auc,
  R = 1000,
  predict_time = 36  
)
#  95% CI
boot_ci <- boot.ci(boot_results, type = "perc")
#  AUC AND 95% CI
cat("AUC at 3-year:", round(boot_results$t0, 3), "\n")
cat("95% CI:", round(boot_ci$percent[4:5], 3), "\n")


### LODDS
cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
aa$cox_score <- predict(cox)
calc_auc <- function(data, indices, predict_time) {
  d <- data[indices, ]  
  roc <- survivalROC(
    Stime = d$time,
    status = d$css,
    marker = d$cox_score,
    predict.time = predict_time,
    method = "KM"
  )
  return(roc$AUC)
}
# 
set.seed(123)

# 3-year AUC
library(survivalROC)
library(boot)
# 执行 Bootstrap（R=1000 次）
boot_results <- boot(
  data = aa,
  statistic = calc_auc,
  R = 1000,
  predict_time = 36  
)
# 95% CI
boot_ci <- boot.ci(boot_results, type = "perc")
# AUC AND 95% CI
cat("AUC at 3-year:", round(boot_results$t0, 3), "\n")
cat("95% CI:", round(boot_ci$percent[4:5], 3), "\n")


# ROC curve (1-year)
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
cox_predic_score=predict(cox)
roc_cox1 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 12,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
cox_predic_score=predict(cox)
roc_cox3 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 18,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
cox_predic_score=predict(cox)
roc_cox5 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 12,  
                        method = "KM")

par(mar = c(5, 5, 2, 2))  

# 1-year ROC curve
plot(roc_cox1$FP, roc_cox1$TP, 
     type = "l", col = "orange", 
     xlim = c(0, 1), ylim = c(0, 1),
     lty = 3, lwd = 3,
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "1 year")

# 3-year ROC curve
lines(roc_cox3$FP, roc_cox3$TP, 
      type = "l", col = "#203b66", 
      lty = 4, lwd = 3)

# 5-year ROC curve
lines(roc_cox5$FP, roc_cox5$TP, 
      type = "l", col = "#c92327", 
      lty = 1, lwd = 2)

# Reference curve
abline(0, 1, lty = 2, lwd = 2, col = "black")

# Legend
legend(x = 0.5, y = 0.4,  
       legend = c(paste0("pN stage = ", round(roc_cox1$AUC, 3)),
                  paste0("LNR = ", round(roc_cox3$AUC, 3)),
                  paste0("LODDS = ", round(roc_cox5$AUC, 3))),
       col = c("orange", "#203b66","#c92327"), 
       lty = c(3,4,1), 
       lwd = c(3,3,2),
       bty = "n", 
       cex = 0.8) 


# ROC curve (3-year)
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
cox_predic_score=predict(cox)
roc_cox1 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 36,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
cox_predic_score=predict(cox)
roc_cox3 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 36,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
cox_predic_score=predict(cox)
roc_cox5 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 36,  
                        method = "KM")

# Image
par(mar = c(5, 5, 2, 2))  

# 1-year ROC curve
plot(roc_cox1$FP, roc_cox1$TP, 
     type = "l", col = "orange", 
     xlim = c(0, 1), ylim = c(0, 1),
     lty = 3, lwd = 3,
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "3 year")

# 3-year ROC curve
lines(roc_cox3$FP, roc_cox3$TP, 
      type = "l", col = "#203b66", 
      lty = 4, lwd = 3)

# 5-year ROC curve
lines(roc_cox5$FP, roc_cox5$TP, 
      type = "l", col = "#c92327", 
      lty = 1, lwd = 2)

# Reference curve
abline(0, 1, lty = 2, lwd = 2, col = "black")

# Legend
legend(x = 0.5, y = 0.4,  
       legend = c(paste0("pN stage = ", round(roc_cox1$AUC, 3)),
                  paste0("LNR = ", round(roc_cox3$AUC, 3)),
                  paste0("LODDS = ", round(roc_cox5$AUC, 3))),
       col = c("orange", "#203b66","#c92327"), 
       lty = c(3,4,1), 
       lwd = c(3,3,2),
       bty = "n",  
       cex = 0.8)  


# ROC curve (5-year)
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
cox_predic_score=predict(cox)
roc_cox1 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 60,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
cox_predic_score=predict(cox)
roc_cox3 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 60,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
cox_predic_score=predict(cox)
roc_cox5 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 60,  
                        method = "KM")

# Image
par(mar = c(5, 5, 2, 2))  

# 1-year ROC curve
plot(roc_cox1$FP, roc_cox1$TP, 
     type = "l", col = "orange", 
     xlim = c(0, 1), ylim = c(0, 1),
     lty = 3, lwd = 3,
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "5 year")

# 3-year ROC curve
lines(roc_cox3$FP, roc_cox3$TP, 
      type = "l", col = "#203b66", 
      lty = 4, lwd = 3)

# 5-year ROC curve
lines(roc_cox5$FP, roc_cox5$TP, 
      type = "l", col = "#c92327", 
      lty = 1, lwd = 2)

# Reference 
abline(0, 1, lty = 2, lwd = 2, col = "black")

# Legend
legend(x = 0.5, y = 0.4,  
       legend = c(paste0("pN stage = ", round(roc_cox1$AUC, 3)),
                  paste0("LNR = ", round(roc_cox3$AUC, 3)),
                  paste0("LODDS = ", round(roc_cox5$AUC, 3))),
       col = c("orange", "#203b66","#c92327"), 
       lty = c(3,4,1), 
       lwd = c(3,3,2),
       bty = "n",  
       cex = 0.8) 
# 5*5 inch


#  ROC curve (Validation)
library(survival)
library(survminer)
aa<-read.csv("validation.csv")
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))

### Construct the model
fit_n <- coxph(Surv(time, css=="1") ~aa$n, data = aa)
fit_lnr <- coxph(Surv(time, css=="1") ~aa$lnr, data = aa)
fit_lodds <- coxph(Surv(time, css=="1") ~aa$lodds, data = aa)

# (R²)
null_fit <- coxph(Surv(time, css=="1") ~ 1, data = aa)
ll_null <- logLik(null_fit)[1]
N        <- nrow(aa)

# N stage
ll_n <- logLik(fit_n)[1]
r_squared <- 1 - exp((2 * (ll_null - ll_n)) / N)
aic_n    <- AIC(fit_n)
cindex_n <- concordance(fit_n)$concordance
# Print
cat("R²:", round(r_squared, 3), "\n")
cat("AIC:", round(aic_n, 1), "\n")
cat("C-index:", round(cindex_n, 3), "\n")
lower <- 0.658 - 0.016 * 1.96
upper <- 0.658 + 0.016 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# LNR
ll_lnr <- logLik(fit_lnr)[1]
r_squared <- 1 - exp((2 * (ll_null - ll_lnr)) / N)
aic_lnr    <- AIC(fit_lnr)
cindex_lnr <- concordance(fit_lnr)$concordance
# Print
cat("R²:", round(r_squared, 3), "\n")
cat("AIC:", round(aic_lnr, 1), "\n")
cat("C-index:", round(cindex_lnr, 3), "\n")
lower <- 0.674 - 0.017 * 1.96
upper <- 0.674 + 0.017 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# LODDS
ll_lodds <- logLik(fit_lodds)[1]
r_squared <- 1 - exp((2 * (ll_null - ll_lodds)) / N)
aic_lodds    <- AIC(fit_lodds)
cindex_lodds <- concordance(fit_lodds)$concordance
# Print
cat("R²:", round(r_squared, 3), "\n")
cat("AIC:", round(aic_lodds, 1), "\n")
cat("C-index:", round(cindex_lodds, 3), "\n")
lower <- 0.696 - 0.017 * 1.96
upper <- 0.696 + 0.017 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# ROC 
library(survivalROC)
library(boot)
### N stage
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
aa$cox_score <- predict(cox)
calc_auc <- function(data, indices, predict_time) {
  d <- data[indices, ]  
  roc <- survivalROC(
    Stime = d$time,
    status = d$css,
    marker = d$cox_score,
    predict.time = predict_time,
    method = "KM"
  )
  return(roc$AUC)
}
set.seed(123)

# 3-year AUC
# Bootstrap（R=1000）
boot_results <- boot(
  data = aa,
  statistic = calc_auc,
  R = 1000,
  predict_time = 36  
)
# 95% CI
boot_ci <- boot.ci(boot_results, type = "perc")
# AUC AND 95% CI
cat("AUC at 3-year:", round(boot_results$t0, 3), "\n")
cat("95% CI:", round(boot_ci$percent[4:5], 3), "\n")


### LNR
cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
aa$cox_score <- predict(cox)
calc_auc <- function(data, indices, predict_time) {
  d <- data[indices, ]  
  roc <- survivalROC(
    Stime = d$time,
    status = d$css,
    marker = d$cox_score,
    predict.time = predict_time,
    method = "KM"
  )
  return(roc$AUC)
}
set.seed(123)

# 3-year AUC
library(survivalROC)
library(boot)
# Bootstrap（R=1000 ）
boot_results <- boot(
  data = aa,
  statistic = calc_auc,
  R = 1000,
  predict_time = 36  
)
#  95% CI
boot_ci <- boot.ci(boot_results, type = "perc")
# AUC AND 95% CI
cat("AUC at 3-year:", round(boot_results$t0, 3), "\n")
cat("95% CI:", round(boot_ci$percent[4:5], 3), "\n")


### LODDS
cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
aa$cox_score <- predict(cox)
calc_auc <- function(data, indices, predict_time) {
  d <- data[indices, ]  
  roc <- survivalROC(
    Stime = d$time,
    status = d$css,
    marker = d$cox_score,
    predict.time = predict_time,
    method = "KM"
  )
  return(roc$AUC)
}
set.seed(123)

# 3-year AUC
library(survivalROC)
library(boot)
# Bootstrap（R=1000 ）
boot_results <- boot(
  data = aa,
  statistic = calc_auc,
  R = 1000,
  predict_time = 36  
)
#  95% CI
boot_ci <- boot.ci(boot_results, type = "perc")
# AUC AND 95% CI
cat("AUC at 3-year:", round(boot_results$t0, 3), "\n")
cat("95% CI:", round(boot_ci$percent[4:5], 3), "\n")


# ROC curve (1-year)
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
cox_predic_score=predict(cox)
roc_cox1 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 12,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
cox_predic_score=predict(cox)
roc_cox3 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 18,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
cox_predic_score=predict(cox)
roc_cox5 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 12,  
                        method = "KM")

par(mar = c(5, 5, 2, 2))  

# 1-year ROC
plot(roc_cox1$FP, roc_cox1$TP, 
     type = "l", col = "orange", 
     xlim = c(0, 1), ylim = c(0, 1),
     lty = 3, lwd = 3,
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "1 year")

# 3-year ROC）
lines(roc_cox3$FP, roc_cox3$TP, 
      type = "l", col = "#203b66", 
      lty = 4, lwd = 3)

# 5-year ROC
lines(roc_cox5$FP, roc_cox5$TP, 
      type = "l", col = "#c92327", 
      lty = 1, lwd = 2)

abline(0, 1, lty = 2, lwd = 2, col = "black")

legend(x = 0.5, y = 0.4,  
       legend = c(paste0("pN stage = ", round(roc_cox1$AUC, 3)),
                  paste0("LNR = ", round(roc_cox3$AUC, 3)),
                  paste0("LODDS = ", round(roc_cox5$AUC, 3))),
       col = c("orange", "#203b66","#c92327"), 
       lty = c(3,4,1), 
       lwd = c(3,3,2),
       bty = "n",  
       cex = 0.8)  


# ROC curve (3-year)
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
cox_predic_score=predict(cox)
roc_cox1 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 36,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
cox_predic_score=predict(cox)
roc_cox3 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 36,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
cox_predic_score=predict(cox)
roc_cox5 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 36,  
                        method = "KM")

par(mar = c(5, 5, 2, 2))  

# 1-year ROC
plot(roc_cox1$FP, roc_cox1$TP, 
     type = "l", col = "orange", 
     xlim = c(0, 1), ylim = c(0, 1),
     lty = 3, lwd = 3,
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "3 year")

# 3-year ROC
lines(roc_cox3$FP, roc_cox3$TP, 
      type = "l", col = "#203b66", 
      lty = 4, lwd = 3)

# 5-year ROC
lines(roc_cox5$FP, roc_cox5$TP, 
      type = "l", col = "#c92327", 
      lty = 1, lwd = 2)

abline(0, 1, lty = 2, lwd = 2, col = "black")

legend(x = 0.5, y = 0.4,  
       legend = c(paste0("pN stage = ", round(roc_cox1$AUC, 3)),
                  paste0("LNR = ", round(roc_cox3$AUC, 3)),
                  paste0("LODDS = ", round(roc_cox5$AUC, 3))),
       col = c("orange", "#203b66","#c92327"), 
       lty = c(3,4,1), 
       lwd = c(3,3,2),
       bty = "n",  # 无边框
       cex = 0.8)  # 字体大小


# ROC curve (5-year)
cox<- coxph(Surv(time,css=="1")~ n,data=aa)
cox_predic_score=predict(cox)
roc_cox1 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 60,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lnr,data=aa)
cox_predic_score=predict(cox)
roc_cox3 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 60,  
                        method = "KM")

cox<- coxph(Surv(time,css=="1")~ lodds,data=aa)
cox_predic_score=predict(cox)
roc_cox5 <- survivalROC(aa$time, aa$css, cox_predic_score,
                        predict.time = 60,  
                        method = "KM")

par(mar = c(5, 5, 2, 2))  

# 1-year ROC
plot(roc_cox1$FP, roc_cox1$TP, 
     type = "l", col = "orange", 
     xlim = c(0, 1), ylim = c(0, 1),
     lty = 3, lwd = 3,
     xlab = "1 - Specificity", 
     ylab = "Sensitivity",
     main = "5 year")

# 3-year ROC
lines(roc_cox3$FP, roc_cox3$TP, 
      type = "l", col = "#203b66", 
      lty = 4, lwd = 3)

# 5-year ROC
lines(roc_cox5$FP, roc_cox5$TP, 
      type = "l", col = "#c92327", 
      lty = 1, lwd = 2)

abline(0, 1, lty = 2, lwd = 2, col = "black")

legend(x = 0.5, y = 0.4,  
       legend = c(paste0("pN stage = ", round(roc_cox1$AUC, 3)),
                  paste0("LNR = ", round(roc_cox3$AUC, 3)),
                  paste0("LODDS = ", round(roc_cox5$AUC, 3))),
       col = c("orange", "#203b66","#c92327"), 
       lty = c(3,4,1), 
       lwd = c(3,3,2),
       bty = "n",  
       cex = 0.8)  
# 5*5 inch


library(survival)
library(survminer)
aa<-read.csv("validation.csv")
aa$n<-factor(aa$n,levels = c(1,2,3,4),
                 labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
                 labels = c("1","2","3","4","5"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))


# N classification
fit=survfit(Surv(time,css)~ n,data=aa)
fit 
summary(fit)
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p


# LNR classification
fit=survfit(Surv(time,css)~ lnr,data=aa)
fit 
summary(fit)
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p


# LODDS classification
fit=survfit(Surv(time,css)~ lodds,data=aa)
fit 
summary(fit)
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p


library(survival)
library(survminer)
aa<-read.csv("SEER.csv")
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("1","2","3","4","5"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))

# LODDS I - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "I")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS II - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "II")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS III - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "III")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS IV - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "IV")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LODDS V - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "V")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS VI - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "VI")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LODDS I - LNR
table(aa$lodds)
dta <- subset(aa, lodds == "I")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS II - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "II")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS III - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "III")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS IV - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "IV")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LODDS V - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "V")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS VI - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "VI")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N0 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N0")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N1 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N1")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N2 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N2")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N3 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N3")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 1 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "1")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 2 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "2")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 3 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "3")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LNR 4 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "4")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 5 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "5")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


library(survival)
library(survminer)
aa<-read.csv("validation.csv")
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("1","2","3","4","5"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))

# LODDS I - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "I")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS II - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "II")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS III - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "III")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS IV - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "IV")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LODDS V - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "V")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS VI - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "VI")
table(dta$n)
fit=survfit(Surv(time,css==1)~n,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LODDS I - LNR
table(aa$lodds)
dta <- subset(aa, lodds == "I")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS II - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "II")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS III - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "III")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS IV - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "IV")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LODDS V - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "V")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LODDS VI - pN stage
table(aa$lodds)
dta <- subset(aa, lodds == "VI")
table(dta$lnr)
fit=survfit(Surv(time,css==1)~lnr,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N0 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N0")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N1 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N1")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N2 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N2")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# N3 stage - LODDS
table(aa$n)
dta <- subset(aa, n == "N3")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 1 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "1")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 2 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "2")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 3 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "3")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# LNR 4 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "4")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)

# LNR 5 - LODDS
table(aa$lnr)
dta <- subset(aa, lnr == "5")
table(dta$lodds)
fit=survfit(Surv(time,css==1)~lodds,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
ggsurvplot(fit,
           pval = TRUE,
           risk.table = F,
           conf.int = F)


# Comparison GRSS and CRIS (SEER database)
library(networkD3)
nodes <- data.frame(
  name = c("N0 ","N1","N2","N3",
           "LODDS I", "LODDS II", "LODDS III","LODDS IV", "LODDS V", "LODDS VI"),
  group = c("A","B","C","D",
            "E","F","G","H","I","J")
)

links <- data.frame(
  source = c(0,0,0, 1,1,1,1,1,1, 2,2,2,2,2,2, 3,3,3,3,3,3),
  target = c(4,5,6, 4,5,6,7,8,9, 4,5,6,7,8,9, 4,5,6,7,8,9),
  value = c(1653,287,151, 101,206,271,142,69,48, 41,133,290,345,276,312, 3,9,13,8,10,12)
)

node_color <- JS('d3.scaleOrdinal() 
                  .domain(["A","B","C","D","E","F","G","H","I","J"]) 
                  .range(["#D4C866","#6A5ACD","#B76EBD","#9CCC65",
                         "#203b66", "#C92327","#006837",
                         "#D95F02","#A32A31","#F2B749"])')


# Create an interactive Sankey diagram
p <- sankeyNetwork(Links = links, 
                   Nodes = nodes,
                   Source = "source", 
                   Target = "target",
                   Value = "value", 
                   NodeID = "name",
                   NodeGroup="group",
                   fontSize = 0, 
                   sinksRight=FALSE,
                   iterations = 32,
                   nodeWidth = 60,
                   colourScale = node_color,
                   nodePadding=30
)  
p

### Save PDF

library(webshot)

saveNetwork(p,"sankey.html")

webshot("sankey.html" , 
        "sankk.pdf"
)


# Bar plot
library(ggplot2)
df <- data.frame(
  Stage = rep(c("N0","N1","N2","N3"), each = 6),
  LODDS = rep(c("LODDS I","LODDS II","LODDS III",
                "LODDS IV","LODDS V","LODDS VI"), 4),
    Value = c(
    1653,287,151,0,0,0,
    101,206,277,142,69,48,     
    41,133,290,345,276,312,      
    3,9,13,8,10,12               
  )
)

# 
df$Stage <- factor(df$Stage, levels = c("N0","N1","N2","N3"))
df$LODDS <- factor(df$LODDS, levels = c("LODDS I","LODDS II","LODDS III","LODDS IV","LODDS V","LODDS VI"))
# Plot
ggplot(df, aes(x = Stage, y = Value, fill = LODDS)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c(
    "#203b66", "#C92327", "#006837",
    "#D95F02", "#A32A31", "#F2B749"
  )) +
  labs(
    x = "",
    y = "",
    fill = "LODDS Classification",
    title = "SEER database"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),
    legend.position = c(0.98, 0.95),
    legend.justification = c(1, 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



# Comparison GRSS and CRIS (Validation cohort)
library(networkD3)
nodes <- data.frame(
  name = c("N0 ","N1","N2","N3",
           "LODDS I", "LODDS II", "LODDS III","LODDS IV", "LODDS V", "LODDS VI"),
  group = c("A","B","C","D",
            "E","F","G","H","I","J")
)

links <- data.frame(
  source = c(0,0,0, 1,1,1,1,1,1, 2,2,2,2,2,2, 3,3,3,3,3),
  target = c(4,5,6, 4,5,6,7,8,9, 4,5,6,7,8,9, 5,6,7,8,9),
  value = c(371,64,30, 7,27,28,21,11,6, 4,17,28,49,43,31, 6,9,5,6,11)
)

node_color <- JS('d3.scaleOrdinal() 
                  .domain(["A","B","C","D","E","F","G","H","I","J"]) 
                  .range(["#D4C866","#6A5ACD","#B76EBD","#9CCC65",
                         "#203b66", "#C92327","#006837",
                         "#D95F02","#A32A31","#F2B749"])')


# Create an interactive Sankey diagram
p <- sankeyNetwork(Links = links, 
                   Nodes = nodes,
                   Source = "source", 
                   Target = "target",
                   Value = "value", 
                   NodeID = "name",
                   NodeGroup="group",
                   fontSize = 0, 
                   sinksRight=FALSE,
                   iterations = 32,
                   nodeWidth = 60,
                   colourScale = node_color,
                   nodePadding=30
)  
p

### Save PDF

library(webshot)

saveNetwork(p,"sankey.html")

webshot("sankey.html" , 
        "sankey-validation.pdf"
)
getwd()

# Bar plot
library(ggplot2)
df <- data.frame(
  Stage = rep(c("N0","N1","N2","N3"), each = 6),
  LODDS = rep(c("LODDS I","LODDS II","LODDS III",
                "LODDS IV","LODDS V","LODDS VI"), 4),
  Value = c(
    371,64,30,0,0,0,
    7,27,28,21,11,6,     
    4,17,28,49,43,31,      
    0,6,9,5,6,11               
  )
)

# 
df$Stage <- factor(df$Stage, levels = c("N0","N1","N2","N3"))
df$LODDS <- factor(df$LODDS, levels = c("LODDS I","LODDS II","LODDS III","LODDS IV","LODDS V","LODDS VI"))
# Plot
ggplot(df, aes(x = Stage, y = Value, fill = LODDS)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c(
    "#203b66", "#C92327", "#006837",
    "#D95F02", "#A32A31", "#F2B749"
  )) +
  labs(
    x = "",
    y = "",
    fill = "LODDS Classification",
    title = "International multicenter cohort"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),
    legend.position = c(0.98, 0.95),
    legend.justification = c(1, 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# 6*8 inch


# Create a scatter plot
aa<-read.csv("SEER.csv")

# Calculate the correlation
cor_test <- cor.test(aa$LNR, aa$LODDS, method = "spearman", use = "complete.obs")
cor_test

# LODDS and LNR
plot(aa$LNR, 
     aa$LODDS,
     xlab = "LNR",
     ylab = "LODDS",
     main = "",
     pch = 19,       
     col = "#2E86AB",  
     cex = 1.0,      
     bty = "n",      
     axes = FALSE)   
axis(1, lwd = 1.5)   
axis(2, lwd = 1.5)   
box(lwd = 1.5, bty = "l")

# Calculate the correlation
cor_test <- cor.test(aa$pln, aa$LODDS, method = "spearman", use = "complete.obs")
cor_test

# LODDS and PLN
plot(aa$pln, 
     aa$LODDS,
     xlab = "pln",
     ylab = "LODDS",
     main = "",
     pch = 19,       
     col = "#2E86AB",  
     cex = 1.0,      
     bty = "n",      
     axes = FALSE)   
axis(1, lwd = 1.5)   
axis(2, lwd = 1.5)   
box(lwd = 1.5, bty = "l")

# Calculate the correlation
cor_test <- cor.test(aa$pln, aa$LNR, use = "complete.obs")
cor_test

# LNR and PLN
plot(aa$pln, 
     aa$LNR,
     xlab = "pln",
     ylab = "LNR",
     main = "",
     pch = 19,       
     col = "#2E86AB",  
     cex = 1.0,      
     bty = "n",      
     axes = FALSE)   
axis(1, lwd = 1.5)   
axis(2, lwd = 1.5)   
box(lwd = 1.5, bty = "l")


# LODDS-classification
g1 <- aa$LODDS <= -3.5                          
g2 <- aa$LODDS > -3.5 & aa$LODDS <= -3.0        
g3 <- aa$LODDS > -3.0 & aa$LODDS <= -2.5        
g4 <- aa$LODDS > -2.5 & aa$LODDS <= -2.0        
g5 <- aa$LODDS > -2.0 & aa$LODDS <= -1.5       
g6 <- aa$LODDS > -1.5                           

# 
x <- numeric(nrow(aa))
x[g1] <- seq(0, 1, length.out = sum(g1))
x[g2] <- seq(1, 2, length.out = sum(g2))
x[g3] <- seq(2, 3, length.out = sum(g3))
x[g4] <- seq(3, 4, length.out = sum(g4))
x[g5] <- seq(4, 5, length.out = sum(g5))
x[g6] <- seq(5, 6, length.out = sum(g6))

# Rank
y <- aa$LODDS
y[g1] <- sort(y[g1])
y[g2] <- sort(y[g2])
y[g3] <- sort(y[g3])
y[g4] <- sort(y[g4])
y[g5] <- sort(y[g5])
y[g6] <- sort(y[g6])

# Color
col_vec <- rep("#000000", nrow(aa))
col_vec[g1] <- "#D4556E"    
col_vec[g2] <- "#38A169"    
col_vec[g3] <- "#2E75B6"    
col_vec[g4] <- "#DD6B20"    
col_vec[g5] <- "#805AD5"   
col_vec[g6] <- "#D72828"    

# Plot
plot(x, y,
     ylab = "LODDS",
     xlab = "",
     main = "",
     pch = 19,
     col = col_vec,
     cex = 1.0,
     bty = "n",
     axes = FALSE)

# Remove X and Y
axis(1, lwd = 1.5, at = 0:6, labels = 0:6) 
axis(2, lwd = 1.5)
box(lwd = 1.5, bty = "l")
abline(v = c(1,2,3,4,5), lty = 2, lwd = 2, col = "black")


# LODDS-survival 
g1 <- aa$LODDS <= -3.5                          
g2 <- aa$LODDS > -3.5 & aa$LODDS <= -3.0        
g3 <- aa$LODDS > -3.0 & aa$LODDS <= -2.5        
g4 <- aa$LODDS > -2.5 & aa$LODDS <= -2.0        
g5 <- aa$LODDS > -2.0 & aa$LODDS <= -1.5       
g6 <- aa$LODDS > -1.5                           

x <- numeric(nrow(aa))
x[g1] <- seq(0, 1, length.out = sum(g1))
x[g2] <- seq(1, 2, length.out = sum(g2))
x[g3] <- seq(2, 3, length.out = sum(g3))
x[g4] <- seq(3, 4, length.out = sum(g4))
x[g5] <- seq(4, 5, length.out = sum(g5))
x[g6] <- seq(5, 6, length.out = sum(g6))

y <- aa$time  

# Color
col_vec <- rep("#000000", nrow(aa))
col_vec[g1] <- "#D4556E"    
col_vec[g2] <- "#38A169"    
col_vec[g3] <- "#2E75B6"    
col_vec[g4] <- "#DD6B20"    
col_vec[g5] <- "#805AD5"   
col_vec[g6] <- "#D72828"    

pch_vec <- ifelse(aa$css == 0, 19, 17)

# Plot
plot(x, y,
     ylab = "Time (moths)",
     xlab = "",
     main = "",
     pch = pch_vec,    
     col = col_vec,    
     cex = 0.6,
     bty = "n",
     axes = FALSE,
     ylim = c(0, 160))

# Remove the x-axis and y-axis
axis(1, lwd = 1.5, at = 0:6, labels = 0:6) 
axis(2, lwd = 1.5, at = c(0,20,40,60,80,100,120,140,160)) 
box(lwd = 1.5, bty = "l")
abline(v = c(1,2,3,4,5), lty = 2, lwd = 2, col = "black")


# LNR-classification
g1 <- aa$LNR == 0                         
g2 <- aa$LNR > 0 & aa$LNR <= 0.05         
g3 <- aa$LNR > 0.05 & aa$LNR <= 0.10      
g4 <- aa$LNR > 0.10 & aa$LNR <= 0.20      
g5 <- aa$LNR > 0.20                       

# X-axis
x <- numeric(nrow(aa))
x[g1] <- seq(0,1, length.out=sum(g1))
x[g2] <- seq(1,2, length.out=sum(g2))
x[g3] <- seq(2,3, length.out=sum(g3))
x[g4] <- seq(3,4, length.out=sum(g4))
x[g5] <- seq(4,5, length.out=sum(g5))

y <- aa$LNR
y[g1] <- sort(y[g1])
y[g2] <- sort(y[g2])
y[g3] <- sort(y[g3])
y[g4] <- sort(y[g4])
y[g5] <- sort(y[g5])


col_vec <- rep("#000000", nrow(aa))
col_vec[g1] <- "#D4556E"    
col_vec[g2] <- "#38A169"    
col_vec[g3] <- "#2E75B6"    
col_vec[g4] <- "#DD6B20"    
col_vec[g5] <- "#D72828"


plot(x, y,
     ylab = "LNR",
     main = "",
     pch = 19,
     col = col_vec,    
     cex = 1.0,
     bty = "n",
     axes = FALSE)

axis(1, lwd=1.5, at=0:5)
axis(2, lwd=1.5)
box(lwd=1.5, bty="l")

# Divider lines for five groups
abline(v=c(1,2,3,4), lty=2, lwd=2, col="black")


# LNR-survival
g1 <- aa$LNR == 0                         
g2 <- aa$LNR > 0 & aa$LNR <= 0.05         
g3 <- aa$LNR > 0.05 & aa$LNR <= 0.10      
g4 <- aa$LNR > 0.10 & aa$LNR <= 0.20      
g5 <- aa$LNR > 0.20                       

# X-axis location
x <- numeric(nrow(aa))
x[g1] <- seq(0,1, length.out=sum(g1))
x[g2] <- seq(1,2, length.out=sum(g2))
x[g3] <- seq(2,3, length.out=sum(g3))
x[g4] <- seq(3,4, length.out=sum(g4))
x[g5] <- seq(4,5, length.out=sum(g5))

y <- aa$time  

# Color
col_vec <- rep("#000000", nrow(aa))
col_vec[g1] <- "#D4556E"    
col_vec[g2] <- "#38A169"    
col_vec[g3] <- "#2E75B6"    
col_vec[g4] <- "#DD6B20"    
col_vec[g5] <- "#D72828"    

pch_vec <- ifelse(aa$css == 0, 19, 17)

# Plot
plot(x, y,
     ylab = "Time (moths)",
     xlab = "",
     main = "",
     pch = pch_vec,    
     col = col_vec,    
     cex = 0.6,
     bty = "n",
     axes = FALSE,
     ylim = c(0, 160))

# Remove the x-axis and y-axis
axis(1, lwd = 1.5, at = 0:6, labels = 0:6) 
axis(2, lwd = 1.5, at = c(0,20,40,60,80,100,120,140,160)) 
box(lwd = 1.5, bty = "l")
abline(v = c(1,2,3,4,5), lty = 2, lwd = 2, col = "black")


library(survival)
library(survminer)
aa<-read.csv("SEER.csv")
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
# Stage
fit=survfit(Surv(time,css)~ stage,data=aa)
fit 
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = T,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid",  
                legend.title="",
                legend.labs=c("N0","N1"
                              ,"N2","N3","LNR5"),
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(),  
  axis.ticks = element_blank(),   
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")  
p


library(survival)
library(survminer)
library(rms)
library(DynNom)
aa<-read.csv("train.csv")
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
summary(aa)
### Univariable analysis
cox=coxph(Surv(time, css=="1") ~aa$age, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$sex, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$race, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$grade, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$t, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$eln, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$pln, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$n, data = aa)
summary(cox)
lower <- 0.632 - 0.008 * 1.96
upper <- 0.632 + 0.008 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


cox=coxph(Surv(time, css=="1") ~aa$lnr, data = aa)
summary(cox)
lower <- 0.669 - 0.009 * 1.96
upper <- 0.669 + 0.009 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


cox=coxph(Surv(time, css=="1") ~aa$lodds, data = aa)
summary(cox)

lower <- 0.683 - 0.009 * 1.96
upper <- 0.683 + 0.009 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


### Multivariable analysis

# TNM stage 
cox<- coxph(Surv(time,css =="1")
            ~ t + n 
            ,data=aa)
summary(cox)
lower <- 0.696 - 0.008 * 1.96
upper <- 0.696 + 0.008 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# LODDS

cox<- coxph(Surv(time,css =="1")
            ~ age + race +grade + t + pln + lodds
            ,data=aa)
summary(cox)
lower <- 0.741 - 0.008 * 1.96
upper <- 0.741 + 0.008 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")

# VIF value
rm(list=ls())
aa<- read.csv('train.csv')
library(rms)
library(car)
cox <- coxph(Surv(time, css == "1") ~ age + race + grade 
             + t + pln + lodds, 
             data = aa)
summary(cox)
# VIF value
vif_values <- vif(cox)
print(vif_values)

nomo<-datadist(aa)
options(datadist='nomo')

# Construct the multivariable regression model
nomo1 <- cph(Surv(time,css==1)~ age + race +grade + t + pln + lodds ,
             x=T,y=T,
             data=aa,
             surv=T,);
nomo1
# C-index------------------------------------------------------------
Cindex <- rcorrcens(Surv(as.numeric(aa$time),aa$css==1)~predict(nomo1))
Cindex 
# Time
#1-,3-,5-year
surv <- Survival(nomo1)                         
surv1 <- function(x)surv(12*1,lp=x)
surv2 <- function(x)surv(12*3,lp=x)
surv3 <- function(x)surv(12*5,lp=x)

# Plot----------------------------------------------------------------------
nomogram
nomo2<-nomogram(nomo1,
                fun=list(surv1,surv2,surv3),
                funlabel=c('1-year survival',
                           '3-year survival',
                           '5-year survival'),
                nint = 10,
                lp = F,
                conf.int = F,
                maxscale=100,
                fun.at = c(0.1,seq(0.1,0.9,by=0.1),0.9))
# Optimize---------------------------------------------------------------------
par(mar = c(1, 1, 1, 1),  
    xpd = TRUE)           
plot(
  nomo2,
  xfrac = 0.35,        
  cex.var = 1.0,      
  cex.axis = 0.8,    
  tcl = -0.5,          
  lmgp = 0.5,          
  label.every = 1,     
  naxes = 11,          
  points.label = 'Points', 
  total.points.label = 'Total Points',  
  force.label = TRUE,  
  col.grid = c("#04579B", "#F9A029"),  
  col.axis = "black", 
  col.vars = "black"   
)                 
nomo2
# 8*10 

### Dynomic nomogram
fit<- coxph(Surv(time, css == "1") ~ age + race + grade 
             + t + pln + lodds, 
             data = aa)
summary(fit)

DynNom(fit)

library(rsconnect)
rsconnect::setAccountInfo(name='zhurunqiu',
                          token='B99676DEEC43180AEB39624E425E3BBA',
                          secret='S/mCmAKGkxQaCQFlGhlqcThHBBHfVTO0iqRXvLKX')

DNbuilder(fit)


library(survival)
library(survminer)
library(rms)
aa<-read.csv("test.csv")
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
summary(aa)
cox=coxph(Surv(time, css=="1") ~aa$age, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$sex, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$race, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$grade, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$t, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$eln, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$pln, data = aa)
summary(cox)

cox=coxph(Surv(time, css=="1") ~aa$n, data = aa)
summary(cox)
lower <- 0.628 - 0.013 * 1.96
upper <- 0.628 + 0.013 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


cox=coxph(Surv(time, css=="1") ~aa$lnr, data = aa)
summary(cox)
lower <- 0.663 - 0.014 * 1.96
upper <- 0.663 + 0.014 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


cox=coxph(Surv(time, css=="1") ~aa$lodds, data = aa)
summary(cox)

lower <- 0.674 - 0.014 * 1.96
upper <- 0.674 + 0.014 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")

# TNM stage 
cox<- coxph(Surv(time,css =="1")
            ~ t + n 
            ,data=aa)
summary(cox)
lower <- 0.681 - 0.014 * 1.96
upper <- 0.681 + 0.014 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# Nomogram
cox<- coxph(Surv(time,css =="1")
            ~ age + race +grade + t + pln + lodds
            ,data=aa)
summary(cox)
lower <- 0.718 - 0.013 * 1.96
upper <- 0.718 + 0.013 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


library(survival)
library(survminer)
library(rms)
aa<-read.csv("validation.csv")
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))

cox=coxph(Surv(time, css=="1") ~aa$n, data = aa)
summary(cox)
lower <- 0.658 - 0.016 * 1.96
upper <- 0.658 + 0.016 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


cox=coxph(Surv(time, css=="1") ~aa$lnr, data = aa)
summary(cox)
lower <- 0.674 - 0.017 * 1.96
upper <- 0.674 + 0.017 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


cox=coxph(Surv(time, css=="1") ~aa$lodds, data = aa)
summary(cox)
lower <- 0.696 - 0.017 * 1.96
upper <- 0.696 + 0.017 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")

# TNM stage 
cox<- coxph(Surv(time,css =="1")
            ~ t + n 
            ,data=aa)
summary(cox)
lower <- 0.711 - 0.017 * 1.96
upper <- 0.711 + 0.017 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# Nomogram
cox<- coxph(Surv(time,css =="1")
            ~ age + race +grade + t + pln + lodds
            ,data=aa)
summary(cox)
lower <- 0.758 - 0.016 * 1.96
upper <- 0.758 + 0.016 * 1.96
cat("95%CI：", round(lower, 3), " ~ ", round(upper, 3), "\n")


# RCS curve
library(plotRCS)
library(survival)
aa<-read.csv("SEER.csv")
aa$lodds<-as.numeric(aa$lodds) 
rcsplot(data = aa,
        outcome ="css",
        time ="time",
        k = 4,
        knots = c(0.05, 0.25, 0.50, 0.75),
        exposure ="lodds")


## Calibration curve
library(rms)
aa<-read.csv("test.csv")
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))

# Data environment
nomo<-datadist(aa)
options(datadist='nomo')
# Model
units(aa$time) <- "month"
nomo1 <- cph(Surv(time,status==1)~ age + race +grade + t + pln + lodds,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 12*1);
# 
p<- calibrate(nomo1,            
              cmethod='KM',    
              method='boot',  
              u=12*1,       
              m=400,        
              B=1000)      
plot(p,
     add=F,
     conf.int=T,           #95%CI
     subtitles = F,      
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,
     errbar.col="purple",
     xlim=c(0.0,1.0),     
     ylim=c(0.0,1.0),
     xlab="Nomogram-Predicted 6-month OS probability",
     ylab="Actual-Predicted 6-month OS probability",
     col="purple")
abline(0,1,lty=3,lwd=2,col="black")


###36-month
nomo2 <- cph(Surv(time,status==1)~age + race +grade + t + pln + lodds,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 36*1);
p<- calibrate(nomo2,            
              cmethod='KM',
              method='boot',   
              u=36*1,         
              m=400,       
              B=1000)      
#4、
plot(p,
     add=F,
     conf.int=T,          
     subtitles = F,       
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,
     errbar.col="orange",
     xlim=c(0.0,1.0),    
     ylim=c(0.0,1.0),
     xlab="Nomogram-Predicted 12-month OS probability",
     ylab="Actual-Predicted 12-month OS probability",
     col="orange")
abline(0,1,lty=3,lwd=2,col="black")

### 60-month
nomo3 <- cph(Surv(time,status==1)~age + race +grade + t + pln + lodds,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 60*1);
p<- calibrate(nomo3,            
              cmethod='KM',
              method='boot',   
              u=60*1,        
              m=400,       
              B=1000)       
#4、
plot(p,
     add=F,
     conf.int=T,           #95%CI
     subtitles = F,       
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,
     errbar.col="springgreen4",
     xlim=c(0.0,1.0),    
     ylim=c(0.0,1.0),
     xlab="Nomogram-Predicted 18-month OS probability",
     ylab="Actual-Predicted 18-month OS probability",
     col="springgreen4")
abline(0,1,lty=3,lwd=2,col="black")


p1 <- calibrate(nomo1,
                cmethod = 'KM',
                method = 'boot',
                u = 12,
                m = 400,
                B = 1000)

plot(p1,
     add = FALSE,
     conf.int = TRUE,
     subtitles = FALSE,
     lwd = 3,
     lty = 1,
     errbar.col = "#c92327",
     xlim = c(0.4, 1.0),
     ylim = c(0.4, 1.0),
     xlab = "Nomogram-predicted survival probability",
     ylab = "Actual-predicted survival probability",
     col = "#c92327",
     main = "")

p3 <- calibrate(nomo2,
                 cmethod = 'KM',
                 method = 'boot',
                 u = 36,
                 m = 400,
                 B = 1000)

plot(p3,
     add = TRUE,
     conf.int = TRUE,
     subtitles = FALSE,
     lwd = 3,
     lty = 1,
     errbar.col = "#203b66",
     col = "#203b66")


p5 <- calibrate(nomo3,
                 cmethod = 'KM',
                 method = 'boot',
                 u = 60,
                 m = 400,
                 B = 1000)

plot(p5,
     add = TRUE,
     conf.int = TRUE,
     subtitles = FALSE,
     lwd = 3,
     lty = 1,
     errbar.col = "orange",
     col = "orange")

legend(x=0.8, 
       y=0.6, 
       legend = c("1-year survival", 
                  "3-year survival", 
                  "5-year survival"),
       col = c("#c92327",
               
               "#203b66",
               
               "orange"),
       lty = 1,
       lwd = 3,
       bty = "n")

abline(0, 1, lty = 2, lwd = 2, col = "black")


##  Validaation cohort
library(rms)
aa<-read.csv("validation.csv")
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2, 3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))

nomo<-datadist(aa)
options(datadist='nomo')
# Model
units(aa$time) <- "month"
nomo1 <- cph(Surv(time, css==1)~ age + race + grade + t + pln + lodds,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 12*1);
# 3、calibration curve
p<- calibrate(nomo1,           
              cmethod='KM',    
              method='boot',  
              u=12*1,         
              m=250,      
              B=1000)     
#4、
plot(p,
     add=F,
     conf.int=T,           #95%CI
     subtitles = F,       
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,
     errbar.col="purple",
     xlim=c(0.0,1.0),     
     ylim=c(0.0,1.0),
     xlab="Nomogram-Predicted 6-month OS probability",
     ylab="Actual-Predicted 6-month OS probability",
     col="purple")
abline(0,1,lty=3,lwd=2,col="black")


###36-month
### calibration
nomo2 <- cph(Surv(time,css==1)~age + race + grade + t + pln + lodds,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 36*1);
p<- calibrate(nomo2,            
              cmethod='KM',
              method='boot',   
              u=36*1,         
              m=250,      
              B=1000)       
#4、
plot(p,
     add=F,
     conf.int=T,           #95%CI
     subtitles = F,      
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,
     errbar.col="orange",
     xlim=c(0.0,1.0),     
     ylim=c(0.0,1.0),
     xlab="Nomogram-Predicted 12-month OS probability",
     ylab="Actual-Predicted 12-month OS probability",
     col="orange")
abline(0,1,lty=3,lwd=2,col="black")

### 60-month
nomo3 <- cph(Surv(time,css==1)~age + race + grade + t + pln + lodds,
             x=T,y=T,
             data=aa,
             surv=T,
             time.inc = 60*1);
p<- calibrate(nomo3,            
              cmethod='KM',
              method='boot',   
              u=60*1,        
              m=250,        
              B=1000)       
#4、plot calibration
plot(p,
     add=F,
     conf.int=T,           #95%CI
     subtitles = F,      
     cex.subtitles=0.8, 
     lwd=2,
     lty=1,
     errbar.col="springgreen4",
     xlim=c(0.0,1.0),     
     ylim=c(0.0,1.0),
     xlab="Nomogram-Predicted 18-month OS probability",
     ylab="Actual-Predicted 18-month OS probability",
     col="springgreen4")
abline(0,1,lty=3,lwd=2,col="black")


# Combine
p1 <- calibrate(nomo1,
                cmethod = 'KM',
                method = 'boot',
                u = 12,
                m = 250,
                B = 1000)

# plot
plot(p1,
     add = FALSE,
     conf.int = TRUE,
     subtitles = FALSE,
     lwd = 3,
     lty = 1,
     errbar.col = "#c92327",
     xlim = c(0.4, 1.0),
     ylim = c(0.4, 1.0),
     xlab = "Nomogram-predicted survival probability",
     ylab = "Actual-predicted survival probability",
     col = "#c92327",
     main = "")

p3 <- calibrate(nomo2,
                cmethod = 'KM',
                method = 'boot',
                u = 36,
                m = 250,
                B = 1000)

# Overlay on the same plot
plot(p3,
     add = TRUE,
     conf.int = TRUE,
     subtitles = FALSE,
     lwd = 3,
     lty = 1,
     errbar.col = "#203b66",
     col = "#203b66")


p5 <- calibrate(nomo3,
                cmethod = 'KM',
                method = 'boot',
                u = 60,
                m = 250,
                B = 1000)

# Overlay on the same plot
plot(p5,
     add = TRUE,
     conf.int = TRUE,
     subtitles = FALSE,
     lwd = 3,
     lty = 1,
     errbar.col = "orange",
     col = "orange")

# Legend
legend(x=0.8, 
       y=0.6, 
       legend = c("1-year survival", 
                  "3-year survival", 
                  "5-year survival"),
       col = c("#c92327",
               
               "#203b66",
               
               "orange"),
       lty = 1,
       lwd = 3,
       bty = "n")

# Reference line
abline(0, 1, lty = 2, lwd = 2, col = "black")


### Time-dependent ROC (Testing)
library(timeROC)
library(survivalROC)
library(survival)
rm(list = ls())
aa<-read.csv("test.csv") 
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
# Modell
cox<- coxph(Surv(time,css=="1")~age + race + grade + t + pln + lodds,
            ,data=aa)
cox_predic_score=predict(cox)

# 1-year
roc_cox1 <- survivalROC(
  aa$time,                
  aa$css,              
  cox_predic_score,       
  predict.time = 12,       
  method = "KM"         
)

plot(
  roc_cox1$FP,                     
  roc_cox1$TP,                   
  type = "l",                    
  col = "#c92327",              
  xlim = c(0, 1),              
  ylim = c(0, 1),             
  lty = 1,                   
  lwd = 2,                 
  xlab = "1-Specificity",  
  ylab = "Sensitivity"    
)

round(roc_cox1$AUC, 3)

legend(
  "bottomright",                                  
  paste("AUC at 1-year:", 
        round(roc_cox1$AUC, 3)),                 
  col = "#c92327",                                
  cex = 0.8,                                     
  lty = 1,                                       
  lwd = 2,                                      
  bty = "n"                                      
)

abline(
  0, 1,                  
  col = "black",         
  lty = 2,               
  lwd = 1                
)


# 3-year
roc_cox2=survivalROC(aa$time,aa$css,
                     cox_predic_score,
                     predict.time =36,
                     method = "KM")
plot(roc_cox2$FP, roc_cox2$TP, 
     type="l",col="#203b66", 
     xlim=c(0,1), 
     ylim=c(0,1), 
     lty=1, lwd=2,
     xlab="1-Specifity",
     ylab="Sensitivity")
round(roc_cox2$AUC,3)
legend("bottomright",
       paste("AUC at 3-year:",round(roc_cox2$AUC,3)),
       col="#203b66",
       cex = 0.8,
       lty=1, lwd=2,
       bty = "n")
abline(0,1,col="black",lty=2,lwd=1)

# 5-year
roc_cox3=survivalROC(aa$time,aa$css,cox_predic_score,
                     predict.time =60,
                     method = "KM")
plot(roc_cox3$FP, 
     roc_cox3$TP, 
     type="l",col="orange", 
     xlim=c(0,1), ylim=c(0,1), 
     lty=1, lwd=2,
     xlab="1-Specifity",
     ylab="Sensitivity")
round(roc_cox3$AUC,3)
legend("bottomright",
       paste("AUC at 5-year:",round(roc_cox3$AUC,3)),
       col="orange",
       cex = 0.8,
       lty=1, lwd=2,
       bty = "n")
abline(0,1,col="black",lty=2,lwd=1)


roc_cox1 <- survivalROC(aa$time, 
                        aa$css, 
                        cox_predic_score,
                        predict.time = 12,
                        method = "KM")

roc_cox2 <- survivalROC(aa$time, 
                        aa$css, 
                        cox_predic_score,
                        predict.time = 36,
                        method = "KM")

roc_cox3 <- survivalROC(aa$time, 
                        aa$css, 
                        cox_predic_score,
                        predict.time = 60,
                        method = "KM")

plot(0, 0, type = "n",
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1-Specificity",
     ylab = "Sensitivity",
     main = "")

# ROC curve
lines(roc_cox1$FP, roc_cox1$TP, col = "#c92327", lty = 1, lwd = 3)
lines(roc_cox2$FP, roc_cox2$TP, col = "#203b66", lty = 1, lwd = 3)
lines(roc_cox3$FP, roc_cox3$TP, col = "orange", lty = 1, lwd = 3)

#Legend
legend(x=0.6,
       y=0.2,
       legend = c(paste("AUC at 1-year:", round(roc_cox1$AUC, 3)),
                  paste("AUC at 3-year:", round(roc_cox2$AUC, 3)),
                  paste("AUC at 5-year:", round(roc_cox3$AUC, 3))),
       col = c("#c92327",
               
               "#203b66",
               
               "orange"),
       lty = 1, 
       lwd = 3,
       cex = 0.8,
       bty = "n")

abline(0, 1, lty = 2, lwd = 2, col = "black")


### Time-dependent ROC (Validation)
library(timeROC)
library(survivalROC)
library(survival)
rm(list = ls())
aa<-read.csv("validation.csv") 
aa$age<-factor(aa$age,levels = c(1,2),
               labels = c(" < 60 years ","≥ 60 years"))
aa$sex<-factor(aa$sex,levels = c(1,2),
               labels = c(" Female ","Male"))
aa$race<-factor(aa$race,levels = c(1,2,3),
                labels = c("White","Black","Other"))
aa$grade<-factor(aa$grade,levels = c(1,2,3),
                 labels = c("G1","G2","G3"))
aa$t<-factor(aa$t,levels = c(1,2,3,4),
             labels = c("T1","T2","T3",'T4'))
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$stage<-factor(aa$stage,levels = c(1,2,3,4,5),
                 labels = c("I","II","III","IVA","IVB"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II","III","IV","V","VI"))
aa$rad<-factor(aa$rad,levels = c(1,2),
               labels = c("None","PORT"))
# Modell
cox<- coxph(Surv(time,css=="1")~age + race +grade + t + pln + lodds,
            ,data=aa)
cox_predic_score=predict(cox)

# 1-year
roc_cox1 <- survivalROC(
  aa$time,                
  aa$css,              
  cox_predic_score,       
  predict.time = 12,       
  method = "KM"         
)

#  ROC 
plot(
  roc_cox1$FP,                     
  roc_cox1$TP,                   
  type = "l",                    
  col = "#c92327",              
  xlim = c(0, 1),              
  ylim = c(0, 1),             
  lty = 1,                   
  lwd = 2,                 
  xlab = "1-Specificity",  
  ylab = "Sensitivity"    
)

round(roc_cox1$AUC, 3)

legend(
  "bottomright",                                  
  paste("AUC at 1-year:", 
        round(roc_cox1$AUC, 3)),                 
  col = "#c92327",                                
  cex = 0.8,                                     
  lty = 1,                                       
  lwd = 2,                                      
  bty = "n"                                      
)

abline(
  0, 1,                  
  col = "black",         
  lty = 2,               
  lwd = 1                
)


# 3-year
roc_cox2=survivalROC(aa$time,aa$css,
                     cox_predic_score,
                     predict.time =36,
                     method = "KM")
plot(roc_cox2$FP, roc_cox2$TP, 
     type="l",col="#203b66", 
     xlim=c(0,1), 
     ylim=c(0,1), 
     lty=1, lwd=2,
     xlab="1-Specifity",
     ylab="Sensitivity")
round(roc_cox2$AUC,3)
legend("bottomright",
       paste("AUC at 3-year:",round(roc_cox2$AUC,3)),
       col="#203b66",
       cex = 0.8,
       lty=1, lwd=2,
       bty = "n")
abline(0,1,col="black",lty=2,lwd=1)

# 5-year
roc_cox3=survivalROC(aa$time,aa$css,cox_predic_score,
                     predict.time =60,
                     method = "KM")
plot(roc_cox3$FP, 
     roc_cox3$TP, 
     type="l",col="orange", 
     xlim=c(0,1), ylim=c(0,1), 
     lty=1, lwd=2,
     xlab="1-Specifity",
     ylab="Sensitivity")
round(roc_cox3$AUC,3)
legend("bottomright",
       paste("AUC at 5-year:",round(roc_cox3$AUC,3)),
       col="orange",
       cex = 0.8,
       lty=1, lwd=2,
       bty = "n")
abline(0,1,col="black",lty=2,lwd=1)


roc_cox1 <- survivalROC(aa$time, 
                        aa$css, 
                        cox_predic_score,
                        predict.time = 12,
                        method = "KM")

roc_cox2 <- survivalROC(aa$time, 
                        aa$css, 
                        cox_predic_score,
                        predict.time = 36,
                        method = "KM")

roc_cox3 <- survivalROC(aa$time, 
                        aa$css, 
                        cox_predic_score,
                        predict.time = 60,
                        method = "KM")

plot(0, 0, type = "n",
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1-Specificity",
     ylab = "Sensitivity",
     main = "")

lines(roc_cox1$FP, roc_cox1$TP, col = "#c92327", lty = 1, lwd = 3)
lines(roc_cox2$FP, roc_cox2$TP, col = "#203b66", lty = 1, lwd = 3)
lines(roc_cox3$FP, roc_cox3$TP, col = "orange", lty = 1, lwd = 3)

legend(x=0.6,
       y=0.2,
       legend = c(paste("AUC at 1-year:", round(roc_cox1$AUC, 3)),
                  paste("AUC at 3-year:", round(roc_cox2$AUC, 3)),
                  paste("AUC at 5-year:", round(roc_cox3$AUC, 3))),
       col = c("#c92327",
               
               "#203b66",
               
               "orange"),
       lty = 1, 
       lwd = 3,
       cex = 0.8,
       bty = "n")

abline(0, 1, lty = 2, lwd = 2, col = "black")


### DCA cureve (Testing cohort)
rm(list = ls())
# Training cohort
pbc<-read.table("test.csv",header = T,sep = ",")
pbc$died <- pbc$css==1
pbc$status =ifelse(pbc$died=="TRUE",1,0)
head(pbc)
library(survival)
Srv = Surv(pbc$time, pbc$died)

## 3-year
# pN stage
coxmod1 = coxph(Srv ~ n , data=pbc)
pbc$N = c(1 - (summary(survfit(coxmod1,newdata=pbc), times=36)$surv))

# LNR
coxmod2 = coxph(Srv ~ lnr, data=pbc)
pbc$LNR= c(1 - (summary(survfit(coxmod2,newdata=pbc), times=36)$surv))

# LODDS
coxmod3 = coxph(Srv ~ lodds, data=pbc)
pbc$LODDS= c(1 - (summary(survfit(coxmod3,newdata=pbc), times=36)$surv))

# TNM
coxmod4 = coxph(Srv ~ t + n, data=pbc)
pbc$TNM= c(1 - (summary(survfit(coxmod4,newdata=pbc), times=36)$surv))


# Nomogram
coxmod5 = coxph(Srv ~ age + race +grade + t + pln + lodds , data=pbc)
pbc$Nomogram= c(1 - (summary(survfit(coxmod5,newdata=pbc), times=36)$surv))

head(pbc)
source("stdca.R")  
mod1<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="N", 
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
mod2<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="LNR",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
mod3<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="LODDS",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
mod4<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="TNM",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")

mod4<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="Nomogram",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
stdca(data=pbc, 
      outcome="css", 
      ttoutcome="time", 
      timepoint=36,
      predictors=c("N","LNR","LODDS","TNM","Nomogram"), 
      cmprsk=TRUE, 
      smooth=TRUE, 
      xstop=1.0,
      intervention="FALSE",
      interventionper = 1000)

### DCA cureve (Validation cohort)
rm(list = ls())
# Training cohort
pbc<-read.table("validation.csv",header = T,sep = ",")
pbc$died <- pbc$css==1
pbc$status =ifelse(pbc$died=="TRUE",1,0)
head(pbc)
library(survival)
Srv = Surv(pbc$time, pbc$died)

## 3-year
# pN stage
coxmod1 = coxph(Srv ~ n , data=pbc)
pbc$N = c(1 - (summary(survfit(coxmod1,newdata=pbc), times=36)$surv))

# LNR
coxmod2 = coxph(Srv ~ lnr, data=pbc)
pbc$LNR= c(1 - (summary(survfit(coxmod2,newdata=pbc), times=36)$surv))

# LODDS
coxmod3 = coxph(Srv ~ lodds, data=pbc)
pbc$LODDS= c(1 - (summary(survfit(coxmod3,newdata=pbc), times=36)$surv))

# TNM
coxmod4 = coxph(Srv ~ t + n, data=pbc)
pbc$TNM= c(1 - (summary(survfit(coxmod4,newdata=pbc), times=36)$surv))


# Nomogram
coxmod5 = coxph(Srv ~ age + race +grade + t + pln + lodds , data=pbc)
pbc$Nomogram= c(1 - (summary(survfit(coxmod5,newdata=pbc), times=36)$surv))

head(pbc)
source("stdca.R")  
mod1<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="N", 
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
mod2<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="LNR",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
mod3<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="LODDS",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
mod4<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="TNM",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")

mod4<-stdca(data=pbc, 
            outcome="css", 
            ttoutcome="time", 
            timepoint=36, 
            predictors="Nomogram",
            cmprsk=TRUE, 
            smooth=TRUE, 
            xstop=1.0,
            intervention="FALSE")
stdca(data=pbc, 
      outcome="css", 
      ttoutcome="time", 
      timepoint=36,
      predictors=c("N","LNR","LODDS","TNM","Nomogram"), 
      cmprsk=TRUE, 
      smooth=TRUE, 
      xstop=1.0,
      intervention="FALSE",
      interventionper = 1000)


# Kaplan-meire (SEER database: Three lymph node staging system)
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("SEER.csv") 
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II", "III", "IV","V","VI"))
# N stage
fit=survfit(Surv(time,css)~ n,data=aa)
fit 
summary(fit)
# 1-year
res<- summary(fit, times = 12)
surv_12 <- res$surv
round(surv_12, 3)
# 3-year
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
# 5-year
res<- summary(fit, times = 60)
surv_60 <- res$surv
round(surv_60, 3)
# Plot
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = T,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid",  
                legend.title="",
                legend.labs=c("N0","N1"
                              ,"N2","N3"),
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(),  
  axis.ticks = element_blank(),   
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")  
p


# LNR
fit=survfit(Surv(time,css)~ lnr,data=aa)
fit 
summary(fit)
# 1-year
res<- summary(fit, times = 12)
surv_12 <- res$surv
round(surv_12, 3)
# 3-year
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
# 5-year
res<- summary(fit, times = 60)
surv_60 <- res$surv
round(surv_60, 3)
# Plot
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p


# LODDS
fit=survfit(Surv(time,css)~ lodds,data=aa)
fit 
summary(fit)
# 1-year
res<- summary(fit, times = 12)
surv_12 <- res$surv
round(surv_12, 3)
# 3-year
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
# 5-year
res<- summary(fit, times = 60)
surv_60 <- res$surv
round(surv_60, 3)
# Plot
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p
# 6*7.5inch


# Kaplan-meire (Validation cohort: Three lymph node staging system)
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("validation.csv") 
aa$n<-factor(aa$n,levels = c(1,2,3,4),
             labels = c("N0","N1","N2","N3"))
aa$lnr<-factor(aa$lnr,levels = c(1,2,3,4,5),
               labels = c("I","II","III","IV","V"))
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II", "III", "IV","V","VI"))
# N stage
fit=survfit(Surv(time,css)~ n,data=aa)
fit 
summary(fit)
# 1-year
res<- summary(fit, times = 12)
surv_12 <- res$surv
round(surv_12, 3)
# 3-year
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
# 5-year
res<- summary(fit, times = 60)
surv_60 <- res$surv
round(surv_60, 3)
# Plot
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = T,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid",  
                legend.title="",
                legend.labs=c("N0","N1"
                              ,"N2","N3"),
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(),  
  axis.ticks = element_blank(),   
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")  
p


# LNR
fit=survfit(Surv(time,css)~ lnr,data=aa)
fit 
summary(fit)
# 1-year
res<- summary(fit, times = 12)
surv_12 <- res$surv
round(surv_12, 3)
# 3-year
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
# 5-year
res<- summary(fit, times = 60)
surv_60 <- res$surv
round(surv_60, 3)
# Plot
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p


# LODDS
fit=survfit(Surv(time,css)~ lodds,data=aa)
fit 
summary(fit)
# 1-year
res<- summary(fit, times = 12)
surv_12 <- res$surv
round(surv_12, 3)
# 3-year
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
# 5-year
res<- summary(fit, times = 60)
surv_60 <- res$surv
round(surv_60, 3)
# Plot
p <- ggsurvplot(fit,
                pval = TRUE,  
                conf.int = T,
                risk.table = F,                   
                risk.table.col = "strata",          
                surv.median.line = "none",         
                xlim = c(0,120),                      
                xlab = "Time (months)",              
                ylab = "",                          
                break.time.by = 24,                
                ylim = c(0,1),                      
                linetype = "solid", 
                legend = "none",
                risk.table.height = 0.3,          
                risk.table.y.text =T
) 
p
# 6*7.5inch


# SEER database
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("SEER.csv") 
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                labels = c("I","II", "III", "IV","V","VI"))
# LODDS-I
dta <- subset(aa, lodds == "I")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p


# LODDS-II
dta <- subset(aa, lodds == "II")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# LODDS-III
dta <- subset(aa, lodds == "III")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# LODDS-IV
dta <- subset(aa, lodds == "IV")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# LODDS-V
dta <- subset(aa, lodds == "V")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p


# LODDS-VI
dta <- subset(aa, lodds == "VI")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p



# PORT Subgroup (validation)
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("validation.csv") 
aa$lodds<-factor(aa$lodds,levels = c(1,2,3,4,5,6),
                 labels = c("I","II", "III", "IV","V","VI"))
# LODDS-I
dta <- subset(aa, lodds == "I")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p


# LODDS-II
dta <- subset(aa, lodds == "II")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# LODDS-III
dta <- subset(aa, lodds == "III")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# LODDS-IV
dta <- subset(aa, lodds == "IV")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# LODDS-V
dta <- subset(aa, lodds == "V")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p


# LODDS-VI
dta <- subset(aa, lodds == "VI")
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p


# SEER database
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("group.csv") 
aa$risk<-factor(aa$risk,levels = c(1,2,3),
               labels = c("Low-risk","Medium-risk", "High-risk"))
fit=survfit(Surv(time,css==1)~risk,data=aa)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#203b66',"#db893f",'#c92327'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Low-risk","Medium-risk","High-risk"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p
# 6*6 inch


# Low-risk group
table(aa$risk)
dta <- subset(aa, risk == "Low-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)   
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# Medium-risk group
table(aa$risk)
dta <- subset(aa, risk == "Medium-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p
cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# High-risk group
table(aa$risk)
dta <- subset(aa, risk == "High-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit) 
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p
cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# Training cohort
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("group.csv")
table(aa$group)
dta <- subset(aa, group == "train")
aa$risk<-factor(aa$risk,levels = c(1,2,3),
                labels = c("Low-risk","Medium-risk", "High-risk"))
fit=survfit(Surv(time,css==1)~risk,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#203b66',"#db893f",'#c92327'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-speciffic survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Low-risk","Medium-risk","High-risk"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p


# Testing group
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("group.csv") 
table(aa$group)
dta <- subset(aa, group == "test")
aa$risk<-factor(aa$risk,levels = c(1,2,3),
                labels = c("Low-risk","Medium-risk", "High-risk"))
fit=survfit(Surv(time,css==1)~risk,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#203b66',"#db893f",'#c92327'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-speciffic survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Low-risk","Medium-risk","High-risk"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# Low-risk group
table(aa$risk)
dta <- subset(aa, risk == "Low-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-speciffic survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("PORT","Non-PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# Medium-risk group
table(aa$risk)
dta <- subset(aa, risk == "Medium-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-speciffic survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("PORT","Non-PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p
cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# High-risk group
table(aa$risk)
dta <- subset(aa, risk == "High-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-speciffic survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("PORT","Non-PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p
cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# Validation cohort
library(survival)
library(survminer)
rm(list = ls())
aa<-read.csv("validation.csv") 
aa$risk<-factor(aa$risk,levels = c(1,2,3),
                labels = c("Low-risk","Medium-risk", "High-risk"))
fit=survfit(Surv(time,css==1)~risk,data=aa)
summary(fit)            
fit                     
p <- ggsurvplot(fit,
                palette= c('#203b66',"#db893f",'#c92327'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Low-risk","Medium-risk","High-risk"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

# Low-risk group
table(aa$risk)
dta <- subset(aa, risk == "Low-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)            
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
fit  
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# Medium-risk group
table(aa$risk)
dta <- subset(aa, risk == "Medium-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit) 
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("Non-PORT","PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p

cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)


# High-risk group
table(aa$risk)
dta <- subset(aa, risk == "High-risk")
table(dta$rad)
fit=survfit(Surv(time,css==1)~rad,data=dta)
summary(fit)  
res<- summary(fit, times = 36)
surv_36 <- res$surv
round(surv_36, 3)
fit                     
p <- ggsurvplot(fit,
                palette= c('#c92327','#203b66'), 
                pval = TRUE,  
                conf.int = TRUE,
                risk.table = TRUE,                    
                risk.table.col = "strata",          
                surv.median.line = "none",        
                xlim = c(0,120),                       
                xlab = "Time (months)",             
                ylab = "Cancer-specific survival",                       
                break.time.by = 24,                 
                ylim = c(0,1),                       
                linetype = "solid", 
                legend.title="",
                legend.labs=c("PORT","Non-PORT"),
                risk.table.height = 0.3,         
                risk.table.y.text =T,             
) 

p$table <- p$table + theme(
  axis.line = element_blank(),    
  axis.text.x = element_blank(), 
  axis.ticks = element_blank(),  
  axis.text.y = element_blank()   
) + 
  xlab("") +         
  ylab("") +         
  labs(title = "")   
p
cox<- coxph(Surv(time, css =="1")~ rad, data=dta)
summary(cox)
