library(readr)
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)
library(MLmetrics)
library(car)
library(brms)

mlc_churn <- read_csv("/home/cinnamoll/Code/BTL_TKTN/dataset/mlc_churn.csv")
str(mlc_churn)

#drop state, account_length, area_code do khong co y nghia du doan doi voi ty le roi goi mang
mlc_churn <- subset(mlc_churn, select = -c(state, account_length, area_code))

mlc_churn <- mlc_churn %>%
  mutate_if(is.character, as.factor)

print(table(mlc_churn$churn)) #imbalanced

numeric_vars <- sapply(mlc_churn, is.numeric)
cor_matrix <- cor(mlc_churn[, numeric_vars])
cor_matrix
corrplot(cor_matrix, method = "number", addCoef.col = "black", bg="gray", type = "lower", number.digits=2, number.cex=0.8, diag=FALSE)

#drop moi cot charges do gia tien duoc sinh tu so phut goi
mlc_churn <- subset(mlc_churn, select=-c(total_day_charge, total_eve_charge, total_night_charge, total_intl_charge))
str(mlc_churn)

print(sum(mlc_churn$voice_mail_plan=="no" & mlc_churn$number_vmail_messages>0)) #==0
#dang ky thu thoai -> dung thu thoai
mlc_churn <- subset(mlc_churn, select=-c(voice_mail_plan))

print(sum(mlc_churn$international_plan=="no" & mlc_churn$total_intl_calls>0)) #!=0 => khong xoa

#CRD
k_value <- c(3,5,10)
f1_res <- data.frame()

for (k in k_value) {
  fit_control <- trainControl(
    method = "repeatedcv",  
    number = k,        
    repeats = 10,            
    classProbs = TRUE,
    summaryFunction = prSummary
  )
  
  set.seed(1234)
  rf <- train(
    churn ~ ., 
    data = mlc_churn,
    method = "rf",
    metric = "F",
    trControl = fit_control
  )
  
  cv_res <- rf$resample$F
  
  temp_df <- data.frame(
    k = rep(k, length(cv_res)),
    F1_score = cv_res
  )
  
  temp_df$k <- as.factor(temp_df$k)  
  f1_res <- rbind(f1_res, temp_df)
}

str(f1_res)

# p-value << 0.5 -> k anh huong den F1
levene_result <- leveneTest(F1_score ~ k, data = f1_res)
print(levene_result)

model_lm <- lm(F1_score ~ k, data = f1_res)
summary(model_lm)
rs1 <- resid(model_lm)

#<0.05
fligner.test(f1_res$F1_score ~ f1_res$k)

#phan du phan phoi chuan
shapiro.test(rs1)

# < 0.05, so fold anh huong F1
anova_result <- anova(model_lm)
print(anova_result)

aov_lm <- aov(model_lm) 
tukey_result <- TukeyHSD(aov_lm)
print(tukey_result)
plot(tukey_result, las = 1, col = "red")

