# ==========================================
# 0. 環境設定與資料讀取
# ==========================================
setwd("C:/Users/jef81/Desktop/計算生物學/NBA")

library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)
library(randomForest)
library(rpart)
library(rpart.plot)
library(showtext)
library(sysfonts)

GameLogs    <- read.csv("NBA_GameLogs_202425.csv")
PlayerStats <- read.csv("NBA_PlayerStats_202425.csv")
TeamInfo    <- read.csv("NBA_TeamInfo_202425.csv")
TeamStats   <- read.csv("NBA_TeamStats_202425.csv")

# ==========================================
# 1. 資料整合 (只針對球隊數據，自動抓取所有變項)
# ==========================================

# --- Step 1. 整合球隊統計與基本資訊 ---
# 保留 TeamStats 內所有的數值變項
team_base <- TeamStats %>%
  left_join(TeamInfo %>% select(TEAM_ID, ABBREVIATION), by = "TEAM_ID")

# --- Step 2. 處理比賽日誌 (GameLogs) 並合併球隊數據 ---
# 我們將 GameLogs 作為基底，併入該球隊當季的整體統計數據
full_ml_data <- GameLogs %>%
  select(TEAM_ABBREVIATION, MATCHUP, WL) %>%
  mutate(
    Home = ifelse(grepl("@", MATCHUP), 0, 1),
    WL_num = ifelse(WL == "W", 1, 0)
  ) %>%
  # 根據縮寫併入所有的球隊統計欄位
  left_join(team_base, by = c("TEAM_ABBREVIATION" = "ABBREVIATION")) %>%
  # 移除不需要的文字欄位與 ID
  select(-TEAM_ID, -TEAM_NAME, -MATCHUP, -WL)

# --- Step 3. 自動篩選所有數值變項作為預測因子 ---
# 自動抓取除了目標變數 (WL_num) 以外的所有數值欄位
predictors <- full_ml_data %>%
  select(where(is.numeric)) %>%
  select(-WL_num) %>%
  names()

# 建立公式：WL_num ~ 變項1 + 變項2 + ...
formula_all <- as.formula(paste("WL_num ~", paste(predictors, collapse = " + ")))
# ==========================================
# 2. 模型訓練
# ==========================================

# --- [1] Logistic Regression ---
logit_model_all <- glm(formula_all, data = full_ml_data, family = binomial)

full_ml_data$logit_prob <- predict(logit_model_all, type = "response")
full_ml_data$logit_pred <- ifelse(full_ml_data$logit_prob > 0.5, 1, 0)

# --- [2] SVM ---
# SVM 需要將目標變數轉為 Factor，且資料不能有 NA
svm_df <- full_ml_data %>% na.omit()
svm_df$WL_num <- as.factor(svm_df$WL_num)

svm_model_all <- svm(formula_all, data = svm_df, kernel = "radial", scale = TRUE, probability = TRUE)

# 取得預測機率
svm_pred_res <- predict(svm_model_all, svm_df, probability = TRUE)
svm_df$svm_prob <- attr(svm_pred_res, "probabilities")[, "1"]

# ==========================================
# 3. 結果檢查
# ==========================================
cat("使用變項數量:", length(predictors), "\n")
cat("包含變項範例:", head(predictors, 10), "...\n")

# 查看模型準確度 (以 Logistic 為例)
acc <- mean(full_ml_data$logit_pred == full_ml_data$WL_num, na.rm = TRUE)
cat("Logistic Regression 準確度:", round(acc, 4), "\n")
# ==========================================
# 3. 隨機森林與 CART (使用全變項資料)
# ==========================================

# --- 準備資料：確保目標變數為 Factor 且處理缺失值 ---
# RF 與 CART 在做分類時，目標變數必須是 Factor
final_modeling_df <- full_ml_data %>% 
  na.omit() %>% 
  mutate(WL_num = as.factor(WL_num))

# --- [3] Random Forest ---
# 使用之前定義好的 formula_all
rf_model_all <- randomForest(
  formula_all, 
  data = final_modeling_df, 
  ntree = 500, 
  importance = TRUE, # 開啟重要性評估，這對分析很有幫助
  mtry = floor(sqrt(length(predictors)))
)

# 取得預測機率並存回資料表
rf_probs <- predict(rf_model_all, final_modeling_df, type = "prob")
final_modeling_df$rf_prob <- rf_probs[, "1"]

# --- [4] CART (決策樹) ---
cart_model_all <- rpart(
  formula_all, 
  data = final_modeling_df, 
  method = "class", 
  control = rpart.control(cp = 0.01, minsplit = 30, maxdepth = 5)
)

# 取得預測機率
final_modeling_df$cart_prob <- predict(cart_model_all, final_modeling_df, type = "prob")[, "1"]

# ==========================================
# 4. 模型結果可視化 (額外建議)
# ==========================================

# 1. 繪製決策樹圖 (看看哪些數據是關鍵門檻)
rpart.plot(cart_model_all, main = "NBA 勝負決策樹 (全變項)", type = 2, extra = 104)

# 2. 查看隨機森林最重要的變項 (Feature Importance)
varImpPlot(rf_model_all, main = "隨機森林變項重要性排行")
# ==========================================
# 3. 匯出資料 (為了 Streamlit)
# ==========================================
# 把最後計算完的 GameLogs2 存出去
write.csv(GameLogs2, "NBA_Final_Predictions.csv", row.names = FALSE)
cat("處理完成，已產生 NBA_Final_Predictions.csv\n")