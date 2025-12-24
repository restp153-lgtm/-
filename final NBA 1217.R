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
# 1. Data Cleaning (資料清洗)
# ==========================================

# --- Step 1. 篩選球隊統計 ---
NBA_Stats <- TeamStats[TeamStats$TEAM_ID %in% TeamInfo$TEAM_ID,]
ml_data <- subset(NBA_Stats, select = c(
  "TEAM_ID", "TEAM_NAME", "PTS", "REB", "AST", "BLK", "STL", 
  "TOV", "FG3_PCT", "FG_PCT", "OFF_RATING", "DEF_RATING", "NET_RATING"
))

# --- Step 2. 加入縮寫並處理球員資料 ---
ml_data2 <- ml_data %>%
  left_join(TeamInfo %>% select(TEAM_ID, ABBREVIATION), by = "TEAM_ID")

player_data <- PlayerStats %>%
  select(PLAYER_NAME, TEAM_ABBREVIATION, MIN_base, PLUS_MINUS, NBA_FANTASY_PTS)

# --- Step 3. 每隊選上場時間前 8 名並轉寬格式 ---
top8_players <- player_data %>%
  group_by(TEAM_ABBREVIATION) %>%
  arrange(desc(MIN_base)) %>%
  slice_head(n = 8) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top8_wide <- top8_players %>%
  select(TEAM_ABBREVIATION, rank, PLAYER_NAME, PLUS_MINUS, NBA_FANTASY_PTS) %>%
  pivot_wider(
    names_from = rank,
    values_from = c(PLAYER_NAME, PLUS_MINUS, NBA_FANTASY_PTS),
    names_glue = "player{rank}_{.value}"
  )

# 重新命名欄位
names(top8_wide) <- gsub("PLAYER_NAME", "player", names(top8_wide))
names(top8_wide) <- gsub("PLUS_MINUS", "pm", names(top8_wide))
names(top8_wide) <- gsub("NBA_FANTASY_PTS", "fantasy_pts", names(top8_wide))

# --- Step 4. 合併至比賽日誌 ---
ml_data_final <- ml_data2 %>%
  left_join(top8_wide, by = c("ABBREVIATION" = "TEAM_ABBREVIATION")) %>%
  mutate(ABBREVIATION = ABBREVIATION)

GameLogs2 <- GameLogs %>%
  select(TEAM_ABBREVIATION, MATCHUP, WL) %>%
  mutate(
    Home = ifelse(grepl("@", MATCHUP), 0, 1),
    WL_num = ifelse(WL == "W", 1, 0)
  ) %>%
  left_join(ml_data_final, by = c("TEAM_ABBREVIATION" = "ABBREVIATION"))

# ==========================================
# 2. 模型訓練與預測
# ==========================================

# --- [1] Logistic Regression ---
num_vars <- names(GameLogs2)[sapply(GameLogs2, is.numeric)]
num_vars <- setdiff(num_vars, c("WL_num", "TEAM_ID"))

formula <- as.formula(paste("WL_num ~", paste(num_vars, collapse = " + ")))
logit_model <- glm(formula, data = GameLogs2, family = binomial)

GameLogs2$pred_prob <- predict(logit_model, type = "response")
GameLogs2$pred_class <- ifelse(GameLogs2$pred_prob > 0.5, 1, 0)

# --- [2] SVM ---
num_vars_svm <- setdiff(num_vars, c("GameNo", "error", "pred_prob", "pred_class"))
svm_data <- GameLogs2 %>% select(all_of(c("WL_num", num_vars_svm)))
svm_data$WL_num <- as.factor(svm_data$WL_num)

svm_final <- svm(WL_num ~ ., data = svm_data, kernel = "radial", scale = TRUE, probability = TRUE)
svm_pred_prob <- predict(svm_final, svm_data, probability = TRUE)
GameLogs2$svm_pred_prob <- attr(svm_pred_prob, "probabilities")[, "1"]

# --- [3] Random Forest ---
rf_data <- GameLogs2 %>% select(all_of(c("WL_num", num_vars_svm)))
rf_data$WL_num <- as.factor(rf_data$WL_num)

rf_final <- randomForest(WL_num ~ ., data = rf_data, ntree = 500, mtry = floor(sqrt(ncol(rf_data) - 1)))
rf_pred_prob <- predict(rf_final, rf_data, type = "prob")
GameLogs2$rf_pred_prob <- rf_pred_prob[, "1"]

# --- [4] CART ---
cart_final <- rpart(as.factor(WL_num) ~ ., data = rf_data, method = "class", 
                    control = rpart.control(cp = 0.01, minsplit = 30, maxdepth = 5))
GameLogs2$cart_pred_prob <- predict(cart_final, rf_data, type = "prob")[, "1"]

# ==========================================
# 3. 匯出資料 (為了 Streamlit)
# ==========================================
# 把最後計算完的 GameLogs2 存出去
write.csv(GameLogs2, "NBA_Final_Predictions.csv", row.names = FALSE)
cat("處理完成，已產生 NBA_Final_Predictions.csv\n")