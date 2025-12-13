setwd("C:/Users/jef81/Desktop/計算生物學/NBA")
GameLogs <- read.csv("NBA_GameLogs_202425.csv")
PlayerStats <- read.csv("NBA_PlayerStats_202425.csv")
TeamInfo <- read.csv("NBA_TeamInfo_202425.csv")
TeamStats <- read.csv("NBA_TeamStats_202425.csv")

NBA_Stats <- TeamStats[TeamStats$TEAM_ID %in% TeamInfo$TEAM_ID,]
ml_data <- subset(NBA_Stats, select = c("TEAM_ID", "TEAM_NAME", "PTS", "REB",
                                   "AST", "BLK", "STL", "TOV", "FG3_PCT", 
                                   "FG_PCT", "OFF_RATING", "DEF_RATING",
                                   "NET_RATING"))
library(dplyr)
library(tidyr)
library(ggplot2)

#### clean data ####
# --- Step 1. 先把 TeamInfo 的 ABBREVIATION 加進 ml_data ---
ml_data2 <- ml_data %>%
  left_join(TeamInfo %>% select(TEAM_ID, ABBREVIATION),
            by = "TEAM_ID")

# --- Step 2. 從 PlayerStats 取出需要的欄位 ---
player_data <- PlayerStats %>%
  select(PLAYER_NAME, TEAM_ABBREVIATION, MIN_base,
         PLUS_MINUS, NBA_FANTASY_PTS)

# --- Step 3. 每隊選上場時間前 8 名 ---
top8_players <- player_data %>%
  group_by(TEAM_ABBREVIATION) %>%
  arrange(desc(MIN_base)) %>%
  slice_head(n = 8) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# --- Step 4. 寬格式 pivot_wider ---
top8_wide <- top8_players %>%
  select(TEAM_ABBREVIATION, rank,
         PLAYER_NAME, PLUS_MINUS, NBA_FANTASY_PTS) %>%
  pivot_wider(
    names_from = rank,
    values_from = c(PLAYER_NAME, PLUS_MINUS, NBA_FANTASY_PTS),
    names_glue = "player{rank}_{.value}"
  )

# 改成你要的欄位名稱：
names(top8_wide) <- gsub("PLAYER_NAME", "player", names(top8_wide))
names(top8_wide) <- gsub("PLUS_MINUS", "pm", names(top8_wide))
names(top8_wide) <- gsub("NBA_FANTASY_PTS", "fantasy_pts", names(top8_wide))

# --- Step 5. 用 ABBREVIATION 合併進 ml_data2 ---
ml_data_final <- ml_data2 %>%
  left_join(top8_wide,
            by = c("ABBREVIATION" = "TEAM_ABBREVIATION"))
# 把 ABBREVIATION 加入最終資料
ml_data_final <- ml_data_final %>%
  mutate(ABBREVIATION = ABBREVIATION)

# 如果 MATCHUP 包含 "@", 代表客場
GameLogs2 <- GameLogs %>%
  mutate(
    Home = ifelse(grepl("@", MATCHUP), 0, 1),
    WL_num = ifelse(WL == "W", 1, 0)
  )

# --- Step 2. 把 ml_data_final 的球隊變數加入 ---
# 先選 ml_data_final 需要的欄位
team_vars <- ml_data_final

# --- Step 1. 解析主客場與勝負 ---
GameLogs2 <- GameLogs %>%
  select(TEAM_ABBREVIATION, MATCHUP, WL) %>%  # 只保留合併用欄位
  mutate(
    Home = ifelse(grepl("@", MATCHUP), 0, 1),
    WL_num = ifelse(WL == "W", 1, 0)
  )

# --- Step 2. 合併 ml_data_final ---
GameLogs2 <- GameLogs2 %>%
  left_join(ml_data_final, by = c("TEAM_ABBREVIATION" = "ABBREVIATION"))



#### logistic regression模型 ####
# --- Step 3. 建立 logistic regression 模型 ---
# 只使用數值欄位（排除 WL_num）
num_vars <- names(GameLogs2)[sapply(GameLogs2, is.numeric)]
num_vars <- setdiff(num_vars, c("WL_num"))

formula <- as.formula(
  paste("WL_num ~", paste(num_vars, collapse = " + "))
)

logit_model <- glm(formula, data = GameLogs2, family = binomial)

# --- Step 4. 預測勝率 ---
GameLogs2$pred_prob <- predict(logit_model, type = "response")
GameLogs2$pred_class <- ifelse(GameLogs2$pred_prob > 0.5, 1, 0)

# --- Step 5. 計算準確率 ---
accuracy <- mean(GameLogs2$pred_class == GameLogs2$WL_num)
cat("逐場比賽 logistic regression 準確率:", round(accuracy,4), "\n")

# 計算逐場誤差
GameLogs2 <- GameLogs2 %>%
  mutate(
    GameNo = row_number(),
    error = abs(pred_prob - WL_num)  # 絕對誤差
  )

team_summary <- GameLogs2 %>%
  group_by(TEAM_ABBREVIATION) %>%
  summarise(
    avg_pred_prob = mean(pred_prob, na.rm = TRUE),  # 模型平均勝率
    actual_win_rate = mean(WL_num, na.rm = TRUE),  # 真實勝率
    .groups = "drop"
  )

# --- 畫散點圖 ---
ggplot(team_summary, aes(x = actual_win_rate, y = avg_pred_prob, label = TEAM_ABBREVIATION)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -0.5, size = 3) + # 標上球隊縮寫
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") + # y=x 參考線
  labs(
    title = "球隊預測勝率 vs 真實勝率",
    x = "真實勝率",
    y = "預測勝率"
  ) +
  theme_minimal()


library(dplyr)
library(e1071)
library(ggplot2)

# --- Step 1. 準備資料 ---
num_vars <- names(GameLogs2)[sapply(GameLogs2, is.numeric)]
num_vars <- setdiff(num_vars, c("WL_num", "GameNo", "error", "TEAM_ID"))

svm_data <- GameLogs2 %>%
  select(all_of(c("WL_num", num_vars)))

svm_data$WL_num <- as.factor(svm_data$WL_num)

# --- Step 2. K 折交叉驗證 ---
set.seed(123)
K <- 5
n <- nrow(svm_data)
folds <- sample(rep(1:K, length.out = n))

accuracy_list <- c()

for(k in 1:K){
  train_idx <- which(folds != k)
  test_idx  <- which(folds == k)
  
  train_data <- svm_data[train_idx, ]
  test_data  <- svm_data[test_idx, ]
  
  # 使用預設 SVM 模型（RBF kernel）
  svm_model <- svm(WL_num ~ ., data = train_data, kernel = "radial", scale = TRUE)
  
  pred <- predict(svm_model, test_data)
  acc <- mean(pred == test_data$WL_num)
  accuracy_list <- c(accuracy_list, acc)
}

cat("【基礎 SVM】5 折交叉驗證平均準確率:", round(mean(accuracy_list),4), "\n")


# --- Step 3. 建立最終模型（使用全部資料） ---
svm_final <- svm(
  WL_num ~ ., 
  data = svm_data, 
  kernel = "radial", 
  scale = TRUE,
  probability = TRUE
)

# 預測機率
svm_pred_prob <- predict(svm_final, svm_data, probability = TRUE)
GameLogs2$svm_pred_prob <- attr(svm_pred_prob, "probabilities")[, "1"]

# 預測誤差
GameLogs2$svm_error <- abs(GameLogs2$svm_pred_prob - GameLogs2$WL_num)


# --- Step 4. 計算每隊平均預測勝率 ---
team_summary_svm <- GameLogs2 %>%
  group_by(TEAM_ABBREVIATION) %>%
  summarise(
    avg_pred_prob = mean(svm_pred_prob, na.rm = TRUE),
    actual_win_rate = mean(WL_num, na.rm = TRUE),
    .groups = "drop"
  )


# --- Step 5. 視覺化：預測 vs 真實勝率 ---
ggplot(team_summary_svm, aes(x = actual_win_rate, y = avg_pred_prob, label = TEAM_ABBREVIATION)) +
  geom_point(color = "darkblue", size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "SVM 模型：球隊預測勝率 vs 真實勝率（無超參數調整）",
    x = "真實勝率",
    y = "預測勝率"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))







