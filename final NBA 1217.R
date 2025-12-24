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
ml_data <- subset(
  NBA_Stats,
  select = c(
    # ── 識別 ──
    "TEAM_ID", "TEAM_NAME",
    
    # ── 傳統 box score ──
    "FGM", "FGA", "FG_PCT",
    "FG3M", "FG3A", "FG3_PCT",
    "FTM", "FTA", "FT_PCT",
    "OREB", "DREB", "REB",
    "AST", "TOV",
    "STL", "BLK", "BLKA",
    "PF", "PFD",
    "PTS",
    
    # ── 進階結構比例（重點） ──
    "AST_PCT", "AST_TO", "AST_RATIO",
    "OREB_PCT", "DREB_PCT", "REB_PCT",
    "TM_TOV_PCT",
    "EFG_PCT", "TS_PCT",
    "PIE",
    
    # ── 節奏與球權 ──
    "PACE", "E_PACE", "POSS",
    
    # ── 進階效率（保留但不依賴） ──
    "OFF_RATING", "DEF_RATING", "NET_RATING"
  )
)


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

# 如果沒有安裝 ggrepel，請先執行 install.packages("ggrepel")
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

# ==========================================
# 4. 以「球隊」為單位的模型分析 (修正函數衝突與視覺強化)
# ==========================================

# --- 定義優化後的繪圖函數 ---
create_high_res_plot <- function(df, x_col, model_name) {
  
  # 確保 X 軸數據是數值型態
  df$x_val <- as.numeric(df[[x_col]])
  
  ggplot(df, aes(x = x_val, y = Actual_Win_Rate)) +
    # 1. 理想線 (y = x) - 紅色虛線加粗
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#D7191C", linewidth = 1.2) +
    
    # 2. 模型回歸線 - 鮮豔藍色實線
    geom_smooth(method = "lm", formula = y ~ x, color = "#2C7BB6", se = FALSE, linewidth = 1.5) +
    
    # 3. 數據點 - 加大並增加透明度避免完全遮擋線條
    geom_point(size = 5, color = "#333333", alpha = 0.6) +
    
    # 4. 自動避讓標籤 - 解決重疊並放大字體
    # 移除 segment.linewidth 以相容舊版本，改用 segment.size
    geom_text_repel(
      aes(label = TEAM_ABBREVIATION),
      size = 7,              # 再次放大球隊縮寫
      fontface = "bold", 
      box.padding = 0.5, 
      point.padding = 0.5,
      segment.color = 'grey40',
      segment.size = 0.6     # 舊版 ggrepel 使用 segment.size
    ) +
    
    # 5. 標題與軸標籤
    labs(
      title = paste0(model_name, "：預測 vs 實際勝率"),
      subtitle = "紅色虛線：完美預測 | 藍色實線：模型擬合趨勢",
      x = "模型預測平均勝率",
      y = "實際勝率 (Winning %)"
    ) +
    
    # 6. 設定坐標軸比例與範圍
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    
    # 7. 視覺主題優化
    theme_bw(base_size = 20) + # 大幅提升基礎字體大小
    theme(
      # 使用 ggplot2::margin 確保呼叫正確的函數
      plot.title = element_text(face = "bold", size = 26, margin = ggplot2::margin(b = 15)),
      plot.subtitle = element_text(size = 18, color = "grey30", margin = ggplot2::margin(b = 20)),
      axis.title = element_text(face = "bold", size = 22),
      axis.text = element_text(size = 18, color = "black"),
      panel.grid.major = element_line(color = "grey92"),
      panel.border = element_rect(linewidth = 2, fill = NA)
    )
}

# --- 執行迴圈輸出並儲存 ---
# 確保 plot_configs 已定義（如前次對話所述）
for (config in plot_configs) {
  # 生成圖表
  p <- create_high_res_plot(team_summary, config$col, config$name)
  
  # 1. 在 R 視窗顯示
  print(p)
  
  # 2. 儲存成 PNG
  # 調整長寬比為 8x8，這會讓標籤在圖片中顯得更巨大清晰
  ggsave(
    filename = config$file, 
    plot = p, 
    width = 5,  
    height = 5, 
    dpi = 300,
    bg = "white"
  )
  
  cat(paste0("已匯出清晰圖表：", config$file, " (文字已放大)\n"))
}
# ==========================================
# 5. 匯出球隊預測報表 (供後續分析)
# ==========================================
write.csv(team_summary, "NBA_Team_Model_Evaluation.csv", row.names = FALSE)
# ==========================================
# 6. 計算並匯出準確率比較表
# ==========================================
acc_results <- data.frame(
  Model = titles,
  Accuracy = c(
    mean(GameLogs2$pred_class == GameLogs2$WL_num),
    mean(ifelse(GameLogs2$svm_pred_prob > 0.5, 1, 0) == GameLogs2$WL_num),
    mean(ifelse(GameLogs2$rf_pred_prob > 0.5, 1, 0) == GameLogs2$WL_num),
    mean(ifelse(GameLogs2$cart_pred_prob > 0.5, 1, 0) == GameLogs2$WL_num)
  )
)
print("--- 各模型準確率 (訓練集) ---")
print(acc_results)
