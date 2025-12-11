import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib import font_manager

# 加入中文字體（本機字體）
plt.rcParams['font.family'] = 'Microsoft JhengHei'
st.title("🏀 NBA Game Prediction Dashboard (R Models + Streamlit UI)")
font_manager.fontManager.addfont("fonts/msjh.ttf")
plt.rcParams['font.family'] = 'Microsoft JhengHei'
# --- Sidebar ---
model = st.sidebar.selectbox(
    "選擇模型",
    ["Logistic Regression", "SVM"]
)

# --- Load CSVs based on model ---
if model == "Logistic Regression":
    game_df = pd.read_csv("output/game_predictions_logit.csv")
    team_df = pd.read_csv("output/team_summary_logit.csv")
elif model == "SVM":
    game_df = pd.read_csv("output/game_predictions_svm.csv")
    team_df = pd.read_csv("output/team_summary_svm.csv")

metrics = pd.read_csv("output/model_metrics.csv")
model_acc = metrics[metrics["model"] == model.split()[0].lower()]["accuracy"].values[0]

st.sidebar.metric(label="模型準確率", value=f"{model_acc:.3f}")

# --- Team Summary Plot ---
st.subheader("📊 每隊預測 vs 真實勝率")

fig, ax = plt.subplots()
sns.scatterplot(data=team_df, x="actual_win_rate", y="avg_pred_prob", ax=ax)
plt.plot([0, 1], [0, 1], linestyle="--", color="red")
ax.set_xlabel("真實勝率")
ax.set_ylabel("預測勝率")
st.pyplot(fig)

# --- Show Team Data ---
st.subheader("📄 每隊摘要")
st.dataframe(team_df)

# --- Show Game Predictions ---
st.subheader("📘 逐場比賽預測")
st.dataframe(game_df)
