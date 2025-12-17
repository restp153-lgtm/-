import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt

st.set_page_config(layout="wide")
st.title("🏀 NBA Game Prediction Dashboard")

# ---------------- Sidebar ----------------
page = st.sidebar.radio("頁面", ["模型結果", "模型比較"])

model_map = {
    "Logistic Regression": "logit",
    "SVM": "svm",
    "Random Forest": "rf",
    "CART": "cart"
}

model_name = st.sidebar.selectbox("選擇模型", list(model_map.keys()))
model_key = model_map[model_name]

# ---------------- Load Data ----------------
game_df = pd.read_csv(f"output/game_predictions_{model_key}.csv")
team_df = pd.read_csv(f"output/team_summary_{model_key}.csv")
metrics = pd.read_csv("output/model_metrics.csv")

# ---------------- Page 1: Model Result ----------------
if page == "模型結果":
    acc = metrics.loc[metrics.model == model_key, "accuracy"].values[0]
    st.sidebar.metric("模型準確率", f"{acc:.3f}")

    st.subheader("📊 球隊預測勝率 vs 真實勝率")

    fig, ax = plt.subplots()
    ax.scatter(team_df["actual_win_rate"], team_df["avg_pred_prob"])
    ax.plot([0,1],[0,1], linestyle="--")
    ax.set_xlabel("真實勝率")
    ax.set_ylabel("預測勝率")
    st.pyplot(fig)

    st.subheader("📄 球隊摘要")
    st.dataframe(team_df)

    st.subheader("📘 逐場比賽預測")
    teams = sorted(game_df["TEAM_ABBREVIATION"].unique())
    team_sel = st.selectbox("篩選球隊", ["All"] + teams)

    if team_sel != "All":
        st.dataframe(game_df[game_df["TEAM_ABBREVIATION"] == team_sel])
    else:
        st.dataframe(game_df)

# ---------------- Page 2: Model Comparison ----------------
else:
    st.subheader("📈 模型比較")

    st.dataframe(metrics)

    fig, ax = plt.subplots()
    ax.bar(metrics["model"], metrics["accuracy"])
    ax.set_ylabel("Accuracy")
    st.pyplot(fig)

    fig, ax = plt.subplots()
    ax.bar(metrics["model"], metrics["brier"])
    ax.set_ylabel("Brier Score (lower is better)")
    st.pyplot(fig)
