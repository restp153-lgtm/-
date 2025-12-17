import streamlit as st
import pandas as pd
import plotly.express as px

# 設定頁面寬度
st.set_page_config(page_title="NBA 預測儀表板", layout="wide")

st.title("🏀 NBA 2024-25 賽季預測模型看板")

# 讀取 R 產生的資料
@st.cache_data
def load_data():
    return pd.read_csv("NBA_Final_Predictions.csv")

try:
    df = load_data()

    # 模型選擇器
    model_map = {
        "Logistic Regression": "pred_prob",
        "SVM": "svm_pred_prob",
        "Random Forest": "rf_pred_prob",
        "CART (Decision Tree)": "cart_pred_prob"
    }
    selected_label = st.sidebar.selectbox("切換預測模型", list(model_map.keys()))
    prob_col = model_map[selected_label]

    # --- 1. 球隊匯總計算 ---
    team_summary = df.groupby('TEAM_ABBREVIATION').agg({
        'WL_num': 'mean',
        prob_col: 'mean'
    }).reset_index()
    team_summary.columns = ['球隊', '實際勝率', '模型預測勝率']

    # --- 2. 視覺化：散點圖 ---
    st.subheader(f"📊 {selected_label}：各隊預測勝率 vs 真實勝率")
    fig = px.scatter(
        team_summary, x="實際勝率", y="模型預測勝率", 
        text="球隊", trendline="ols",
        labels={"實際勝率": "實際勝率", "模型預測勝率": "模型預測勝率"},
        template="plotly_white", height=600
    )
    # 加入 y=x 參考線
    fig.add_shape(type="line", x0=0, y0=0, x1=1, y1=1, line=dict(color="Red", dash="dash"))
    fig.update_traces(textposition='top center', marker=dict(size=10, opacity=0.8))
    st.plotly_chart(fig, use_container_width=True)

    # --- 3. 球隊詳細數據表 ---
    st.subheader("📁 各隊勝率明細")
    st.dataframe(team_summary.style.background_gradient(cmap='Blues'), use_container_width=True)

    # --- 4. 逐場預測明細 ---
    st.subheader("📅 逐場比賽預測明細 (前 100 場)")
    game_detail = df[['TEAM_ABBREVIATION', 'MATCHUP', 'WL', 'WL_num', prob_col]].copy()
    game_detail['預測成功'] = ((game_detail[prob_col] > 0.5) == game_detail['WL_num']).map({True: "✅", False: "❌"})
    st.table(game_detail.head(100))

except FileNotFoundError:
    st.error("找不到 'NBA_Final_Predictions.csv'。請先在 R 中運行代碼產生匯出檔案。")