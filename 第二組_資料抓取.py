# ============================
# NBA Big Data Collector (2023-24 Example)
# ============================
# Requirements:
# pip install nba_api pandas requests numpy

from nba_api.stats.endpoints import (
    leaguedashteamstats,
    leaguedashplayerstats,
    leaguegamelog,
    teamdetails
)
import pandas as pd
import time

# ----------- 設定賽季 -----------
season = "2024-25"
print(f"Fetching NBA data for season {season}...\n")

# ======================================================
# 1️⃣ 球隊層級：Base + Advanced 數據
# ======================================================
def fetch_team_stats(season, measure_type):
    for i in range(3):
        try:
            df = leaguedashteamstats.LeagueDashTeamStats(
                season=season,
                measure_type_detailed_defense=measure_type,
                per_mode_detailed="PerGame"
            ).get_data_frames()[0]
            print(f"✅ Team stats ({measure_type}) loaded.")
            return df
        except Exception as e:
            print(f"Retry ({i+1}/3) fetching team {measure_type} stats... {e}")
            time.sleep(3)
    raise RuntimeError(f"Failed to fetch {measure_type} team stats.")

team_base = fetch_team_stats(season, "Base")
team_adv = fetch_team_stats(season, "Advanced")

# 合併球隊基本與進階數據
team_merged = pd.merge(
    team_base,
    team_adv,
    on=["TEAM_ID", "TEAM_NAME"],
    suffixes=("_base", "_adv")
)

# ======================================================
# 2️⃣ 球員層級：Base + Advanced 數據
# ======================================================
def fetch_player_stats(season, measure_type):
    for i in range(3):
        try:
            df = leaguedashplayerstats.LeagueDashPlayerStats(
                season=season,
                measure_type_detailed_defense=measure_type,
                per_mode_detailed="PerGame"
            ).get_data_frames()[0]
            print(f"✅ Player stats ({measure_type}) loaded.")
            return df
        except Exception as e:
            print(f"Retry ({i+1}/3) fetching player {measure_type} stats... {e}")
            time.sleep(3)
    raise RuntimeError(f"Failed to fetch {measure_type} player stats.")

player_base = fetch_player_stats(season, "Base")
player_adv = fetch_player_stats(season, "Advanced")

player_merged = pd.merge(
    player_base,
    player_adv,
    on=["PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION"],
    suffixes=("_base", "_adv")
)

# ======================================================
# 3️⃣ 比賽層級：Game Logs
# ======================================================
def fetch_game_logs(season):
    for i in range(3):
        try:
            df = leaguegamelog.LeagueGameLog(
                season=season,
                player_or_team_abbreviation="T"  # 球隊層級
            ).get_data_frames()[0]
            print("✅ Game logs loaded.")
            return df
        except Exception as e:
            print(f"Retry ({i+1}/3) fetching game logs... {e}")
            time.sleep(3)
    raise RuntimeError("Failed to fetch game logs.")

game_logs = fetch_game_logs(season)

# ======================================================
# 4️⃣ 球隊結構：城市、主場、簡稱
# ======================================================
team_info_list = []
for tid in team_merged["TEAM_ID"].unique():
    try:
        info = teamdetails.TeamDetails(team_id=tid).get_data_frames()[0]
        team_info_list.append(info)
        time.sleep(0.5)
    except Exception as e:
        print(f"⚠️ Failed to get details for TEAM_ID={tid}: {e}")

team_info = pd.concat(team_info_list, ignore_index=True)
print("✅ Team structure info loaded.\n")

# ======================================================
# 🧹 清理與輸出
# ======================================================
team_merged.fillna(0, inplace=True)
player_merged.fillna(0, inplace=True)
game_logs.fillna(0, inplace=True)
team_info.fillna("", inplace=True)

team_merged.to_csv(f"NBA_TeamStats_{season.replace('-', '')}.csv", index=False, encoding="utf-8-sig")
player_merged.to_csv(f"NBA_PlayerStats_{season.replace('-', '')}.csv", index=False, encoding="utf-8-sig")
game_logs.to_csv(f"NBA_GameLogs_{season.replace('-', '')}.csv", index=False, encoding="utf-8-sig")
team_info.to_csv(f"NBA_TeamInfo_{season.replace('-', '')}.csv", index=False, encoding="utf-8-sig")

print("🎯 All data saved successfully!")
print("📂 Files created:")
print(f" - NBA_TeamStats_{season.replace('-', '')}.csv")
print(f" - NBA_PlayerStats_{season.replace('-', '')}.csv")
print(f" - NBA_GameLogs_{season.replace('-', '')}.csv")
print(f" - NBA_TeamInfo_{season.replace('-', '')}.csv")

import tasks
# 1️⃣ 球隊總覽
lakers_summary = tasks.task_team_summary("Los Angeles Lakers", team_merged, game_logs)

# 2️⃣ 得分Top 10球員
top_scorers = tasks.task_top_players(player_merged, metric="PTS")

# 3️⃣ 主客場表現
home_away = tasks.task_home_away_performance("Boston Celtics", game_logs)

