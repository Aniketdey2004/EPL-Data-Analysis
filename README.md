# ⚽ English Premier League Match Analysis

This project explores and analyzes match data from the English Premier League (EPL), focusing on performance patterns, player behavior, and match outcomes. The analysis includes univariate, bivariate, and categorical variable exploration, supported by visualizations and insights.

---

## 📂 Dataset Overview

The dataset includes match-level statistics such as:

- Goals (Half-time & Full-time)
- Shots (Total & On Target)
- Fouls, Yellow & Red Cards
- Corners
- Match Outcomes (Win/Loss/Draw)
- Season and Referee Information

-Link - https://www.kaggle.com/datasets/panaaaaa/english-premier-league-and-championship-full-dataset

---

## 📊 Exploratory Data Analysis

### 🔹 Univariate Analysis

- **Goals Scored (Home & Away):**
  - Most teams score between **0–2 goals**.
  - The data is **discrete**, **positively skewed**, and includes **high-scoring outliers**.
  
- **Shots & Shots on Target:**
  - Home: ~10–14 shots (3–7 on target)
  - Away: ~7–11 shots (2–5 on target)
  - Distributions are **right-skewed** with notable **outliers indicating dominance**.

- **Fouls & Cards:**
  - Both teams average ~9–13 fouls.
  - Home teams show more **instances of clean play**.
  - Yellow Cards: Home (~0–2), Away (~1–2)
  - Red Cards: Mostly 0, occasionally 1–2 (more for home teams)

- **Corners:**
  - Home: 4–7 (more frequent), Away: 3–5
  - High outliers in both suggest **dominant attacking performances**.

---

### 🔹 Categorical Insights

- **Seasons:**
  - 1993/94 and 1994/95 were the **longest seasons**.
  - 2024/25 had the **fewest matches**.
  
- **Match Results:**
  - **Home wins** are the most common result across all seasons.

---

### 🔹 Bivariate Analysis

- **Goals Relationships:**
  - Strong **positive correlation** between half-time and full-time goals.
  - **Negative correlation** between home and away goals.

- **Shots vs. Goals:**
  - Strong **positive linear relationship**.
  - Shots on target are better predictors of goals than total shots.

- **Corners vs. Goals:**
  - Positive relationship: more corners often lead to more goals.

- **Fouls vs. Shots:**
  - No clear correlation; **fouls don’t significantly impact the opponent’s attacking play**.

- **Match Outcome Predictors:**
  - **High goals, shots, shots on target, and corners** are key indicators of wins.
  - **Fouls and cards** show no strong relationship with match outcomes.

---



### 🔹 Referee Analysis

- Top 20 referees by number of matches were visualized.
- **M. Dean** officiated the most matches by far.

---

## ✅ Key Takeaways

- **Home Advantage is Real:** Home teams generally score more, shoot more, and win more.
- **Shots on Target Are Key:** High correlation with goals, outperforming total shots as a predictor.
- **Corners Reflect Dominance:** Strong link between corners and offensive pressure leading to goals.
- **Dirty Play Patterns:** Outliers in red/yellow cards show aggression under pressure, often from home teams.
- **Referee Influence Exists:** A few referees officiate disproportionately high numbers of matches.
- **Performance Fluctuations:** Teams and matches vary significantly season-to-season, making rolling and seasonal trends critical.
- **Goal-Driven Results:** Goals, especially full-time goals, are the most predictive features for match outcomes.
- **Clean Play Not Always Rewarded:** Fouls and cards do not directly correlate with match results.
- **Consistent Patterns Across Seasons:** Despite varying lengths of seasons, the distribution of performance metrics remains stable.

---

## 📌 Requirements

- R (≥ 4.1.0)
- "dplyr", "ggplot2", "ggrepel", "tidyr","forcats","knitr","zoo","corrplot","pactchwork","plotly"
- RStudio (Recommended for visualization and plotting)

---


---

## 🧠 Author

Analysis conducted by [Your Name], April 2025  
For queries or collaboration, feel free to reach out.

---

