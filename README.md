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

- Man United and Tottenham have played the most home matches in Premier League history, with 604 each. Traditional top clubs like Arsenal, Chelsea, Everton, and Liverpool closely follow
- Arsenal, Chelsea, Everton, Liverpool, Man United, and Tottenham have each played over 600 away matches, reflecting their consistent presence in the Premier League.
- Man United, Arsenal, and Liverpool dominate both home and away goal stats, showing consistent attacking strength. Man City holds the highest home goal difference (+579), emphasizing their strong home advantage. Most teams perform significantly better at home, indicating a clear home advantage trend in the league.
- Man United (2259), Arsenal (2197), and Liverpool (2168) lead the all-time scoring charts
- Man United, Arsenal, and Liverpool top the home win rate charts, turning their stadiums into consistent fortresses. Liverpool and Man City closely follow, reinforcing the dominance of traditional and modern top-six clubs at home
- Man United, Chelsea, and Arsenal boast the highest away win percentages, showing strong adaptability and control on the road. Man City and Liverpool also excel away from home
- Man United, Arsenal, and Liverpool emerge as the most successful teams overall, driven by high goal output and strong win rates both home and away
- Teams like Fulham, Stoke, and Newcastle show the strongest home advantage, winning over 20% more matches at home than away
- Bournemouth, Bolton, and Birmingham rank among the highest in average yellow cards, indicating a more aggressive or physical play style
- Hull, Stoke, and Wolves top the list of ‘dirtiest’ teams, based on a composite score likely reflecting yellow/red cards and fouls
- D Coote, M Dean, and M Riley are among the strictest EPL referees.
- Manchester United, Tottenham, and Arsenal lead the league in comeback wins after trailing at half-time. These clubs consistently demonstrate strong mentality and second-half dominance to overturn deficits.
- Man United,Arsenal and liverpool has the highest number of wins by playing fairly in comparison to their rival teams
- Man City dominate in recent years, with both achieving consistently high average goal differences per match—particularly post-2010, reflecting their Premier League success. Man United maintained strong numbers in the late '90s and early 2000s, with a slight decline post-2013 (post-Ferguson era).


---

## 📌 Requirements

- R (≥ 4.1.0)
- "dplyr", "ggplot2", "ggrepel", "tidyr","forcats","knitr","zoo","corrplot","pactchwork","plotly"
- RStudio (Recommended for visualization and plotting)

---


---

## 🧠 Author

Analysis conducted by Team Dexter, April 2025  
Pradyumna Bhowmick (Leader) 
Piyush Kumar Bharti 
Aniket Dey 
Aarav Raj 

---

