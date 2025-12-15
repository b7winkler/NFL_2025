offense_raw <- read.csv("/Users/benwinkler/Desktop/NFL_2025_offense_raw_newCSV.csv",
                        stringsAsFactors = FALSE)

View(offense_raw)

defense_raw <- read.csv("/Users/benwinkler/Desktop/NFL_2025_defense_raw_newCSV.csv",
                        stringsAsFactors = FALSE)

View(defense_raw)        
names(defense_raw)      
head(defense_raw)        

offense_raw <- offense_raw[, !names(offense_raw) %in% "Pen"] #the following 4 queries are used to remove columns that weren't needed
defense_raw <- defense_raw[, !names(defense_raw) %in% "Pen"]

offense_raw <- offense_raw[, !names(offense_raw) %in% "Yds.1"]
defense_raw <- defense_raw[, !names(defense_raw) %in% "Yds.1"]

offense_raw <- offense_raw[, !names(offense_raw) %in% "X1stPy"]
defense_raw <- defense_raw[, !names(defense_raw) %in% "X1stPy"]

offense_raw <- offense_raw[, !names(offense_raw) %in% "FL"]
defense_raw <- defense_raw[, !names(defense_raw) %in% "FL"]

offense_raw <- offense_raw[1:(nrow(offense_raw) - 3), ] #this removed the last three rows which just showed averages for each column
defense_raw <- defense_raw[1:(nrow(defense_raw) - 3), ]

drive_averages <- read.csv("/Users/benwinkler/Desktop/Drive_Averages_Data .csv", 
                           stringsAsFactors = FALSE)

view(drive_averages)

drive_averages <- drive_averages[-nrow(drive_averages), ]

advanced_defense <- read.csv("/Users/benwinkler/Desktop/Advanced_Defense_Data.csv",
                             stringsAsFactors = FALSE)

view(advanced_defense)

# Cleaning columns to not have %
advanced_defense$Bltz. <- as.numeric(gsub("%", "", advanced_defense$Bltz.))
advanced_defense$Prss. <- as.numeric(gsub("%", "", advanced_defense$Prss.))


# Check the result
head(advanced_defense$Bltz.)

# Cleaning columns to not have %
advanced_defense$QBKD. <- as.numeric(gsub("%", "", advanced_defense$QBKD.))


library(ggplot2)
#Vis 1: Avg Time of Drives vs Percentage of Drives Ending in Score (Scatterplot)
ggplot(drive_averages, aes(x = Time, y = Sc., label = Tm)) +
  geom_point() +
  geom_text(vjust = -0.5, size = 3) +
  labs(
    title = "Avg Time of Drives vs Percentage of Drives Ending in Score",
    x = "Avg Time of Drives",
    y = "Percentage of Drives Ending in Score"
  ) 

#Vis 2 (Histogram): 
library(ggplot2)

top <- offense_raw[offense_raw$TO > 20, ]

ggplot(offense_raw, aes(x = TO)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(data = top, aes(label = Tm, y = 1), vjust = -1) +
  labs(
    title = "Distribution of Offensive Turnovers (TO)",
    x = "Turnovers (TO)",
    y = "Number of Teams"
  )

#Vis 3 (Scatterplot):
library(ggplot2)

ggplot(offense_raw, aes(x = Att_Pass, y = NY.A)) +
  geom_point() +
  geom_text(aes(label = Tm), vjust = -0.5) +
  labs(
    title = "Passing Attempts vs. Net Yards per Attempt",
    x = "Pass Attempts (Att_Pass)",
    y = "Net Yards per Attempt (NY.A)"
  )

#Vis 4 (Scatterplot):
library(ggplot2)

ggplot(offense_raw, aes(x = Att_Rush, y = Y.A)) +
  geom_point() +
  geom_text(aes(label = Tm), vjust = -0.5) +
  labs(
    title = "Rushing Attempts vs. Yards per Attempt",
    x = "Rushing Attempts (Att_Rush)",
    y = "Yards per Attempt (Y.A)"
  )

#Vis 5 (scatterplot)
library(ggplot2)

ggplot(offense_raw, aes(x = Att_Rush, y = X1stDown_Rush)) +
  geom_point() +
  geom_text(aes(label = Tm), vjust = -0.5) +
  labs(
    title = "Rushing Attempts vs. Rushing First Downs",
    x = "Rushing Attempts (Att_Rush)",
    y = "Rushing First Downs (X1stDown_Rush)"
  )

#Vis 6 (Boxplot)
library(ggplot2)

ggplot(offense_raw, aes(x = Tm, y = Sc.)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Sc. by Team",
    x = "Team",
    y = "Sc."
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Vis 7 (Scatterplot)
library(ggplot2)

ggplot(offense_raw, aes(x = TO, y = Y.P)) +
  geom_point() +
  geom_text(aes(label = Tm), vjust = -0.5) +
  labs(
    title = "Turnovers vs. Yards per Play",
    x = "Turnovers (TO)",
    y = "Yards per Play (Y.P)"
  )

#Vis 8 (Bar graph)
library(ggplot2)

ggplot(drive_averages, aes(x = reorder(Tm, Yds), y = Yds)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total Yards Per Drive by Team",
    x = "Team",
    y = "Yards Per Drive"
  ) 


#Vis 9 (Used to pick linear regression variables)
library(ggplot2)
library(dplyr)
library(tidyr)

# Select numeric variables
numeric_vars <- offense_raw %>% select(where(is.numeric))

# Compute correlation matrix
corr_matrix <- cor(numeric_vars, use = "complete.obs")

# Convert matrix to long format WITHOUT reshape2
corr_long <- as.data.frame(corr_matrix) %>%
  mutate(Var1 = rownames(.)) %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "value"
  )

# Plot heatmap
ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Correlation Heatmap of Offensive Statistics",
    x = "",
    y = ""
  )

#Vis 10
library(ggplot2)
library(dplyr)

# Order teams by Yds descending
defense_plot <- defense_raw %>%
  arrange(desc(Yds)) %>%
  mutate(Team = factor(Tm, levels = Tm))

ggplot(defense_raw, aes(x = "Yds", y = Tm)) +
  geom_point(aes(size = Yds, color = Yds)) +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Bubble Heatmap of Defensive Yards Allowed",
    x = "",
    y = "Team",
    size = "Yards Allowed",
    color = "Yards Allowed"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#Vis 11
advanced_defense_ordered <- advanced_defense %>%
  arrange(desc(QBKD.)) %>%
  mutate(Team = factor(Tm, levels = Tm))

ggplot(advanced_defense_ordered, aes(x = Bltz., y = QBKD., label = Tm)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Blitz Rate vs QB Knockdowns (Teams Ordered by QBKD)",
    x = "Blitz Rate (Bltz.)",
    y = "QB Knockdowns (QBKD.)"
  ) +
  theme_minimal()


#Vis 12
library(dplyr)
library(ggplot2)

# 1. Merge the datasets by team
combined <- defense_raw %>%
  inner_join(advanced_defense, by = "Tm")

# 2. Ensure Prss. is numeric (if it has % signs)
combined$Prss. <- as.numeric(gsub("%", "", combined$Prss.))

# Optional: convert to decimal
# combined$Prss. <- combined$Prss. / 100

# 3. Create scatterplot with labels (without ggrepel)
ggplot(combined, aes(x = Prss., y = TO)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(aes(label = Tm), vjust = -0.5, hjust = 0.5, size = 3) +  # label points
  labs(
    x = "Pressure % (Prss.)",
    y = "Turnovers (TO)",
    title = "Turnovers vs Pressure % by Team"
  ) +
  theme_minimal()

#Vis 13
library(ggplot2)
library(dplyr)

Q1 <- quantile(advanced_defense$MTkl, 0.25)
Q3 <- quantile(advanced_defense$MTkl, 0.75)
IQR_val <- IQR(advanced_defense$MTkl)

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- advanced_defense %>%
  filter(MTkl < lower_bound | MTkl > upper_bound)

ggplot(advanced_defense, aes(x = MTkl)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_point(data = outliers, aes(y = 0), color = "red", size = 3) +
  geom_text(
    data = outliers,
    aes(y = 0, label = Tm),
    vjust = -0.7, size = 3, color = "red"
  ) +
  labs(
    title = "Histogram of Missed Tackles (MTkl) with Outliers Labeled",
    x = "Missed Tackles (MTkl)",
    y = "Number of Teams"
  ) +
  theme_minimal()

#Vis 14
library(ggplot2)

ggplot(advanced_defense, aes(x = Prss., y = DADOT)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_text(aes(label = Tm), vjust = -0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Pressure Rate vs DADOT",
    x = "Pressure Rate (Prss.)",
    y = "DADOT (Depth of Target)"
  ) +
  theme_minimal()

#Vis 15
library(ggplot2)

ggplot(defense_raw, aes(x = Y.A, y = Sc.)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_text(aes(label = Tm), vjust = -0.7, size = 3) +
  labs(
    title = "Y.A vs Sc. (Defense)",
    x = "Yards Allowed per Attempt (Y.A)",
    y = "Points Allowed (Sc.)"
  ) +
  theme_minimal()

#Vis 16
library(ggplot2)

ggplot(defense_raw, aes(x = NY.A, y = Int)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_text(aes(label = Tm), vjust = -0.7, size = 3) +
  labs(
    title = "NY/A vs INT (Defense)",
    x = "Net Yards Allowed per Attempt (NY/A)",
    y = "Interceptions (INT)"
  ) +
  theme_minimal()



# REGRESSION
#1: Regression for Offense
# Load packages
library(tidyverse)

# View structure (optional, helps confirm column names)
str(offense_raw)

# Fit linear regression model
offense_model <- lm(EXP ~ NY.A + Sc. + TO. + Y.P, data = offense_raw)

# Show full regression summary
summary(offense_model)

# Optional: diagnostic plots
par(mfrow = c(2, 2))
plot(offense_model)

# Predict Offense EXPPG
offense_raw$Pred_Off_EXPPG <- predict(offense_model)

#2: Regression for Defense
library(tidyverse)

# View structure (optional, helps confirm column names)
str(defense_raw)

# Fit linear regression model
defense_model <- lm(EXP ~ NY.A + Sc. + TO. + Y.P, data = defense_raw)

# Show full regression summary
summary(defense_model)

# Optional: diagnostic plots
par(mfrow = c(2, 2))
plot(defense_model)

#Predict Defense EXPPG
defense_raw$Pred_Def_EXPPG <- predict(defense_model)

# Merge together by team name
library(dplyr)

combined <- offense_raw %>%
  select(Tm, Pred_Off_EXPPG) %>%
  left_join(defense_raw %>% select(Tm, Pred_Def_EXPPG), by = "Tm")

# Calculate TeamScore
combined$TeamScore <- combined$Pred_Off_EXPPG + combined$Pred_Def_EXPPG

# Standardize TeamScore
combined$TeamScore_z <- scale(combined$TeamScore)

# Rank by TeamScore
combined_ranked <- combined[order(-combined$TeamScore), ]

combined_ranked$Rank <- seq_len(nrow(combined_ranked))

head(combined_ranked[, c("Rank", "Tm", "Pred_Off_EXPPG", "Pred_Def_EXPPG", "TeamScore")], 32)



#SQL Queries
#Top 10 teams in Passing Efficiency 
library(sqldf)
sqldf("SELECT Tm, `NY.A` FROM offense_raw ORDER BY `NY.A` DESC LIMIT 10")

#Teams forcing the most interceptions
sqldf("SELECT Tm, `Int` FROM defense_raw ORDER BY `Int` DESC")

#Teams with the most turnovers
sqldf("SELECT Tm, `TO` FROM offense_raw ORDER BY `TO` DESC")

#Best pass defenses (lowest NY/A allowed)
sqldf("SELECT Tm, `NY.A` FROM defense_raw ORDER BY `NY.A` ASC")

#Top blitzing teams
sqldf("SELECT Tm, `Bltz.` FROM advanced_defense ORDER BY `Bltz.` DESC")



