## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

matches <- read_csv("matches.csv", show_col_types = FALSE)
deliveries <- read_csv("deliveries.csv", show_col_types = FALSE)


## -------------------------------------------------------------------------------------------------------------------
matches$date <- as.Date(matches$date, format = "%d/%m/%y")


## -------------------------------------------------------------------------------------------------------------------
matches$team1 <-
  gsub("Rising Pune Supergiants",
       "Rising Pune Supergiant",
       matches$team1)
matches$team2 <-
  gsub("Rising Pune Supergiants",
       "Rising Pune Supergiant",
       matches$team2)
matches$winner <-
  gsub("Rising Pune Supergiants",
       "Rising Pune Supergiant",
       matches$winner)
matches$toss_winner <-
  gsub("Rising Pune Supergiants",
       "Rising Pune Supergiant",
       matches$toss_winner)
deliveries$batting_team <-
  gsub("Rising Pune Supergiants",
       "Rising Pune Supergiant",
       deliveries$batting_team)
deliveries$bowling_team <-
  gsub("Rising Pune Supergiants",
       "Rising Pune Supergiant",
       deliveries$bowling_team)
matches$venue <-
  gsub("Feroz Shah Kotla Ground", "Feroz Shah Kotla", matches$venue)
matches$venue <-
  gsub("M Chinnaswamy Stadium", "M. Chinnaswamy Stadium", matches$venue)
matches$venue <-
  gsub("MA Chidambaram Stadium, Chepauk",
       "M.A. Chidambaram Stadium",
       matches$venue)
matches$venue <-
  gsub("M. A. Chidambaram Stadium",
       "M.A. Chidambaram Stadium",
       matches$venue)
matches$venue <-
  gsub(
    "Punjab Cricket Association IS Bindra Stadium, Mohali",
    "Punjab Cricket Association Stadium",
    matches$venue
  )
matches$venue <-
  gsub(
    "Punjab Cricket Association Stadium, Mohali",
    "Punjab Cricket Association Stadium",
    matches$venue
  )
matches$venue <-
  gsub("IS Bindra Stadium",
       "Punjab Cricket Association Stadium",
       matches$venue)
matches$venue <-
  gsub(
    "Rajiv Gandhi International Stadium, Uppal",
    "Rajiv Gandhi International Stadium",
    matches$venue
  )
matches$venue <-
  gsub(
    "Rajiv Gandhi Intl. Cricket Stadium",
    "Rajiv Gandhi International Stadium",
    matches$venue
  )


## -------------------------------------------------------------------------------------------------------------------
city_matches <- matches %>% 
  group_by(city) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

top_10_cities <- head(city_matches, 10)

ggplot(top_10_cities, aes(x = reorder(city, count, FUN = rev), y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), nudge_y = 3) +
  labs(x = "City", y = "Number of matches") +
  coord_flip() +
  ggtitle("Top 10 Cities with most number of matches played") +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
matches_per_year <- matches %>%
  group_by(season) %>%
  summarise(count = n())

ggplot(matches_per_year, aes(x = season, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Number of matches") +
  ggtitle("Number of IPL matches per year") +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
wins <- matches %>%
  count(winner) %>%
  rename(name = winner)

ggplot(na.omit(wins), aes(x = reorder(name, n, FUN = rev), y = n)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Total wins by each team") + 
  xlab("Teams") + 
  ylab("Number of Matches Won") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = n), nudge_y = 4) + 
  coord_flip()


## -------------------------------------------------------------------------------------------------------------------
batsmen <- merge(matches[c('id', 'season')], deliveries, by.x='id', by.y='match_id')
batsmen <- batsmen[, !(names(batsmen) %in% c('id'))]
season <- aggregate(total_runs ~ season, data=batsmen, FUN=sum)

avgruns_each_season <- aggregate(id ~ season, data=matches, FUN=length)
names(avgruns_each_season) <- c('season', 'matches')
avgruns_each_season$total_runs <- season$total_runs
avgruns_each_season$average_runs_per_match <- avgruns_each_season$total_runs / avgruns_each_season$matches

ggplot() + 
  geom_line(data=avgruns_each_season, aes(x=season, y=average_runs_per_match)) +
  xlab("Season") + ylab("Average Runs") +
  ggtitle("Average Runs per Season")+
   theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
s_man_of_match <- matches %>% 
  group_by(player_of_match) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(15)

df_man_of_match <- data.frame(s_man_of_match) %>% 
  rename(times = count)
  # reset_index()

# Then, create the century dataset
cen <- deliveries %>% 
  group_by(batsman, match_id) %>% 
  summarize(batsman_runs = sum(batsman_runs)) %>% 
  filter(batsman_runs >= 100) %>% 
  group_by(batsman) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
  # reset_index()

# Half-century dataset
half_cen <- deliveries %>% 
  group_by(batsman, match_id) %>% 
  summarize(batsman_runs = sum(batsman_runs)) %>% 
  filter(batsman_runs >= 50 & batsman_runs < 100) %>% 
  group_by(batsman) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
  # reset_index()

# Merge the century and half-century datasets
df_big <- merge(cen, half_cen, by = "batsman", all = TRUE) %>% 
  replace_na(list(count.x = 0, count.y = 0))

# Strike rate dataset
df_strike_rate <- deliveries %>% 
  group_by(batsman) %>% 
  summarize(Ball = n(), batsman_runs = mean(batsman_runs)) %>% 
  arrange(desc(batsman_runs))
df_strike_rate <- rename(df_strike_rate, Strike.Rate = batsman_runs)
df_strike_rate$Strike.Rate <- df_strike_rate$Strike.Rate * 100

# Runs per match dataset
df_runs_per_match <- deliveries %>% 
  group_by(batsman, match_id) %>% 
  summarize(batsman_runs = sum(batsman_runs))
df_total_runs <- df_runs_per_match %>% 
  group_by(batsman) %>% 
  summarize(Batsman.Run = sum(batsman_runs), Match.Count = n(), Average.score = mean(batsman_runs))

# Number of sixes and fours datasets
df_sixes <- deliveries %>% 
  filter(batsman_runs == 6) %>% 
  group_by(batsman) %>% 
  summarize(Six = n())
df_four <- deliveries %>% 
  filter(batsman_runs == 4) %>% 
  group_by(batsman) %>% 
  summarize(Four = n())

# Merge all the datasets
df_batsman_stat <- full_join(full_join(full_join(df_strike_rate, df_total_runs, by = "batsman"), df_sixes, by = "batsman"), df_four, by = "batsman")

colnames(df_batsman_stat) <- c("batsman", "Ball", "Strike_Rate", "Batsman_Run", "Match_Count", "Average_score", "Six", "Four")
# df_batsman_stat$Strike_Rate <- df_batsman_stat$Strike_Rate * 100
df_batsman_stat <- df_batsman_stat %>% arrange(desc(Batsman_Run)) 
  # reset_index(drop = TRUE)

batsman_stats <- full_join(df_batsman_stat, df_big, by = "batsman") %>% replace_na(list(`count_x` = 0, `count_y` = 0))
colnames(batsman_stats) <- c("batsman", "Ball", "Strike_Rate", "Batsman_Run", "Match_Count", "Average_score", "Six", "Four", "100s", "50s")


## -------------------------------------------------------------------------------------------------------------------
centuries <- batsman_stats[order(batsman_stats$`100s`),]
half_centuries <- batsman_stats[order(batsman_stats$`50s`),]
centuries[is.na(centuries)] <- 0
half_centuries[is.na(half_centuries)] <- 0
centuries <- centuries[order(centuries$`100s`),]
half_centuries <- centuries[order(centuries$`50s`),]
centuries <- tail(centuries, 20)
half_centuries <- tail(half_centuries, 20)


## -------------------------------------------------------------------------------------------------------------------
ggplot(centuries, aes(x = reorder(batsman,`100s`, FUN = rev), y = `100s`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `100s`), nudge_y = 0.1) +
  labs(x = "Batsman", y = "Number of 100s") +
  ggtitle("Top 20 Batsmasn with most number of 100s") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggplot(half_centuries, aes(x = reorder(batsman,`50s`, FUN = rev), y = `50s`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `50s`), nudge_y = 1) +
  labs(x = "Batsman", y = "Number of 50s") +
  ggtitle("Top 20 Batsmasn with most number of 50s") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()


## -------------------------------------------------------------------------------------------------------------------
data <- read.csv(file = "26801-0001-Data.tsv", sep = "\t", na = c(-99))

data %>%
  pivot_longer(cols = starts_with("APR_RATE_"), names_to = "YEAR", 
               values_to = "APR") %>%
  select(SCL_UNITID, SCL_NAME, SPORT_CODE, SPORT_NAME, YEAR, APR) %>%
  mutate(YEAR = str_sub(YEAR, 10, 13)) -> data

print(data)

ggplot(data, aes(x = YEAR, y = APR)) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle("APR Distribution From 2004 - 2014") +
  xlab("Year") +
  ylab("APR") +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
data %>%
  filter(SPORT_CODE >= 1 & SPORT_CODE <= 37) %>%
  mutate(GENDER = ifelse(SPORT_CODE >= 1 &
                           SPORT_CODE <= 18, "Men", "Women")) -> data

ggplot(data, aes(x = YEAR, y = APR, color = GENDER)) +
  geom_boxplot(na.rm = TRUE) +
  scale_color_manual(values = c("Blue", "Pink")) +
  ggtitle("APR Distribution Over Time by Gender Division") +
  xlab("Year") +
  ylab("APR") +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
data %>%
  filter(GENDER == "Men") %>%
  ggplot(aes(x = SPORT_NAME, y = APR)) +
    geom_boxplot(na.rm = TRUE) +
    ggtitle("APR Distribution for Men's Sports") +
    xlab("Sport") +
    ylab("APR") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip()

