## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

load("37938-0001-Data.rda")
my_data <- da37938.0001

my_data <- my_data %>% 
  mutate(RACE = recode(RACE, 
                       "(1) Asian" = "Asian",
                       "(2) Black/AA" = "Black",
                       "(3) Hispanic/Latino" = "Hispanic",
                       "(4) Middle Eastern" = "Middle Eastern",
                       "(5) Native Hawaiian/Pacific Islander" = "Native Hawaiian",
                       "(6) White" = "White", 
                       "(7) American Indian" = "American Indian", 
                       "(8) Multirace" = "Multirace",
                       "(9) Other" = "Other"))

unweighted_props <- my_data %>% 
  group_by(RACE) %>% 
  summarize(count = n()) %>% 
  mutate(Type = "Unweighted", prop = count / sum(count))

weighted_props <- my_data %>%
  group_by(RACE) %>%
  summarize(count = sum(WEIGHT))%>%
  mutate(Type = "Weighted", prop = count / sum(my_data$WEIGHT))

combined_props <- bind_rows(unweighted_props, weighted_props)

ggplot(combined_props, aes(x = RACE, y = prop, fill = Type)) +
  geom_col(position = position_dodge()) +
  ggtitle("Proportions by Race/Ethnicity") +
  xlab("Race/Ethnicity") + 
  ylab("Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

load("37938-0001-Data.rda")
my_data <- da37938.0001

my_data <- my_data %>% 
  filter(!is.na(SEXUALID))

my_data <- my_data %>% 
  mutate(SEXUALID = recode(SEXUALID, 
                       "(1) Straight/heterosexual" = "Heterosexual",
                       "(2) Lesbian" = "Lesbian",
                       "(3) Gay" = "Gay",
                       "(4) Bisexual" = "Bisexual",
                       "(5) Queer" = "Queer",
                       "(6) Same-gender loving" = "Homosexuals", 
                       "(7) Other" = "Other", 
                       "(8) Asexual spectrum" = "Asexual",
                       "(9) Pansexual" = "Pansexual"))

unweighted_props <- my_data %>% 
  group_by(SEXUALID) %>% 
  summarize(count = n()) %>% 
  mutate(Type = "Unweighted", prop = count / sum(count))

weighted_props <- my_data %>%
  group_by(SEXUALID) %>%
  summarize(count = sum(WEIGHT))%>%
  mutate(Type = "Weighted", prop = count / sum(my_data$WEIGHT))

combined_props <- bind_rows(unweighted_props, weighted_props)

ggplot(combined_props, aes(x = SEXUALID, y = prop, fill = Type)) +
  geom_col(position = position_dodge()) +
  ggtitle("Proportions by Sexual Orientations") +
  xlab("Sexual Orientation") + 
  ylab("Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

load("37938-0001-Data.rda")
my_data <- da37938.0001

my_data <- my_data %>% select(STUDYID, LIFESAT, LIFESAT_I, SOCIALWB, SOCIALWB_I, 
                              NONAFFIRM, NONAFFIRM_I, NONDISCLOSURE, 
                              NONDISCLOSURE_I, HCTHREAT, HCTHREAT_I,
                              KESSLER6, KESSLER6_I, EVERYDAY, EVERYDAY_I)

my_data <- na.omit(my_data)

ggplot(my_data, aes(x = SOCIALWB_I, y = LIFESAT_I)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', color="red") +
  geom_smooth(method = 'loess', color = "blue", formula = 'y ~ x') +
  labs(x = "Social well-being", y = "Satisfaction with life") + 
  ggtitle("Satisfaction with life vs Social well-being") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
ggplot(my_data, aes(x = NONAFFIRM_I, y = LIFESAT_I)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', color="red") +
  geom_smooth(method = 'loess', color = "blue", formula = 'y ~ x') +
  labs(x = "Non-affirmation of gender identity", y = "Satisfaction with life") +
  ggtitle("Non-affirmation of gender identity vs Satisfaction with life") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
ggplot(my_data, aes(x = NONDISCLOSURE_I, y = LIFESAT_I)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', color="red") +
  geom_smooth(method = 'loess', color = "blue", formula = 'y ~ x') +
  labs(x = "Non-disclosure of gender identity", y = "Satisfaction with life") +
  ggtitle("Non-disclosure of gender identity vs Satisfaction with life") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
ggplot(my_data, aes(x = HCTHREAT_I, y = LIFESAT_I)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', color="red") +
  geom_smooth(method = 'loess', color = "blue", formula = 'y ~ x') +
  labs(x = "Healthcare stereotype threat", y = "Satisfaction with life") +
  ggtitle("Healthcare stereotype threat vs Satisfaction with life") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
ggplot(my_data, aes(x = KESSLER6_I, y = LIFESAT_I)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', color="red") +
  geom_smooth(method = 'loess', color = "blue", formula = 'y ~ x') +
  labs(x = "Mental distress/disorder", y = "Satisfaction with life") +
  ggtitle("Mental distress/disorder vs Satisfaction with life") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
ggplot(my_data, aes(x = EVERYDAY_I, y = LIFESAT_I)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x', color="red") +
  geom_smooth(method = 'loess', color = "blue", formula = 'y ~ x') +
  labs(x = "Everyday discrimination", y = "Satisfaction with life") +
  ggtitle("Everyday discrimination vs Satisfaction with life") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
model1 <- lm(LIFESAT_I ~ KESSLER6_I, data = my_data)
summary(model1)


## -------------------------------------------------------------------------------------------------------------------
library(modelr)

my_data %>%
  add_residuals(model1, "resid") %>%
  ggplot(aes(x = KESSLER6_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  labs(x = "Mental distress/disorder", y = "Residuals") +
  ggtitle("Mental distress/disorder vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
my_data %>%
  add_residuals(model1, "resid") %>%
  ggplot(aes(sample=resid)) +
  geom_qq() +
  ggtitle("QQPlot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
my_data <- my_data %>%
  add_residuals(model1, "resid") %>%
  filter(resid <= 4)
new_model1 <- lm(LIFESAT_I ~ KESSLER6_I, data = my_data)
summary(new_model1)


## -------------------------------------------------------------------------------------------------------------------
my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(x = KESSLER6_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  labs(x = "Mental distress/disorder", y = "Residuals") +
  ggtitle("Mental distress/disorder vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(sample=resid)) +
  geom_qq() +
  ggtitle("QQPlot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(x = SOCIALWB_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  geom_smooth(aes(y = resid), color = "blue", method = 'loess', formula = 'y ~ x') +
  geom_smooth(aes(y = resid), method = "lm", se = FALSE, formula = 'y ~ x', color = "red") +
  labs(x = "Social well-being", y = "Residuals") +
  ggtitle("Social well-being vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(x = NONAFFIRM_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  geom_smooth(aes(y = resid), color = "blue", method = 'loess', formula = 'y ~ x') +
  geom_smooth(aes(y = resid), method = "lm", se = FALSE, formula = 'y ~ x', color = "red") +
  labs(x = "Non-affirmation of gender identity", y = "Residuals") +
  ggtitle("Non-affirmation of gender identity vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(x = NONDISCLOSURE_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  geom_smooth(aes(y = resid), color = "blue", method = 'loess', formula = 'y ~ x') +
  geom_smooth(aes(y = resid), method = "lm", se = FALSE, formula = 'y ~ x', color = "red") +
  labs(x = "Non-disclosure of gender identity", y = "Residuals") +
  ggtitle("Non-disclosure of gender identity vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(x = HCTHREAT_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  geom_smooth(aes(y = resid), color = "blue", method = 'loess', formula = 'y ~ x') +
  geom_smooth(aes(y = resid), method = "lm", se = FALSE, formula = 'y ~ x', color = "red") +
  labs(x = "Healthcare stereotype threat", y = "Residuals") +
  ggtitle("Healthcare stereotype threat vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

my_data %>%
  add_residuals(new_model1, "resid") %>%
  ggplot(aes(x = EVERYDAY_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  geom_smooth(aes(y = resid), color = "blue", method = 'loess', formula = 'y ~ x') +
  geom_smooth(aes(y = resid), method = "lm", se = FALSE, formula = 'y ~ x', color = "red") +
  labs(x = "Everyday discrimination", y = "Residuals") +
  ggtitle("Everyday discrimination vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
model2 <- lm(LIFESAT_I ~ KESSLER6_I + SOCIALWB_I, data = my_data)
summary(model2)


## -------------------------------------------------------------------------------------------------------------------
my_data %>%
  add_residuals(model2, "resid") %>%
  ggplot(aes(x = KESSLER6_I + SOCIALWB_I)) +
  geom_point(aes(y = resid), alpha = 0.35) +
  labs(x = "Mental distress/disorder + Social well-being", y = "Residuals") +
  ggtitle("Mental distress/disorder + Social well-being vs Residuals") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## -------------------------------------------------------------------------------------------------------------------
my_data %>%
  add_residuals(model2, "resid") %>%
  ggplot(aes(sample=resid)) +
  geom_qq() +
  ggtitle("QQPlot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

