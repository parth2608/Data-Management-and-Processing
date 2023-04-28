## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
library(readr)
library(dplyr)
library(ggplot2)

path <- file.path(getwd(), "flavors_of_cacao.csv")
cocoa <- read_csv(file=path, show_col_types = FALSE) %>%
  setNames(c("company", "origin_bar", "ref", "review_year", "cocoa_perc", 
             "company_location", "rating", "bean_type", "bean_orig")) %>%
  mutate(cocoa_perc = as.numeric(gsub("%", "", cocoa_perc)))
head(cocoa)


## -------------------------------------------------------------------------------------------------------------------
ggplot(cocoa, aes(x= review_year, y = rating, color = cocoa_perc)) + 
    geom_point() + 
    geom_jitter() +
    geom_smooth()


## -------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
library(modelr)
library(ggplot2)

load("37938-0001-Data.rda")
my_data <- da37938.0001

my_data <- my_data %>% select(LIFESAT_I, SOCIALWB_I, NONAFFIRM_I, 
                              NONDISCLOSURE_I, HCTHREAT_I, KESSLER6_I, 
                              EVERYDAY_I)

my_data <- na.omit(my_data)

set.seed(2)
my_data_part <- resample_partition(my_data, p=c(train=0.5, test=0.5))
trainSet <- my_data[my_data_part$train$idx,]
testSet <- my_data[my_data_part$test$idx,]


## -------------------------------------------------------------------------------------------------------------------
predictors <- colnames(trainSet)[-1]
rmse_results <- c()
for (predictor in predictors) {
  formula <- as.formula(paste("LIFESAT_I ~", predictor))
  model <- lm(formula, data = trainSet)
  predictions <- predict(model, newdata = testSet)
  residuals <- testSet$LIFESAT_I - predictions
  rmse_value <- sqrt(mean(residuals^2))
  rmse_results <- append(rmse_results, rmse_value)
}
rmse_df <- data.frame(Predictor = predictors, RMSE = rmse_results)
rmse_df


## -------------------------------------------------------------------------------------------------------------------
single_predictor <- rmse_df$RMSE[rmse_df$Predictor == "KESSLER6_I"]


## -------------------------------------------------------------------------------------------------------------------
model <- lm(LIFESAT_I ~ KESSLER6_I, data=trainSet)


## -------------------------------------------------------------------------------------------------------------------
steps <- function(response, predictors, candidates, train, test)
{
  rhs <- paste0(paste0(predictors, collapse="+"), "+", candidates)
  formulas <- lapply(paste0(response, "~", rhs), as.formula)
  rmses <- sapply(formulas,
                  function(fm) rmse(lm(fm, data=train),
                                    data=test))
  names(rmses) <- candidates
  attr(rmses, "best") <- rmses[which.min(rmses)]
  rmses
}


## -------------------------------------------------------------------------------------------------------------------
preds <- "KESSLER6_I"
cands <- c("SOCIALWB_I", "NONAFFIRM_I", "NONDISCLOSURE_I", "HCTHREAT_I", 
           "EVERYDAY_I")
s1 <- steps("LIFESAT_I", preds, cands, trainSet, testSet)
model <- c(model, attr(s1, "best"))
s1


## -------------------------------------------------------------------------------------------------------------------
fit1 <- lm(LIFESAT_I ~ KESSLER6_I + SOCIALWB_I, data=trainSet)
double_predictor <- rmse(fit1, testSet)


## -------------------------------------------------------------------------------------------------------------------
preds <- c("KESSLER6_I", "SOCIALWB_I")
cands <- c("NONAFFIRM_I", "NONDISCLOSURE_I", "HCTHREAT_I", "EVERYDAY_I")
s1 <- steps("LIFESAT_I", preds, cands, trainSet, testSet)
model <- c(model, attr(s1, "best"))
s1


## -------------------------------------------------------------------------------------------------------------------
fit2 <- lm(LIFESAT_I ~ KESSLER6_I + SOCIALWB_I + EVERYDAY_I, data=trainSet)
triple_predictor <- rmse(fit2, testSet)


## -------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(lattice)

s1_df <- data.frame(Variable = character(), RMSE = numeric())
s1_df <- rbind(s1_df, data.frame(Variable = "Single Predictor", RMSE = single_predictor))
s1_df <- rbind(s1_df, data.frame(Variable = "Double Predictor", RMSE = double_predictor))
s1_df <- rbind(s1_df, data.frame(Variable = "Triple Predictor", RMSE = triple_predictor))

preds <- c("KESSLER6_I", "SOCIALWB_I", "EVERYDAY_I")

cands <- c("NONAFFIRM_I", "NONDISCLOSURE_I", "HCTHREAT_I")

steps <- function(response, predictors, candidates, train, test)
{
  rhs <- paste0(paste0(predictors, collapse="+"), "+", candidates)
  formulas <- lapply(paste0(response, "~", rhs), as.formula)
  rmses <- sapply(formulas,
                  function(fm) rmse(lm(fm, data=train),
                                    data=test))
  names(rmses) <- candidates
  attr(rmses, "best") <- rmses[which.min(rmses)]
  rmses
}

s1 <- steps("LIFESAT_I", preds, cands, trainSet, testSet)
s1_df <- rbind(s1_df, data.frame(Variable = names(s1), RMSE = as.numeric(s1)))

s1_df$Variable <- factor(s1_df$Variable, levels = s1_df$Variable)

ggplot(s1_df, aes(x = Variable, y = RMSE)) +
  geom_point() +
  geom_line(group = 1) +
  ggtitle("RMSE vs Predictors") +
  xlab("Predictors") +
  ylab("RMSE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1))

