## ----setup, include=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------
imputeNA <- function(data, use.mean = FALSE) {
  mode <- function(x) {
    unique_values <- unique(na.omit(x))
    return (unique_values[which.max(tabulate(match(x, unique_values)))])
  }
  for (col in colnames(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      data[[col]][is.na(data[[col]])] <- mode(data[[col]])
    } else if (is.numeric(data[[col]])) {
      if (use.mean) {
        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      } else {
        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      }
    }
  }
  return(data)
}


## -------------------------------------------------------------------------------------------------------------------
testdf <- data.frame (
  row.names = c("Jack", "Rosa", "Dawn", "Vicki", "Blake", "Guillermo"),
  age = c(24, 23, NA, 25, 32, 19),
  city = c("Harlem", NA, "Queens", "Brooklyn", "Brooklyn", NA),
  gpa = c(3.5, 3.6, 4.0, NA, 3.8, NA)
)

testdf


## -------------------------------------------------------------------------------------------------------------------
imputeNA(testdf)

## -------------------------------------------------------------------------------------------------------------------
imputeNA(testdf, use.mean = TRUE)


## -------------------------------------------------------------------------------------------------------------------
countNA <- function(data, byrow = FALSE) {
  if (byrow) {
    count_NA <- rowSums(is.na(data))
  } else {
    count_NA <- colSums(is.na(data))
  }
  return(count_NA)
}


## -------------------------------------------------------------------------------------------------------------------
testdf <- data.frame(
  row.names = c("Jack", "Rosa", "Dawn", "Vicki", "Blake", "Guillermo"),
  age = c(24, 23, NA, 25, 32, 19),
  city = c("Harlem", NA, "Queens", "Brooklyn", "Brooklyn", NA),
  gpa = c(3.5, 3.6, 4.0, NA, 3.8, NA)
)

testdf


## -------------------------------------------------------------------------------------------------------------------
countNA(testdf)

## -------------------------------------------------------------------------------------------------------------------
countNA(testdf, byrow = TRUE)


## -------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(fivethirtyeight)

police_killings_copy <- na.omit(police_killings)
ggplot(police_killings_copy) +
  aes(raceethnicity, fill = raceethnicity) +
  facet_wrap( ~ nat_bucket) +
  geom_bar() +
  ggtitle('Police killings based on Race & Income Quintile') +
  xlab('Race/Ethnicity') +
  ylab('Count of Americans Killed') +
  scale_fill_discrete(name = "Race/Ethnicity") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(fivethirtyeight)

ggplot(congress_age) +
  aes(factor(congress), age, color = chamber) +
  facet_wrap( ~ chamber) +
  geom_boxplot() +
  ggtitle('Age Distribution in US Congress by the Congress Chamber') +
  xlab('Congress Number (80-113)') +
  ylab('Age (in years)') +
  scale_fill_discrete(name = "Chamber") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")


## ----message = FALSE------------------------------------------------------------------------------------------------
library(tidyverse)
library(fivethirtyeight)

bechdel_copy <- na.omit(bechdel)
ggplot(bechdel_copy) +
  aes(budget_2013, intgross, color = binary) +
  geom_point() +
  geom_smooth() +
  ggtitle('Worldwide Gross in 2013 (US dollars) vs Movie budget in 2013 (US dollars)') +
  xlab("Movie budget in 2013 (US dollars)") +
  ylab("Worldwide gross in 2013 (US dollars)") +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  theme(plot.title = element_text(hjust = 0.5))

