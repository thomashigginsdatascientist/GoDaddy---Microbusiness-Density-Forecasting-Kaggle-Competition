

library(tidyverse)
library(Sherlock)
library(lubridate)
library(caret)


data <- read_csv("C:/Users/thigg/Documents/GoDaddy Kaggle Comp/train.csv")


sample_submission <- read_csv("C:/Users/thigg/Documents/GoDaddy Kaggle Comp/sample_submission.csv")


test <- read_csv("C:/Users/thigg/Documents/GoDaddy Kaggle Comp/test.csv")


census_starter <- read_csv("C:/Users/thigg/Documents/GoDaddy Kaggle Comp/census_starter.csv")


str(data)

str(census_starter)

data$year <- year(data$first_day_of_month)

namer <- colnames(census_starter)
namer <- namer[-6]

census_starter1 <- census_starter %>%
  pivot_longer(cols = any_of(namer))

census_starter1$year <- gsub("*._", "", census_starter1$name)
census_starter1$year <- parse_number(census_starter1$year)
census_starter1$name <- gsub('[[:digit:]]+', '', census_starter1$name)

census_starter2 <- census_starter1 %>%
  pivot_wider(id_cols = c("cfips", "year"))


data <- left_join(data, census_starter2, by = c("cfips", "year"))

Sherlock::na_finder(data)

data1 <- data[complete.cases(data),]

data1 <- as.data.frame(data1)

cfips <- unique(data1$cfips)

length(cfips)

storage <- test[1,]
storage$pred <- 0
storage <- storage[-1,]

for(i in 1:length(cfips)){
  
  focus <- cfips[i]
  
  train <- data1 %>%
    filter(cfips == focus)
  
  test1 <- test %>%
    filter(cfips == focus)
  
  
  model1 <- train(microbusiness_density ~ first_day_of_month, data = train, method = "lm")
  
  test1$pred <- predict(model1, newdata = test1)
  
  storage <- rbind(storage, test1)
  
  print(paste0("on to the next one! So far we have completed ", i, " models!"))
  
}
