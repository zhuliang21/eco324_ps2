library(tidyverse)

# clear environment
rm(list=ls())

# set working directory
setwd("~/Projects/eco324_ps1")

# load the data
data <- read.csv("verboven_cars.csv")

# rename variables
data <- data %>%
  rename(year = ye,
         country = ma,
         population = pop,
         horsepower = hp,
         fuel = li,
         width = wi,
         height = he,
         weight = we,
         demographic = home,
         quantity = qu)

# assume the alpha is constant across all markets (note it is not the same with the one in the question)

alpha <- -0.03

# generate new variables
data <- data %>%
  mutate(market_size = population/4) %>%
  # calculate market share s_j
  mutate(s_j = quantity/(market_size)) %>%
  # calculate price = eurpr/1000
  mutate(price = eurpr) %>%
  # calculate Lerner index
  mutate(lerner_index = -1 / ( alpha * (1-s_j) * price))

index_mean_summary <- data %>%
  group_by(country, year) %>%
  summarize(mean_lerner_index = mean(lerner_index, na.rm = TRUE))
  
top5_brand <- data %>%
  group_by(brand) %>%
  summarize(total_quantity = sum(quantity, na.rm = TRUE)) %>%
  top_n(5, total_quantity) %>%
  # pull the name of the top 5 brands
  pull(brand)

print(top5_brand)

top5_index_summary <- data %>%
  filter(brand %in% top5_brand) %>%
  group_by(brand) %>%
  summarize(mean_lerner_index = mean(lerner_index, na.rm = TRUE))

data <- data %>%
  # calculate the estimated mc = p - (-1 / alpha * (1-s_j))
  mutate(mc = price - (-1 / alpha * (1-s_j)))
