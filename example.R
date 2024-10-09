# Install the tidyverse package
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

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

# generate new variables
data <- data %>%
  mutate(market_size = population/4) %>%
  # calculate market share s_j
  mutate(s_j = quantity/(market_size)) %>%
  # calculate the outside option market share s_0
  # which is 1 - sum(s_j) by year and country
  group_by(year, country) %>%
  mutate(s_0 = 1 - sum(s_j)) %>%
  ungroup() %>%
  # calculate y = log(s_j) - log(s_0)
  mutate(y = log(s_j) - log(s_0)) %>%
  # calculate ln_pop and ln_gdp
  mutate(ln_pop = log(population),
         ln_gdp = log(ngdp)) %>%
  # calculate price = eurpr/1000
  mutate(price = eurpr/1000)

# Install the `fixest` package for fixed effects regression
if (!requireNamespace("fixest", quietly = TRUE)) {
  install.packages("fixest")
}

library(fixest)

# run a fixed effect regression by fixest package
# fixed effects comes from year, country, and brand
# y is the dependent variable, 
# price, horsepower, width, height, weight, fuel, demographic, ln_pop, ln_gdp are 
# the independent variables

fe_reg_a <- feols(y ~ price + horsepower + width + height + weight + fuel + demographic + ln_pop + ln_gdp | year + country + brand, data = data)

# print the regression results
summary(fe_reg_a)

# run a similar regression as above but with the hetergeneity price coefficient across countries (add a price*country interaction term)

fe_reg_b <- feols(y ~ price + horsepower + width + height + weight + fuel + demographic + ln_pop + ln_gdp + price:country | year + country + brand, data = data)

# print the regression results
summary(fe_reg_b)

# Test whether the price coefficients are the same across countries.
# Test if price:countryFrance = price:countryGermany = price:countryItaly = price:countryUK = 0

# install the `car` package for the linear hypothesis test
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

library(car)

# run the linear hypothesis test
linearHypothesis(fe_reg_b, c("price:countryFrance = 0", "price:countryGermany = 0", "price:countryItaly = 0", "price:countryUK = 0"))

# Construct the IV set 
data_with_iv <- data %>%
  group_by(country, year) %>%
  mutate(IV_horsepower = horsepower - (sum(horsepower) - horsepower)/(n() - 1),
         IV_width = width - (sum(width) - width)/(n() - 1),
         IV_fuel = fuel - (sum(fuel) - fuel)/(n() - 1),
         IV_height = height - (sum(height) - height)/(n() - 1),
         IV_weight = weight - (sum(weight) - weight)/(n() - 1),
         IV_demographic = demographic - (sum(demographic) - demographic)/(n() - 1),
  )

# summary statistics of the IV set (new variables)
data_with_iv %>% 
  select(IV_horsepower, IV_width, IV_fuel, IV_height, IV_weight, IV_demographic) %>%
  summary()

# run a fixed effect regression with the IV set (price is the endogenous,
# price ~ IV_horsepower + IV_width + IV_fuel + IV_height + IV_weight + IV_demographic) by ivfeols
iv_fe_reg_c <- feols(y ~ horsepower + width + height + weight + fuel + demographic + ln_pop + ln_gdp | year + country + brand | price ~ IV_horsepower + IV_width + IV_fuel + IV_height + IV_weight + IV_demographic, data = data_with_iv)

# print the regression results
summary(iv_fe_reg_c)