# Tips for PS1

## Stata Beginer Guide

### Installation

- Option 1: Download Stata from the official website and install it on your computer (requires a purchased license).
- Option 2: Use Stata through a Virtual SINC Site
  - upload your data files to the virtual site (via Virtual SINC Site W11 Desktop, Upload Button)
  - run Stata in the browser
  - Safari is recommended for Mac users.

### Some important file types

- .do file: the script file that contains Stata commands.
- .dta file: the data file format used by Stata.
- .log file: a file that records the output of your Stata commands.

### Basic commands

Set a working directory

```stata
cd "~/your_directory"
```

Open a log file

```stata
log using "log_file.log", replace

* [your commands here]

log close
```

Import csv data file to Stata

```stata
import delimited "data.csv", clear
```

Rename a variable

```stata
rename old_name new_name
```

Generate a new variable

```stata
* Generate a new variable by existing variables
gen new_var = old_var1/100

* Generate a new variable by grouping existing variables
egen new_var = group(old_var)

* Generate a new variable by summing existing variables
egen new_var = sum(old_var)
```

Drop a variable

```stata
drop var_name
```

Run a regression

```stata
* Run a simple OLS regression
reg y x1 x2
```

Summary statistics

```stata
* Summary statistics for all variables
summarize

* Summary statistics for a specific variable
summarize var_name
```

Construct a set of variables and call them by *

```stata
* Construct a set of variables (IV)
IV1_fuel = ...
IV1_height = ...
IV1_weight = ...

* using IV1* to call all variables starting with IV1, e,g, 
summarize IV1*
```

### Install third party packages

For example, package `ivreghdfe` is for running IV regression with fixed effects. It is not included in the official Stata package. You can install it by running the following command in Stata:

```stata
* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")

* Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from("https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/")
```

Here is the GitHub repository for `ivreghdfe`: [ivreghdfe](https://github.com/sergiocorreia/ivreghdfe) in case you want to check.

### IV regression with fixed effects

```stata
* Run IV regression with fixed effects
ivreghdfe y (x1 = IV*) x2 x3, absorb(fe_var1, fe_var2, fe_var3)
```

it will run a regression of y on x1, x2, x3, with x1 being instrumented by all IVs starting with IV, and fixed effects of fe_var.

## R Beginner Guide

### R Installation

- Option 1: Download R and RStudio from the official websites and install them on your computer. (R is free and open-source).
- Option 2: Use R through a Virtual SINC Site
  - upload your data files to the virtual site (via Virtual SINC Site W11 Desktop, Upload Button)
  - run R in the browser
  - Safari is recommended for Mac users.

### Script file type

- .R file: the script file that contains R commands.
- .Rmd file: R Markdown file for dynamic documents.

### Enable Copilot in RStudio

1. Open RStudio.
2. Go to Tools > Global Options.
3. Go to Coplilot, enable GitHub Copilot, and sign in with GitHub account. You need to have a GitHub Copilot subscription to use it. (Free for students with verification).

### Install Tidyverse and use it for data manipulation

```r
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(tidyverse)
```

### Basic commands with Tidyverse

Set a working directory

```r
setwd("~/your_directory")
```

Open a log file

```r
sink("log_file.log")
```

Import csv data file to R

```r
data <- read.csv("data.csv")
```

Rename a variable

```r

data <- data %>%
  rename(new_name = old_name)
```

Select specific columns

```r
data <- data %>%
  select(column1, column2)
```

Generate a new variable

```r
data <- data %>%
  mutate(new_var = old_var1 / 100)
```

Drop a variable

```r
data <- data %>%
  select(-var_name)
```

Summary statistics

```r
# Summary statistics for all variables in dataframe 'data'
summary(data)
```

Run a regression

```r
# Run a simple OLS regression
model <- lm(y ~ x1 + x2, data = data)
```

### Install third party packages to support IV regression with multiple-level fixed effects

For example, package `fixest` is for running IV regression with fixed effects. You can install it by running the following command in R:

```r
# Install fixest if not already installed
if (!requireNamespace("fixest", quietly = TRUE)) {
  install.packages("fixest")
}
# Do you want to install from sources the package which needs compilation? (Yes/no/cancel), answer no
library(fixest)
```

### IV regression with multiple fixed effects

```r
# Run IV regression with fixed effects
model <- feols(y ~ x1 | fe_var1 + fe_var2 + fe_var3, (endogenous_variable ~ i v1 + iv2), data = data)

# Summary of the model
summary(model)
```
