library(here)
library(readxl)
list.files("../raw data")

#=================================================================================================
# This file will ONLY Import & read the files
#=================================================================================================

#================================================================================================
# 1. IMPORT AVERAGE WAGES PER EMPLOYEE
#================================================================================================
library(here)
avg_wage_per_employee <- read.csv(here("data","raw", "Average Wage Earnings per Employee_CLEAN.csv"))
head(avg_wage_per_employee)
summary(avg_wage_per_employee)

#=================================================================================================
# 2. IMPORT WAGE PAYMENTS PER SECTOR
#=================================================================================================
library(readxl)
library(here)
sector_wages <- read_excel(here("data", "raw", "Wage Payments by Industry and Sector_CLEAN.xlsx"))
head(sector_wages)
summary(sector_wages)

#=================================================================================================
# 3. IMPORT CONSUMER PRICE INDICES
#=================================================================================================
library(readxl)
library(here)
library(janitor)

cpi <- read_excel(here("data","raw", "Consumer Price Index by divison_CLEAN.xlsx"))
#head(cpi)
#summary(cpi)
glimpse(cpi)
names(cpi)

#=================================================================================================
# 4. IMPORT WFP FOOD PRICES FOR 2006-2026
#=================================================================================================
library(readxl)
library(here)
food_prices <-read_excel (here("data","raw","wfp_food_prices_ken_2006_2026.xlsx"))
head(food_prices)
summary(food_prices)

#=================================================================================================
# 5. IMPORT YEAR-ON-YEAR INLFATION BY DIVISION
#=================================================================================================
library(here)
yoy <- read.csv(here("data","raw","Year on Year Inflation by division_CLEAN.csv"))
head(yoy)
summary(yoy)




