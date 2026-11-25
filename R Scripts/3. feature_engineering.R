#===================================================================================
# Goal: Create data variables from the cleaned datasets to provide more insight
# These features will be used to gain more insights and trends from the datasets
#===================================================================================

#===============================================================================
# LOAD THE RDS (CLEANED) FILES
#===============================================================================
library(here)
library(tidyverse)

cpi <- readRDS(
  here("data", "processed", "cpi_clean.rds")
  )

avg_sector_wages <- readRDS(
  here("data", "processed","avg_sector_wages.rds")
  )

avg_wage_per_employee <- readRDS(
  here("data", "processed","avg_wage_per_employee.rds")
  )

food_prices <- readRDS(
  here("data", "processed", "food_prices_clean.rds")
  )

yoy <- readRDS(
  here("data", "processed", "yoy_clean.rds")
  )

#===============================================================================
# CPI FEATURES
#===============================================================================
# To generate the Month-to-Month CPI Growth Rate

cpi_features <- cpi %>%
  mutate(
    year = Year,
    decade = floor(year /10) * 10,
    
    inflation_period = 
      case_when(
        Year < 2020 ~ "Pre-COVID",
        Year <= 2022 ~ "COVID/Post-COVID",
        TRUE ~ "Recent"
      )
  )

cpi_features <- cpi_features %>%
  group_by(Division) %>%
  arrange(Year) %>%
  mutate(
    monthly_change = ((CPI_INDEX / lag(CPI_INDEX)) - 1) * 100
  ) %>%
  ungroup() 
#===============================================================================
# SECTOR WAGES FEATURES
#===============================================================================
# To get the annual Wage Growth

sector_wages_features <- avg_sector_wages %>%
  group_by(sector, activity) %>%
  arrange(year) %>%
  mutate(
    wage_growth = ((earnings / lag(earnings)) -1) * 100
  ) %>%
  ungroup()

sector_wages_features <- sector_wages_features %>%
  mutate(
    log_sector_earnings = log(earnings)
  )
#===============================================================================
# WAGES PER EMPLOYEE FEATURES
#===============================================================================
# To get the annual wage growth per employee

employee_wages_features <- avg_wage_per_employee %>%
  group_by(sector, activity) %>%
  arrange(year) %>%
  mutate(
    wage_growth = ((earnings / lag(earnings)) -1) * 100
  ) %>%
  ungroup()

employee_wages_features <- employee_wages_features %>%
  mutate(
    log_emp_earnings = log(earnings)
  )
#===============================================================================
# FOOD PRICES FEATURES
#===============================================================================
food_features <- food_prices %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = quarter(date)
  )

food_features <- food_features %>%
  group_by(commodity, market) %>%
  arrange(date) %>%
  mutate(
    price_pct_change = 
      ((price_in_ksh / lag(price_in_ksh)) -1) * 100
  ) %>%
  ungroup()

food_features <- food_features %>%
  group_by(commodity, pricetype) %>%
  arrange(date, .by_group = TRUE) %>%
  summarise(
    total_price_KSH = sum(price_in_ksh, na.rm=TRUE),
    average_price_KSH = mean(price_in_ksh, na.rm=TRUE),
    max_price_KSH = max(price_in_ksh, na.rm=TRUE),
    min_price_KSH = min(price_in_ksh, na.rm=TRUE),
    observations = n(),
    .groups = "drop"
  ) %>%
  ungroup()

#===============================================================================
# YEAR-ON-YEAR (YoY) INFLATION FEATURES
#===============================================================================
yoy_features <- yoy %>%
  mutate(
    inflation_band = 
      case_when(
        inflation_rate < 3 ~ "Low",
        inflation_rate < 7 ~ "Moderate",
        inflation_rate < 10 ~ "High",
        TRUE ~ "Severe"
      )
  )

# Add some rolling features - for forecasting purposes
# For a 3-Month Moving Average
library(zoo)

yoy_features <- yoy_features %>%
  group_by(division) %>%
  arrange(date) %>%
  mutate(
    inflation_ma3 = rollmean(
      inflation_rate, 3, fill=NA, align="right"
    )
  ) %>%
  ungroup()

# lag features - for ML (rolling information, hence why I use lag)
yoy_features <- yoy_features %>%
  group_by(division) %>%
  arrange(date) %>%
  mutate(
    lag1_inflation = lag(inflation_rate, 1),
    
    lag3_inflation = lag(inflation_rate, 3),
  ) %>% 
  ungroup()

#===============================================================================
# SAVE FEATURES TO RDS FILES
#===============================================================================
saveRDS(
  cpi_features,
  here("data", "processed", "cpi_features.rds")
)

saveRDS(
  sector_wages_features,
  here("data", "processed", "sector_wages_features.rds")
)

saveRDS(
  employee_wages_features,
  here("data","processed","employee_wages_features.rds")
)

saveRDS(
  food_features,
  here("data", "processed","food_features.rds")
)

write.csv(
  food_features,
  here("data", "processed", "food_features.csv")
)

saveRDS(
  yoy_features,
  here("data","processed","yoy_features.rds")
)



