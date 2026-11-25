#===============================================================================
# To create a master dataset from the cleaned files
#===============================================================================

#===============================================================================
# LOAD RDS FILES
#===============================================================================
library(tidyverse)
library(here)

cpi <- readRDS(
  here("data", "processed", "cpi_features.rds")
)

sector_wages <- readRDS(
  here("data", "processed", "sector_wages_features.rds")
)

employee_wages <- readRDS(
  here("data", "processed", "employee_wages_features.rds")
)

yoy <- readRDS(
  here("data", "processed", "yoy_features.rds")
)

food_prices <- readRDS(
  here("data", "processed", "food_features.rds")
)

#===============================================================================
# CLEAN THE COLUMN CHARACTERS & MERGE THE SECTOR & EMPLOYEE WAGES
#===============================================================================
sector_wages <- sector_wages %>%
  mutate(
    activity = str_squish(activity),
    sector = str_squish(sector)
  )

employee_wages <- employee_wages %>%
  mutate(
    activity = str_squish(activity),
    sector = str_squish(sector)
  )

# Master Wages Dataset
wages_master <- sector_wages %>%
  left_join(
    employee_wages, 
    by=c("year", "sector", "activity"),
    suffix = c("_sector", "_employee")
  )

# I create Useful Labor metrics
wages_master <- wages_master %>%
  mutate(
    estimated_employees = 
      earnings_sector/ earnings_employee
  )

#===============================================================================
# NORMALISE AND REORGANIZE THE CPI & YoY INFLATION METRICS
#===============================================================================
# 1. Create a national CPI Summary
cpi_summary <- cpi %>%
  group_by(year) %>%
  summarise(
    avg_cpi = mean(CPI_INDEX, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

# 2. Summarize the YoY inflation
yoy_summary <- yoy %>%
  group_by(year) %>%
  summarise(
    avg_inflation = mean(inflation_rate, na.rm=TRUE),
    .groups = "drop"
    ) %>%
  ungroup()

# 3. Merge CPI + YoY Inflation Datasets to a Master Dataset
macro_master <- cpi_summary %>%
  left_join(
    yoy_summary, by="year"
  )

#===============================================================================
# MERGE THE WAGES AND INFLATION DATASETS 
#===============================================================================
master_data <- wages_master %>%
  left_join(
    macro_master, by="year"
  )

#===============================================================================
# PURCHASING POWER METRICS
#===============================================================================
master_data <- master_data %>%
  mutate (
  real_wage = earnings_employee / (avg_cpi /100),

  purchasing_power_index = real_wage / max(real_wage, na.rm=TRUE) * 100
  )

#===============================================================================
# INCORPORATE GROWTH PARAMETERS
#===============================================================================
master_data <- master_data %>%
  group_by(sector, activity) %>%
  arrange(year) %>%
  
  mutate(
    wage_bill_growth = (
      earnings_sector / lag(earnings_sector) -1
    ) * 100,
    
    employee_wage_growth = (
      earnings_employee / lag(earnings_employee) -1
    ) * 100,
    
    # Ensure the "year" characters are read as numeric
    year = (as.integer(year))
    
  ) %>%
  ungroup()

#===============================================================================
# SAVE THE MASTER DATASET
#===============================================================================
write_csv(
  master_data,
  here("data", "processed", "master_data.csv")
)

saveRDS(
  master_data,
  here("data", "processed", "master_data.rds")
)