#=====================================================
# TO CHECK, CLEAN AND NORMALIZE UP THE DATA
#=====================================================

#=====================================================
# 1. CLEAN & NORMALIZE AVERAGE WAGES PER EMPLOYEE
#=====================================================
library(tidyverse)
library(here)
library(zoo)
library(janitor)

# Read the CSV
avg_wage_per_employee <- read.csv(
  here("data","raw", "Average Wage Earnings per Employee_CLEAN.csv")
  ) %>%
  
  # standardize column names
  clean_names() %>%
  
  # Remove duplicates
  distinct() %>%
  
  # Clean up the names in the Activity Column
  mutate(
    activity = activity %>%
      str_replace_all("^'+|'+$", "") %>% # remove leading/trailing apostrophes
      str_replace_all('"', "") %>% # Eliminate stray quotes
      str_replace_all("[–—]", "-") %>%
      str_replace_all("â€“", "-") %>%
      str_replace_all("â€”", "-") %>%
      str_squish() %>%
      str_trim()
  ) %>%
  
  # Find & Remove Sectors/Activities with ALL earnings missing
  group_by(sector, activity) %>%
  filter(!all(is.na(earnings))) %>%
  ungroup() %>%
  
  # Sort before interpolating the Earnings Column
  arrange(sector, activity, year) %>%
  
  # Fill the occasional missing values
  group_by(sector, activity) %>%
  mutate(
    earnings = na.approx(
      earnings, na.rm =FALSE, rule=2
    )
  ) %>%
  ungroup()

  # Save clean csv
  write_csv(
    avg_wage_per_employee,
    here("data","processed","avg_wage_per_employee.csv")
  )
  
  # Save RDS version
  saveRDS(
    avg_wage_per_employee,
    here("data","processed","avg_wage_per_employee.rds")
  )

#=====================================================
# 2. CLEAN & NORMALIZE WAGE PAYMENTS PER SECTOR
#=====================================================
library(tidyverse)
library(here)
library(zoo)
library(readxl)
  
avg_sector_wages <- read_excel(
  here("data","raw", "Wage Payments by Industry and Sector_CLEAN.xlsx")
  ) %>%
  
  # standardize column names
  clean_names() %>%

  # Remove Duplicates
  distinct() %>%
  
  # Clean Up and unify the Activity Column
  mutate(
    activity = activity %>%
      str_replace_all("^'+|'+$", "") %>% # remove leading/trailing apostrophes
      str_replace_all('"', "") %>% # Eliminate stray and embedded quotes
      str_replace_all("[–—]", "-") %>%
      str_squish() %>%
      str_trim()
    ) %>%
  
  #Find and remove the Sector & Activities where Earnings is missing
  group_by(sector, activity) %>%
  filter(!all(is.na(earnings))) %>%
  ungroup() %>%
  
  # Sort prior to interpolation of earnings
  arrange(sector, activity, year) %>%
  
  # Fill up missing Earnings 
  group_by(sector, activity) %>%
  mutate(
    earnings = na.approx(
      earnings, na.rm=FALSE, rule=2
    )
  ) %>%
  ungroup()
  
  # Save new & Clean CSV
  write_csv(
    avg_sector_wages,
    here(
      "data","processed","avg_sector_wages.csv"
    )
  )
  
  # Save RDS
  saveRDS(
    avg_sector_wages,
    here(
      "data","processed","avg_sector_wages.rds"
    )
  )

#=====================================================
# 3. CLEAN & NORMALIZE CONSUMER PRICE INDICES (CPI)
#=====================================================
library(tidyverse)
library(readxl)
library(here)
library(zoo)
library(janitor)
library(readr)

cpi_clean <- read_excel(
  here("data","raw", "Consumer Price Index by divison_CLEAN.xlsx")
  ) %>%
  
  # Drop Duplicates
  distinct() %>%
  
  # Drop unwanted columns
  select(-YearMonth, -Month_num, -Date) %>%
  
  # standardize column names
  clean_names() %>%
  
  # Clean and Normalize the Division column
  mutate(
    division = division %>%
      as.character() %>%
      str_replace_all("^'+|'+$", "") %>% # remove leading/trailing apostrophes
      str_replace_all('"', "") %>% # Eliminate stray and embedded double quotes
      str_replace_all("[–—]", "-") %>%
      str_squish() %>% # remove extra spaces
      str_trim() # trim leading and trailing spaces
  ) %>%
  
  # Remove corrupted rows
  filter(
    !(
      is.na(year) &
      is.na(month) &
      is.na(cpi_index)
    )
  ) %>%
  
  # Find and remove from Division with no usable CPI values
  group_by(division) %>%
  filter(
    !all(is.na(cpi_index))
  ) %>%
  ungroup() %>%
  
  # Sort
  arrange(
    division, year
  ) %>%
  
  # Fill up occasional missing CPI values
  group_by(division) %>%
  mutate(
    cpi_index = na.approx(
      cpi_index, na.rm=FALSE, rule=2
    )
  ) %>%
  ungroup() %>%
  
  # Write and Save CSV
  write_csv(
    cpi_clean,
    here(
      "data","processed","cpi_clean.csv"
    )
  )
  
  # Save RDS
  saveRDS(
    cpi_clean,
    here(
      "data","processed","cpi_clean.rds"
    )
  )

#=====================================================
# 4. CLEAN & NORMALIZE WFP FOOD PRICES FOR 2006-2026
#=====================================================
library(tidyverse)
library(readxl)
library(here)
library(janitor)

food_prices_clean <-read_excel(
  here("data","raw", "wfp_food_prices_ken_2006_2026.xlsx")
  ) %>%
  
  # Drop Columns
  select(-admin1, -latitude, -longitude, -commodity_id, -priceflag) %>%
  
  # standardize column names
  clean_names() %>%

  #Drop completely the duplicated rows
  distinct() %>%
  
  # Standardize the text character in each column
  mutate(
    across(
      where(is.character),
      ~ str_squish(str_trim(.x))
    ),
    # Ensure the date column is stored correctly as a Date
    date = as.Date(date),
      
      # Convert the prices columns to numeric
      price_in_ksh = as.numeric(price_in_ksh),
      usdprice = as.numeric(usdprice),
      
      # Standardize some categorical column values
      county = str_to_title(County),
      market = str_to_title(market),
      food_category = str_to_sentence(food_category),
      commodity = str_to_sentence(commodity),
      unit = str_to_upper(unit),
      pricetype = str_to_title(pricetype),
      currency = str_to_upper(currency),
  ) %>%
  
  # Drop and remove missing fields required for analysis
  drop_na(
    date,
    county,
    market,
    commodity,
    unit,
    pricetype,
    price_in_ksh,
    usdprice
  ) %>%
  
  # Filter and retain only positive food prices from the dataset
  # To guard against negative or zero local prices. Its a validation step
  filter(
    price_in_ksh >= 0,
    usdprice >= 0
  ) %>%
  
  # sort the records in a consistent fashion
  arrange(
    date, county, market, commodity, pricetype
  )

  # Write and Save CSV
  write_csv(
    food_prices_clean,
    here(
      "data","processed","food_prices_clean.csv"
    ),
    na = ""
  )

  # Save RDS
  saveRDS(
    food_prices_clean,
    here(
      "data","processed","food_prices_clean.rds"
    )
  )

#=====================================================
# 5. CLEAN & NORMALIZE YEAR-ON-YEAR INLFATION BY DIVISION
#=====================================================
library(tidyverse)
library(here)
library(zoo)
library(janitor)

yoy_clean <- read.csv(
  here("data","raw","Year on Year Inflation by division_CLEAN.csv")
  ) %>%
  
  # standardize column names
  clean_names() %>%

  # Remove duplicates
  distinct() %>%
  
  # Standardize the text character in each column
  mutate(
    date = as.Date(
      date, format = "%m/%d/%Y"
    ),
    
    # Standardize the characters in the division column
    division = division %>%
      str_replace_all("^'+|'+$", "") %>%
      str_replace_all('"', "") %>%
      str_squish() %>%
      str_trim()
  ) %>%
  
  filter(
    division != "#REF!"
  ) %>%
  
  # Drop and remove missing fields required for analysis
  drop_na(
    frequency,
    division,
    date,
    year,
    unit,
    inflation_rate
  ) %>%
  
  # Sort the data out
  arrange(
    division,
    date
  )

  # save to CSV
  write_csv(
    yoy_clean,
    here(
      "data", "processed", "yoy_clean.csv"
    )
  )
  
  # save an RDS file
  saveRDS(
    yoy_clean,
    here(
      "data", "processed", "yoy_clean.rds"
    )
  )