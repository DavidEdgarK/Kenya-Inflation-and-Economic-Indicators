#===============================================================================
# EXPLORATORY DATA ANALYSIS
#===============================================================================

library(tidyverse)
library(here)
library(scales)

#===============================================================================
# LOAD RDS DATASETS
#===============================================================================

master_data <- readRDS(
  here("data", "processed", "master_data.rds")
)

food_data <- readRDS(
  here("data", "processed", "food_features.rds")
)

#===============================================================================
# 1. HOW DOES CPI CHANGE OVER TIME?
#===============================================================================

# Annual distinct CPI series
cpi_trend <- master_data %>%
  distinct(year, avg_cpi) %>%
  drop_na(avg_cpi) %>%
  arrange(year)

# Plot CPI trend line
plot_cpi <- ggplot(
  cpi_trend, aes(
    x = year,
    y = avg_cpi
  )
) +
  geom_line(
    linewidth = 1.2,
    color = "skyblue"
  ) +
  geom_point(
    size = 3, 
    color = "skyblue"
  ) +
  labs(
    title = "Average CPI Trend in Kenya",
    subtitle = "Consumer Price Index over Time",
    x= "Year",
    y = "Average CPI"
  ) + 
  theme_minimal()
plot_cpi

# Save plot
ggsave(
  here("plots", "cpi_trend.png"),
  plot_cpi, width = 10, height = 6
)

# CPI summary
cpi_trend %>%
  summarise(
    min_cpi = min(avg_cpi),
    mac_cpi = max(avg_cpi),
    mean_cpi = mean(avg_cpi)
  )

#===============================================================================
# 2. HOW DOES INFLATION EVOLVE OVER TIME?
#===============================================================================

inflation_trend <- master_data %>%
  distinct(year, avg_inflation) %>%
  drop_na(avg_inflation) %>%
  arrange(year)

# Line/trend line Plot
p_inflation <- ggplot(
  inflation_trend, 
  aes(
    x = year,
    y = avg_inflation
  ) 
) + 
  geom_line(
    linewidth = 1.2,
    color = "purple"
  ) +
  geom_point(
    size = 3,
    color = "purple"
  ) +
  labs(
    title = "Inflation Trend in Kenya",
    subtitle = "Average Year-on-Year Inflation"
  ) + 
  theme_minimal()
p_inflation

# Save
  ggsave(
    here("plots", "inflation_trend.png"),
    p_inflation, width=10, height=6
  )
  
# INFLATION DISTRIBUTION - To help illustrate inflation volatility
p_hist <- ggplot(
  inflation_trend,
  aes(avg_inflation)
) + geom_histogram(
  bins = 10,
  fill = "violet"
) + labs(
  title = "Distribution of Inflation Rates",
  x = "Inflation Rate",
  y = "Count"
) + theme_minimal()
p_hist

# Save Histogram
  ggsave(
    here("plots", "inflation_distribution.png"),
    p_hist, width=10, height=6
  )

# BOXPLOT -INFLATION
p_box <- ggplot(
  inflation_trend,
  aes(
    y = avg_inflation
  )
) + geom_boxplot(
  fill = "orange"
) +
  labs(
    title = "Inflation Distribution"
  ) + theme_minimal()
p_box

# Save boxplot
 ggsave(
   here("plots", "inflation_distribution_boxplot.png"),
   p_box, width=10, height=6
 )

#===============================================================================
# 3. HOW DO FOOD PRICES EVOLVE OVER TIME?
#===============================================================================
# a) The top 20 expensive food groups
p_food <- food_data %>%
   arrange(
     desc(average_price_KSH)
   ) %>%
   slice(1:20) %>%
   ggplot(
     aes(
       x = reorder(commodity, average_price_KSH),
       y = average_price_KSH,
       fill = pricetype
     )
   ) + 
   geom_col() +
   coord_flip() +
   labs(
     title = "Top 20 Most Expensive Food Commodities",
     x = "",
     y = "Average Prices (KES)"
   ) +
   theme_minimal()
p_food

  # Save png
  ggsave(
    here("plots", "top_20_food_prices.png"),
    p_food, width=12, height=8
  )  

# b) Retail vs Wholesale Price comparisons
p_pricetype <- food_data %>%
  ggplot(
    aes(
      pricetype, 
      average_price_KSH,
      fill = pricetype
    )
  ) + 
  geom_boxplot() +
  labs(
    title = "Retail vs Wholesale Prices"
  ) +
  theme_minimal()
p_pricetype
  
  #Save png
  ggsave(
    here("plots", "retail_wholesale_comparison.png"),
    p_pricetype, width=8, height=6
  )

# c) Purchasing Power
p_power <- master_data %>%
  filter(
    activity == "Total - all activities"
  ) %>%
  mutate(
    year = factor(year)
  ) %>%
  
  ggplot(
    aes(
      x = year,
      y = purchasing_power_index,
      color = sector,
      group = sector
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Purchasing Power Index over Time",
    x = "Year",
    y = "Purchasing Power Index"
  ) +
  theme_minimal()
p_power

  #save
  ggsave(
    here("plots", "purchasing_power_trend.png"),
    p_power, width=10, height=6
  )

 
 
 
 
 
 
 
 








  







