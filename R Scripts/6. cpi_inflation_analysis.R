#===============================================================================
# CPI & INFLATION ANALYSIS
#===============================================================================

library(tidyverse)
library(here)

#===============================================================================
# LOAD RDS DATASETS
#===============================================================================

cpi_data <- readRDS(
  here("data", "processed", "cpi_features.rds")
)
head(cpi_data)
tail(cpi_data)

#===============================================================================
# 1. Which CPI categories grew the fastest?
#===============================================================================
cpi_growth <- cpi_data %>%
  group_by(Division) %>%
  arrange(Year, Month, .by_group=TRUE) %>%
  summarise(
    first_cpi = first(CPI_INDEX),
    last_cpi = last(CPI_INDEX),
    total_growth = (
      last_cpi / first_cpi - 1
    ) * 100,
    .groups = "drop"
  ) %>%
  arrange(
    desc(total_growth)
  )

# Plot the fastest growing CPI categories
p_cpi_growth <- cpi_growth %>%
  slice_max(
    total_growth,
    n = 15
  ) %>%
  # Plot
  ggplot(
    aes(
      x = reorder(Division, total_growth),
      y = total_growth
    )
  ) + 
  geom_col(
    fill = "purple"
  ) +
  geom_text(
    aes(label = round(total_growth, 1)),
    hjust = -0.2
  ) +
  coord_flip() +
  labs(
    title = "Top CPI Categories by Growth",
    x = "",
    y = "Growth (%)"
  ) +
  theme_minimal()
  
  # Save plot
  ggsave(
   here("plots", "cpi_fastest_growth.png"),
   p_cpi_growth, width = 12, height = 8
  )

#===============================================================================
# 2. Which Division is driving Inflation the most?
#===============================================================================
inflation_driver <- cpi_data %>%
    group_by(Division) %>%
    summarise(
      avg_cpi = mean(CPI_INDEX, na.rm=TRUE),
      avg_growth = mean(monthly_change, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    arrange(avg_cpi)

# Plot the Scatter plot
p_driver <- ggplot(
  inflation_driver,
  aes(
    x = avg_cpi,
    y = avg_growth
  )
) +
  geom_line(
   aes(group=1),
   color = "darkblue",
   linewidth = 1
  )  +
  geom_point(
    size=4,
    color="darkgreen"
  ) +
  geom_text(
    aes(label = Division),
    hjust = -0.1,
    vjust = -0.5,
    size = 3
  ) + 
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  labs(
    title = "Inflation Driver Analysis",
    x = "Average CPI",
    y = "Average CPI Growth (%)"
  ) +
  theme_minimal()

  # Save Plot
  ggsave(
    here("plots", "inflation_driver_scatter.png"),
    p_driver, width=12, height=8, dpi=300
  )
  
# HEAT MAP
p_heatmap <- cpi_data %>%
  ggplot(
    aes(
      x = factor(Year),
      y = Division,
      fill = CPI_INDEX
    )
  ) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "CPI Heatmap by Division",
    x = "Year",
    y = "Division"
  ) +
  theme_minimal()

  # Save
  ggsave(
    here("plots", "cpi_heatmap.png"),
    p_heatmap, width = 14, height=8
  )
  
# Ranking Table
inflation_driver %>%
  arrange(
    desc(avg_growth)
  ) %>%
  write.csv(
    here("outputs", "top_inflation_drivers.csv")
  )