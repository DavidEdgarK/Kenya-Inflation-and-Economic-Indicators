# KENYA INFLATION ANALYSIS

## Project Objective and overview

This project investigates inflation trends in Kenya using Consumer Price Index (CPI), wage earnings, and food prices datasets.

The objective is to:

- Analyze historical inflation patterns.

- Compare inflation against wage growth

- Assess food price movements

- Build forecasting models

- Apply machine learning techniques to discover patterns

- Generate visualizations and reports

## Data Sources

This project uses multiple datasets from KNBS and WFP sources, including:

1.  Consumer Price Indexes (CPI)

2.  Food Prices (2006-2026)

3.  Average Wage Earnings per employee

4.  Average wage payments by Industry & Sector

The datasets are integrated to evaluate the inflation and cost-of-living trends in Kenya.

## Analysis Workflow

The project follows the workflow below:

1.  Data Import

2.  Data Cleaning

3.  Feature Engineering

4.  Dataset Merging

5.  Exploratory Data Analysis

6.  Inflation Analysis

7.  Food Price Analysis

8.  Wage Analysis

9.  Purchasing Power Analysis

10. Correlation Analysis

11. Market Basket Analysis

12. KMeans Clustering

13. Forecasting

14. Visualization

## Packages Used

- tidyverse

- dplyr

- readr

- readcl

- ggplot2

- lubridate

- forecast

- cluster

- plotly

- here

## Key Analyses

### Inflation Trends

Analysis of CPI movements through time

### Wage growth

Comparison of wage growth against inflation rates

### Food Price Analysis

Investigation of changes in food prices across periods

### Time Series Forecasting

Forecasting future inflation and price movements

### Machine Learning

- KMeans Clustering

- Market Basket Analysis

- Correlation Analysis

## Reproducibility

This project uses the **here()** package for managing file paths.

Example:

library(here)

cpi \<- read_csv(here("raw data", "consumer_price_index.csv"))

This will avoid issues occasioned by changing working directories.

## Running the Project

Run the scripts in the following order:

1.  data_import.R
2.  data_cleaning.R
3.  feature_engineering.R
4.  data_merging.R
5.  eda.R
6.  cpi_inflation_analysis.R
7.  food_price_analysis.R
8.  wage_analysis.R
9.  purchasing_power_analysis.R
10. correlation_analysis.R
11. market_basket_analysis.R
12. kmeans_clustering.R
13. forecasting.R
14. visualization.R

## Project Outputs

The project generates:

- Cleaned datasets

- Inflation indicators

- Correlation matrices

- Cluster profiles

- Forecasts

- Analytical visualizations

## Author

**David Edgar Kimani MSc** - Data Analyst & Scientist

Kenya

**Specializations:**

- R Analytics

- Python

- SQL

- MS Excel

- Machine Learning

- Data Engineering and Data Pipelines

- Business Intelligence

- Visualization
