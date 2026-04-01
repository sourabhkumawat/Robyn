# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#############################################################################################
####################         Meta MMM Open Source: Robyn 3.12.1       #######################
####################         Quick Commerce Demo Guide (India)        #######################
#############################################################################################

# Advanced marketing mix modeling using Meta Open Source project Robyn (Blueprint training)
# https://www.facebookblueprint.com/student/path/253121-marketing-mix-models?utm_source=demo

# QUICK COMMERCE SPECIFIC:
# This demo is optimized for Indian quick commerce platforms (Blinkit, Zepto, Swiggy Instamart)
# Features: Lower theta ranges, 6-year festival calendar (2023-2028), context-aware transformations

################################################################
#### Step 0: Setup environment

## Install, load, and check (latest) Robyn version, using one of these 2 sources:
## A) Install the latest stable version from CRAN:
# install.packages("Robyn")
## B) Install the latest dev version from GitHub:
# install.packages("remotes") # Install remotes first if you haven't already
# remotes::install_github("facebookexperimental/Robyn/R")
library(Robyn)

# Load Quick Commerce functions
source("R/R/qcommerce.R")  # Load QC-optimized functions

# Please, check if you have installed the latest version before running this demo. Update if not
# https://github.com/facebookexperimental/Robyn/blob/main/R/DESCRIPTION#L4
packageVersion("Robyn")

## Force multi-core use when running RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

## IMPORTANT: Must install and setup the python library "Nevergrad" once before using Robyn
## Guide: https://github.com/facebookexperimental/Robyn/blob/main/demo/install_nevergrad.R

################################################################
#### Step 1: Load Quick Commerce data

## QUICK COMMERCE: Create realistic Indian QC dataset or load your own
set.seed(123)
n_days <- 730  # 2 years of daily data
dates <- seq.Date(from = as.Date("2025-01-01"), by = "day", length.out = n_days)

# Generate Quick Commerce optimized dataset
generate_qc_dataset <- function(n_days, dates) {

  # Base media spends (Indian quick commerce channels)
  facebook_performance <- pmax(0, rnorm(n_days, 25000, 8000))  # Performance campaigns
  facebook_brand <- pmax(0, rnorm(n_days, 15000, 5000))       # Brand awareness
  google_search <- pmax(0, rnorm(n_days, 20000, 6000))        # Search ads
  google_display <- pmax(0, rnorm(n_days, 8000, 3000))        # Display campaigns
  app_campaigns <- pmax(0, rnorm(n_days, 12000, 4000))        # App install/engagement
  social_influencer <- pmax(0, rnorm(n_days, 6000, 2500))     # Influencer marketing

  # Context variables
  competitor_index <- pmax(0, rnorm(n_days, 100, 20))         # Competitor activity
  weather_index <- pmax(0, rnorm(n_days, 75, 25))            # Weather conditions
  discount_events <- rbinom(n_days, 1, 0.15)                  # Discount events (15% of days)

  # Organic variables
  push_notifications <- rpois(n_days, 50000)                  # Daily push sends
  email_campaigns <- rpois(n_days, 25000)                     # Email sends

  # Add day-of-week effects (QC pattern: higher weekends)
  dow_multiplier <- ifelse(weekdays(dates) %in% c("Saturday", "Sunday"), 1.3, 0.95)
  facebook_performance <- facebook_performance * dow_multiplier * 0.9  # Performance less weekend effect
  facebook_brand <- facebook_brand * dow_multiplier * 1.1             # Brand more weekend effect
  google_search <- google_search * dow_multiplier * 0.95
  google_display <- google_display * dow_multiplier * 1.05
  app_campaigns <- app_campaigns * dow_multiplier * 1.2               # App highest weekend effect
  social_influencer <- social_influencer * dow_multiplier * 1.1

  # Add festival effects (comprehensive Indian calendar)
  festival_boost <- numeric(n_days)
  for (i in 1:n_days) {
    fest_mult <- robyn_qcommerce_festival_multiplier(dates[i], "performance")
    festival_boost[i] <- fest_mult
  }

  # Apply festival effects to media (different channels react differently)
  facebook_performance <- facebook_performance * festival_boost * 1.1   # Performance gets extra boost
  facebook_brand <- facebook_brand * festival_boost                     # Brand standard boost
  google_search <- google_search * festival_boost * 1.1                 # Search gets extra boost
  google_display <- google_display * festival_boost
  app_campaigns <- app_campaigns * festival_boost * 1.15                # App campaigns highest boost
  social_influencer <- social_influencer * festival_boost * 1.05        # Social moderate boost

  # Generate orders with QC-specific response patterns
  orders <- numeric(n_days)
  orders[1] <- 2500 +
    0.08 * facebook_performance[1] +
    0.05 * facebook_brand[1] +
    0.10 * google_search[1] +
    0.06 * google_display[1] +
    0.12 * app_campaigns[1] +
    0.09 * social_influencer[1] +
    0.001 * push_notifications[1] +
    0.002 * email_campaigns[1] +
    50 * discount_events[1]

  for (i in 2:n_days) {
    # Base order generation
    base_orders <- 2500 +
      0.08 * facebook_performance[i] +  # Brand campaigns - lower immediate conversion
      0.05 * facebook_brand[i] +        # Brand awareness - lowest immediate
      0.10 * google_search[i] +         # Search - highest immediate conversion
      0.06 * google_display[i] +        # Display - moderate conversion
      0.12 * app_campaigns[i] +         # App - high conversion (in-app)
      0.09 * social_influencer[i] +     # Social - good conversion
      0.001 * push_notifications[i] +   # Push notifications effect
      0.002 * email_campaigns[i] +      # Email effect
      50 * discount_events[i]           # Discount boost

    # Add carryover effect (lower theta for QC = 0.2 avg)
    carryover <- 0.2 * (orders[i-1] - 2500)

    # Context effects
    context_effect <- 0.5 * competitor_index[i] + 0.3 * weather_index[i]

    # Add noise and ensure positive
    orders[i] <- pmax(500, base_orders + carryover + context_effect + rnorm(1, 0, 200))
  }

  # Create dataset in Robyn format
  dt_qc <- data.frame(
    DATE = dates,
    orders = round(orders),
    # Paid media spends
    facebook_perf_S = facebook_performance,
    facebook_brand_S = facebook_brand,
    google_search_S = google_search,
    google_display_S = google_display,
    app_campaigns_S = app_campaigns,
    social_influencer_S = social_influencer,
    # Context variables
    competitor_index = competitor_index,
    weather_index = weather_index,
    discount_events = discount_events,
    # Organic variables
    push_notifications = push_notifications,
    email_campaigns = email_campaigns
  )

  return(dt_qc)
}

# Generate the dataset
dt_qc_simulated <- generate_qc_dataset(n_days, dates)
head(dt_qc_simulated)

## Check Quick Commerce holidays (India-specific)
# Load standard Prophet holidays for India
data("dt_prophet_holidays")
dt_qc_holidays <- dt_prophet_holidays[dt_prophet_holidays$country == "IN", ]
head(dt_qc_holidays)

# Directory where you want to export results to (will create new folders)
qc_directory <- "~/Desktop/QuickCommerce_Robyn"

################################################################
#### Step 2a: For first time user: Quick Commerce Model specification in 4 steps

#### 2a-1: First, specify input variables with QC optimizations

## For documentation of all arguments run ?robyn_inputs
## QUICK COMMERCE: Standard Robyn functions now support QC parameters directly!

## Method 1: RECOMMENDED - Use standard robyn_inputs() with QC parameters
InputCollect <- robyn_inputs(
  dt_input = dt_qc_simulated,
  dt_holidays = dt_qc_holidays,
  date_var = "DATE",
  dep_var = "orders",
  dep_var_type = "conversion",
  prophet_vars = c("trend", "season", "weekday", "holiday"),
  prophet_country = "IN",
  context_vars = c("competitor_index", "weather_index", "discount_events"),
  paid_media_spends = c("facebook_perf_S", "facebook_brand_S", "google_search_S",
                       "google_display_S", "app_campaigns_S", "social_influencer_S"),
  organic_vars = c("push_notifications", "email_campaigns"),
  factor_vars = c("discount_events"),
  window_start = "2025-01-01",
  window_end = "2026-12-31",
  adstock = "geometric",
  # QUICK COMMERCE: Add QC parameters for automatic optimization
  channel_types = c("performance", "brand", "performance", "brand", "app", "social"),
  city_tier = "tier1",        # Mumbai/Delhi/Bangalore patterns
  context_aware = TRUE,       # Enable festival/day-of-week adjustments
  qc_region = "india"         # 6-year festival calendar (2023-2028)
)
print(InputCollect)

#### 2a-2: Second, define and add Quick Commerce optimized hyperparameters

## QUICK COMMERCE: Get QC-specific hyperparameter names
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

## QUICK COMMERCE: Understand QC-optimized hyperparameters

## QC Hyperparameter Philosophy:
## - LOWER THETA RANGES: Quick commerce has shorter attribution windows (10-30 min delivery)
## - CHANNEL-TYPE BASED: Classification by advertising strategy, not product category
## - CONTEXT-AWARE: Festival, day-of-week, city tier, weather adjustments built-in
## - INDIA-OPTIMIZED: 6-year festival calendar (2023-2028), no updates needed until 2029

## 1. IMPORTANT: set plot = TRUE to create example plots for adstock & saturation
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

## 2. Quick Commerce Theta Recommendations vs Standard Robyn:
## Standard Robyn: TV c(0.3, 0.8), Digital c(0, 0.3), OOH/Print c(0.1, 0.4)
## Quick Commerce: Performance c(0.10, 0.25), Brand c(0.15, 0.35), App c(0.08, 0.20)

## 3. QUICK COMMERCE: Check our channel-type bounds
robyn_qcommerce_bounds("performance")  # For search, performance campaigns
robyn_qcommerce_bounds("brand")        # For brand awareness campaigns
robyn_qcommerce_bounds("app")          # For app campaigns
robyn_qcommerce_bounds("social")       # For social/influencer marketing

## 4. QUICK COMMERCE: Hyperparameters automatically generated!
## When channel_types are provided, robyn_inputs() auto-generates QC-optimized hyperparameters
hyper_limits()

print("Quick Commerce hyperparameters were automatically generated based on channel_types!")
print("No manual hyperparameter setup required - QC optimization is built-in!")

#### 2a-3 (Optional) Quick Commerce Saturation calibration

## QUICK COMMERCE: Use reach & frequency data for calibration if available
## For Indian QC platforms, focus on mobile reach curves

# data("df_curve_reach_freq")
# curve_out <- robyn_calibrate(
#   df_curve = df_curve_reach_freq,
#   curve_type = "saturation_reach"
# )
# curve_out$plot_reach_freq
# facebook_perf_S_gammas <- c(
#   curve_out[["curve_collect"]][["reach 1+"]][["hill"]][["gamma_best"]],
#   curve_out[["curve_collect"]][["reach 10+"]][["hill"]][["gamma_best"]])
# qc_hyperparameters$facebook_perf_S_gammas <- facebook_perf_S_gammas

#### 2a-4: QUICK COMMERCE - Hyperparameters already added automatically!
## Since we provided channel_types, hyperparameters were auto-generated
print(InputCollect)
print("QC hyperparameters auto-generated and stored in InputCollect!")

#### 2a-5: (Optional) Quick Commerce Effect size calibration

## QUICK COMMERCE: Calibration recommendations for Indian market
## Use geo-based experiments (Swiggy GeoLift style) or conversion lift studies

# calibration_input <- data.frame(
#   # QUICK COMMERCE: Typical channels for calibration
#   channel = c("facebook_perf_S", "google_search_S", "app_campaigns_S"),
#   # Use recent festival periods for calibration (higher signal)
#   liftStartDate = as.Date(c("2025-10-20", "2025-11-01", "2025-11-10")),  # Diwali 2025
#   liftEndDate = as.Date(c("2025-11-07", "2025-11-15", "2025-11-20")),
#   # QC typical lift: orders per dollar spent during festivals
#   liftAbs = c(2500, 3200, 1800),  # Incremental orders
#   # Spend during experiment period
#   spend = c(45000, 38000, 22000),
#   # Confidence from experiment
#   confidence = c(0.85, 0.9, 0.8),
#   # Metric: conversion (orders) for QC
#   metric = c("conversion", "conversion", "conversion"),
#   # QC recommendation: use "immediate" for festival experiments
#   calibration_scope = c("immediate", "immediate", "immediate")
# )
# InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input)

################################################################
#### Step 2b: For known QC model specification, setup in one single step

## QUICK COMMERCE: Single-step setup with all QC optimizations
# Alternative: Single-step setup (same as Method 1)
# InputCollect <- robyn_inputs(
#   dt_input = dt_qc_simulated,
#   dt_holidays = dt_qc_holidays,
#   date_var = "DATE",
#   dep_var = "orders",
#   dep_var_type = "conversion",
#   prophet_vars = c("trend", "season", "weekday", "holiday"),
#   prophet_country = "IN",
#   context_vars = c("competitor_index", "weather_index", "discount_events"),
#   paid_media_spends = c("facebook_perf_S", "facebook_brand_S", "google_search_S",
#                        "google_display_S", "app_campaigns_S", "social_influencer_S"),
#   organic_vars = c("push_notifications", "email_campaigns"),
#   factor_vars = c("discount_events"),
#   window_start = "2025-01-01",
#   window_end = "2026-12-31",
#   adstock = "geometric",
#   # QC parameters for automatic optimization
#   channel_types = c("performance", "brand", "performance", "brand", "app", "social"),
#   city_tier = "tier1",
#   context_aware = TRUE,
#   qc_region = "india"
# )

################################################################
#### Step 3: Run Quick Commerce optimized models

## QUICK COMMERCE: Standard robyn_run() - no changes needed!
OutputModels <- robyn_run(
  InputCollect = InputCollect,
  cores = 2,
  iterations = 2000, # 2000 for production, 500 for testing
  trials = 5,
  outputs = FALSE
)

## QUICK COMMERCE: Standard robyn_outputs() with built-in QC validation!
OutputCollect <- robyn_outputs(
  InputCollect = InputCollect,
  OutputModels = OutputModels,
  pareto_fronts = "auto",
  calibration_constraint = 0.1,
  csv_out = "pareto",
  clusters = TRUE,
  export = TRUE,
  plot_folder = qc_directory,
  plot_pareto = TRUE
)

print(OutputCollect)

## QUICK COMMERCE: Validation results automatically included in OutputCollect!
## Check OutputCollect$qcommerce_validation for detailed QC validation results
if(!is.null(OutputCollect$qcommerce_validation)) {
  qc_validation <- OutputCollect$qcommerce_validation
  print("=== QUICK COMMERCE VALIDATION RESULTS ===")
  print(qc_validation$validation_summary)
  print("Full validation details stored in OutputCollect$qcommerce_validation")
}

## 4 csv files are exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R

################################################################
#### Step 4: Select and save the Quick Commerce model

## Compare all model one-pagers and select one that reflects QC business reality
print(OutputCollect)
select_model <- OutputCollect$allSolutions[1] # Pick first model or specify one

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model,
                            export = TRUE, dir = qc_directory)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

################################################################
#### Step 5: Get Quick Commerce budget allocation

## QUICK COMMERCE: Budget allocation for Indian QC platforms
## Considerations: Festival season planning, city-tier expansion, channel efficiency

# Check media summary for selected model
print(ExportedModel)

# NOTE: The order of constraints should follow:
InputCollect$paid_media_selected

# QUICK COMMERCE Scenario 1: "Festival Season Optimization"
# "What's the max orders during upcoming Diwali season?"
QC_AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_30", # Last 30 days as baseline
  channel_constr_low = 0.7,
  channel_constr_up = c(1.5, 1.2, 2.0, 1.3, 2.5, 1.8), # QC channels: higher app/social potential
  scenario = "max_response"
)
print(QC_AllocatorCollect1)
plot(QC_AllocatorCollect1)

# QUICK COMMERCE Scenario 2: "City Expansion Budget"
# Simulate budget for expanding to Tier 2 cities
QC_AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_60",
  total_budget = 2000000, # 20L budget for expansion
  channel_constr_low = c(0.8, 0.5, 0.9, 0.6, 1.2, 1.1), # Favor performance & app for new cities
  channel_constr_up = c(1.5, 1.0, 2.0, 1.2, 3.0, 2.0),
  channel_constr_multiplier = 4,
  scenario = "max_response"
)
print(QC_AllocatorCollect2)
plot(QC_AllocatorCollect2)

# QUICK COMMERCE Scenario 3: "Target Efficiency for Profitability"
# "How much to spend to hit target CPA (Cost Per Acquisition)?"
QC_AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "target_efficiency",
  target_value = 25 # Target CPA of 25 rupees per order
)
print(QC_AllocatorCollect3)
plot(QC_AllocatorCollect3)

################################################################
#### Step 6: Quick Commerce Model refresh for ongoing optimization

## QUICK COMMERCE: Weekly/monthly refresh for fast-moving QC business
## Suitable for updating with new festival data, competitive changes, etc.

# Export model first for refresh
json_file_qc = file.path(qc_directory, paste0("QC_RobynModel-", select_model, ".json"))
robyn_write(InputCollect, OutputCollect, select_model, dir = qc_directory)

# Refresh with new data (simulate adding 1 month of new data)
# In practice, you'd load your latest data here
dt_qc_refresh <- dt_qc_simulated # In reality, this would be your updated dataset

QC_RobynRefresh <- robyn_refresh(
  json_file = json_file_qc,
  dt_input = dt_qc_refresh,
  dt_holidays = dt_qc_holidays,
  refresh_steps = 7,    # Weekly refresh for QC
  refresh_iters = 2000, # Maintain quality
  refresh_trials = 5
)

# Continue with refreshed results
InputCollectX <- QC_RobynRefresh$listRefresh1$InputCollect
OutputCollectX <- QC_RobynRefresh$listRefresh1$OutputCollect
select_modelX <- QC_RobynRefresh$listRefresh1$OutputCollect$selectID

################################################################
#### Step 7: Quick Commerce marginal returns analysis

## QUICK COMMERCE: Get marginal ROI for next spend level per channel
## Critical for QC: Understanding saturation points for each channel type

# Recreate saturation curves for QC channels
QC_Response_Performance <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_perf_S"  # Performance channel
)
QC_Response_Performance$plot

QC_Response_App <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "app_campaigns_S"  # App channel
)
QC_Response_App$plot

## Get marginal response for next 10K spend on performance channel
Current_Spend <- 50000  # Current weekly spend level
QC_Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_perf_S",
  metric_value = Current_Spend,
  date_range = "last_30"
)

Additional_Spend <- Current_Spend + 10000  # Add 10K
QC_Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_perf_S",
  metric_value = Additional_Spend,
  date_range = "last_30"
)

# QUICK COMMERCE: Cost Per Acquisition for the additional 10K spend
marginal_cpa <- (QC_Response2$sim_mean_spend - QC_Response1$sim_mean_spend) /
                (QC_Response2$sim_mean_response - QC_Response1$sim_mean_response)
print(paste("Marginal CPA for next 10K spend:", round(marginal_cpa, 2), "rupees per order"))

## Quick Commerce organic media analysis (Push notifications)
push_volume <- 100000  # 1 lakh push notifications
QC_response_push <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "push_notifications",
  metric_value = push_volume,
  date_range = "last_30"
)
# Orders per push notification
orders_per_push <- QC_response_push$sim_mean_response / QC_response_push$sim_mean_spend
print(paste("Orders per 1000 push notifications:", round(orders_per_push * 1000, 2)))

################################################################
#### Step 8: Quick Commerce Context Analysis

## QUICK COMMERCE SPECIFIC: Analyze context effects
## Festival impact, day-of-week patterns, city tier analysis

print("=== QUICK COMMERCE CONTEXT ANALYSIS ===")

# Festival impact analysis
diwali_dates <- c("2025-11-01", "2025-11-10", "2025-11-15")
for(date in diwali_dates) {
  fest_mult <- robyn_qcommerce_festival_multiplier(date, "performance")
  print(paste("Festival multiplier for", date, ":", round(fest_mult, 2)))
}

# Day-of-week analysis
days <- c("Monday", "Wednesday", "Friday", "Saturday", "Sunday")
for(day in days) {
  dow_mult_perf <- robyn_qcommerce_dow_multiplier(day, "performance")
  dow_mult_brand <- robyn_qcommerce_dow_multiplier(day, "brand")
  print(paste(day, "- Performance:", round(dow_mult_perf, 2),
              "Brand:", round(dow_mult_brand, 2)))
}

# City tier analysis
tiers <- c("tier1", "tier2", "tier3")
for(tier in tiers) {
  city_mult <- robyn_qcommerce_city_multiplier(tier)
  print(paste("City", tier, "multiplier:", round(city_mult, 2)))
}

################################################################
#### Step 9: Quick Commerce Model Comparison

## QUICK COMMERCE: Compare QC-optimized vs Standard Robyn results
## Useful for demonstrating QC optimization benefits

print("=== QUICK COMMERCE VS STANDARD ROBYN COMPARISON ===")
print("QC Optimization Benefits:")
print("1. Lower theta ranges (0.08-0.35) capture short QC attribution windows")
print("2. Festival intelligence: 90+ festivals across 6 years (2023-2028)")
print("3. Context-aware adjustments for Indian market patterns")
print("4. Channel-type optimization instead of complex product categorization")
print("5. Validation framework for QC-specific business logic")

# Compare theta values
qc_thetas <- names(qc_hyperparameters)[grepl("_thetas", names(qc_hyperparameters))]
print("\nQuick Commerce Theta Ranges:")
for(theta_name in qc_thetas) {
  if(!is.null(qc_hyperparameters[[theta_name]])) {
    print(paste(theta_name, ":", paste(qc_hyperparameters[[theta_name]], collapse = " - ")))
  }
}

################################################################
#### Optional: Recreate Quick Commerce models and replicate results

## From an exported JSON file - same process as standard Robyn
## JSON files contain QC metadata and optimization settings

############ WRITE ############
# Manually create JSON file with QC inputs only
robyn_write(InputCollect, dir = qc_directory)

# Create JSON with QC model results
robyn_write(InputCollect, OutputCollect, select_model, dir = qc_directory)

############ READ ############
# Recreate QC `InputCollect` and `OutputCollect` objects
json_file_qc_final <- file.path(qc_directory, paste0("QC_RobynModel-", select_model, ".json"))

# Read and check QC data stored in file
qc_json_data <- robyn_read(json_file_qc_final)
print("Quick Commerce Model JSON Structure:")
print(names(qc_json_data))

# Recreate from JSON (maintains all QC optimizations)
qc_recreated <- robyn_inputs(json_file = json_file_qc_final, dt_input = dt_qc_simulated,
                            dt_holidays = dt_qc_holidays)

################################################################
#### Final: Quick Commerce Summary

print("=== QUICK COMMERCE ROBYN IMPLEMENTATION COMPLETE ===")
print(paste("Models exported to:", qc_directory))
print("Quick Commerce Features Implemented:")
print("✓ India market optimization (6-year festival calendar)")
print("✓ Channel-type based hyperparameters")
print("✓ Context-aware transformations")
print("✓ QC-specific validation framework")
print("✓ Lower theta ranges for quick delivery attribution")
print("✓ Festival season budget optimization")
print("✓ Marginal returns analysis")
print("✓ Model refresh capabilities")
print("\nReady for production deployment on Indian Quick Commerce platforms!")
print("(Blinkit, Zepto, Swiggy Instamart, Dunzo, BigBasket, etc.)")

# Save final results - same as standard Robyn, but with QC optimizations!
qc_final_results <- list(
  InputCollect = InputCollect,         # Contains QC metadata
  OutputCollect = OutputCollect,       # Contains QC validation results
  ExportedModel = ExportedModel,
  AllocatorResults = list(
    FestivalSeason = QC_AllocatorCollect1,
    CityExpansion = QC_AllocatorCollect2,
    TargetEfficiency = QC_AllocatorCollect3
  )
)

save(qc_final_results, file = file.path(qc_directory, "QC_Robyn_Final_Results.RData"))
print(paste("Final results saved to:", file.path(qc_directory, "QC_mmm_Final_Results.RData")))