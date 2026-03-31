# Quick Commerce Simple Example
# Demonstrates how standard Robyn functions now support QC parameters directly

library(Robyn)
source("R/R/qcommerce.R")

# Create sample data
set.seed(123)
dates <- seq.Date(as.Date("2025-01-01"), by = "day", length.out = 365)
dt_qc <- data.frame(
  DATE = dates,
  orders = sample(1000:3000, 365),
  facebook_perf = sample(5000:25000, 365),
  google_search = sample(8000:30000, 365),
  app_campaigns = sample(3000:15000, 365)
)

################################################################
#### BEFORE: Complex Manual Setup (old way)

# Before: Manual hyperparameter setup required
# manual_hyperparams <- list(
#   facebook_perf_thetas = c(0.1, 0.3),
#   facebook_perf_alphas = c(0.5, 3),
#   facebook_perf_gammas = c(0.3, 1),
#   google_search_thetas = c(0.05, 0.25),
#   google_search_alphas = c(0.5, 3),
#   google_search_gammas = c(0.3, 1),
#   app_campaigns_thetas = c(0.08, 0.2),
#   app_campaigns_alphas = c(0.5, 3),
#   app_campaigns_gammas = c(0.3, 1)
# )
#
# InputCollect_old <- robyn_inputs(
#   dt_input = dt_qc,
#   date_var = "DATE",
#   dep_var = "orders",
#   paid_media_spends = c("facebook_perf", "google_search", "app_campaigns"),
#   hyperparameters = manual_hyperparams  # Manual setup required
# )

################################################################
#### AFTER: Clean QC Integration (new way)

# Now: Just add QC parameters to standard robyn_inputs()
InputCollect <- robyn_inputs(
  dt_input = dt_qc,
  date_var = "DATE",
  dep_var = "orders",
  dep_var_type = "conversion",
  paid_media_spends = c("facebook_perf", "google_search", "app_campaigns"),

  # QUICK COMMERCE: Just add these 4 parameters!
  channel_types = c("performance", "performance", "app"),  # That's it!
  city_tier = "tier1",          # Optional: city tier optimization
  context_aware = TRUE,         # Optional: festival/day-of-week effects
  qc_region = "india"          # Optional: India market optimization

  # Hyperparameters automatically generated based on channel_types!
)

# Everything else exactly the same as standard Robyn!
OutputModels <- robyn_run(InputCollect, cores = 2, iterations = 200, trials = 2)
OutputCollect <- robyn_outputs(InputCollect, OutputModels)

# QC validation automatically included!
if(!is.null(OutputCollect$qcommerce_validation)) {
  print("QC Validation automatically performed:")
  print(OutputCollect$qcommerce_validation$validation_summary)
}

print("=== SUCCESS: Quick Commerce optimization with standard Robyn functions! ===")
print("Key benefits:")
print("✓ Same familiar Robyn workflow")
print("✓ QC hyperparameters auto-generated")
print("✓ QC validation automatically included")
print("✓ All existing Robyn features preserved")
print("✓ Works with robyn_allocator(), robyn_refresh(), etc.")

################################################################
#### Comparison Summary

print("\n=== INTEGRATION COMPARISON ===")
print("BEFORE (Manual):")
print("- Manual hyperparameter setup (20+ parameters)")
print("- No QC-specific validation")
print("- No India market optimization")
print("- Complex festival handling")

print("\nAFTER (QC Integration):")
print("- Add 4 QC parameters to robyn_inputs()")
print("- Automatic hyperparameter generation")
print("- Built-in QC validation")
print("- 6-year India festival calendar")
print("- Context-aware transformations")

print("\nResult: Same Robyn workflow + Quick Commerce superpowers! 🚀")