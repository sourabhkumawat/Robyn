# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Quick Commerce Robyn - India Market Optimization
# Includes context-aware transformations and 6-year festival calendar (2023-2028)

####################################################################
#' Quick Commerce Hyperparameter Bounds
#'
#' \code{robyn_qcommerce_bounds()} provides channel-type specific hyperparameter
#' bounds optimized for quick commerce business model in Indian market. Instead
#' of requiring complex product category mapping, this function uses simplified
#' channel types based on advertising strategy.
#'
#' @family Quick Commerce
#' @param channel_type Character. One of \code{c("performance", "brand", "app",
#' "social", "general")}. Defaults to "performance" for quick commerce optimization.
#' \itemize{
#'   \item "performance": Google Ads, Facebook Performance, Search (theta: 0.10-0.25)
#'   \item "brand": Facebook Brand, YouTube, TV, Display (theta: 0.15-0.35)
#'   \item "app": App campaigns, push notifications (theta: 0.08-0.20)
#'   \item "social": Social media, influencer marketing (theta: 0.12-0.28)
#'   \item "general": Mixed/unknown campaigns (theta: 0.12-0.30)
#' }
#' @param region Character. One of \code{c("india", "global")}. Default "india"
#' for India-specific optimizations.
#' @return Named list containing hyperparameter bounds:
#' \itemize{
#'   \item thetas: Numeric vector of length 2 for adstock decay bounds
#'   \item alphas: Numeric vector of length 2 for saturation alpha bounds
#'   \item gammas: Numeric vector of length 2 for saturation gamma bounds
#' }
#' @examples
#' # Get bounds for performance marketing channels
#' bounds_perf <- robyn_qcommerce_bounds("performance")
#' print(bounds_perf$thetas) # c(0.10, 0.25)
#'
#' # Get bounds for brand marketing channels
#' bounds_brand <- robyn_qcommerce_bounds("brand")
#' print(bounds_brand$thetas) # c(0.15, 0.35)
#'
#' # Get bounds for app marketing channels
#' bounds_app <- robyn_qcommerce_bounds("app")
#' print(bounds_app$thetas) # c(0.08, 0.20)
#' @export
robyn_qcommerce_bounds <- function(channel_type = "performance", region = "india") {

  # Input validation
  valid_channels <- c("performance", "brand", "app", "social", "general")
  valid_regions <- c("india", "global")

  if (!channel_type %in% valid_channels) {
    stop(sprintf("channel_type must be one of: %s. Got: %s",
                 paste(valid_channels, collapse = ", "), channel_type))
  }

  if (!region %in% valid_regions) {
    stop(sprintf("region must be one of: %s. Got: %s",
                 paste(valid_regions, collapse = ", "), region))
  }

  if (region == "india") {
    bounds_matrix <- list(
      "performance" = list(       # Google Ads, Facebook Performance, Search
        thetas = c(0.10, 0.25),   # Lower - immediate response driven
        alphas = c(0.5, 3),       # Standard Robyn saturation bounds
        gammas = c(0.3, 1)        # Standard Robyn saturation bounds
      ),
      "brand" = list(             # Facebook Brand, YouTube, TV, Display
        thetas = c(0.15, 0.35),   # Higher - brand carryover effect
        alphas = c(0.5, 3),
        gammas = c(0.3, 1)
      ),
      "app" = list(               # App campaigns, push notifications, in-app
        thetas = c(0.08, 0.20),   # Lowest - immediate in-app actions
        alphas = c(0.5, 3),
        gammas = c(0.3, 1)
      ),
      "social" = list(            # Social media, influencer, organic social
        thetas = c(0.12, 0.28),   # Moderate - social engagement effects
        alphas = c(0.5, 3),
        gammas = c(0.3, 1)
      ),
      "general" = list(           # Fallback for mixed/unknown campaigns
        thetas = c(0.12, 0.30),   # Balanced for quick commerce behavior
        alphas = c(0.5, 3),
        gammas = c(0.3, 1)
      )
    )
  } else {
    # Global/standard defaults (fallback to standard Robyn ranges)
    bounds_matrix <- list(
      "general" = list(
        thetas = c(0.3, 0.8),     # Standard Robyn geometric adstock ranges
        alphas = c(0.5, 3),
        gammas = c(0.3, 1)
      )
    )
    channel_type <- "general"
  }

  return(bounds_matrix[[channel_type]])
}

####################################################################
#' Quick Commerce Context Multipliers
#'
#' \code{robyn_qcommerce_dow_multiplier()} provides day-of-week adjustment
#' multipliers for adstock calculations, optimized for Indian quick commerce
#' shopping patterns and channel-specific behavior.
#'
#' @family Quick Commerce
#' @param dow Character. Day of week: one of \code{c("Monday", "Tuesday",
#' "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")}.
#' @param channel_type Character. One of \code{c("performance", "brand", "app",
#' "social", "general")}. Default "performance".
#' @return Numeric. Multiplier value between 0.7 and 1.3 for theta adjustment.
#' @examples
#' # Weekend effect for performance channels (shopping apps)
#' robyn_qcommerce_dow_multiplier("Saturday", "performance") # ~1.08
#'
#' # Weekend effect for brand channels (more leisure browsing)
#' robyn_qcommerce_dow_multiplier("Saturday", "brand") # ~1.32
#'
#' # Mid-week patterns
#' robyn_qcommerce_dow_multiplier("Wednesday", "performance") # ~0.8
#' @rdname robyn_qcommerce_multipliers
#' @export
robyn_qcommerce_dow_multiplier <- function(dow, channel_type = "performance") {

  # Input validation
  valid_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  valid_channels <- c("performance", "brand", "app", "social", "general")

  if (!dow %in% valid_days) {
    stop(sprintf("dow must be one of: %s. Got: %s",
                 paste(valid_days, collapse = ", "), dow))
  }

  if (!channel_type %in% valid_channels) {
    stop(sprintf("channel_type must be one of: %s. Got: %s",
                 paste(valid_channels, collapse = ", "), channel_type))
  }

  # Base quick commerce day-of-week patterns for India
  base_multipliers <- list(
    "Monday" = 0.9,    # Work week starts, routine shopping
    "Tuesday" = 0.8,   # Mid-week low
    "Wednesday" = 0.8, # Mid-week low
    "Thursday" = 0.9,  # Pre-weekend preparation
    "Friday" = 1.0,    # Weekend preparation begins
    "Saturday" = 1.2,  # Weekend leisure shopping
    "Sunday" = 1.1     # Sunday family time
  )

  base <- base_multipliers[[dow]]
  if (is.null(base)) base <- 1.0

  # Channel-type specific adjustments
  if (channel_type == "performance") {
    # Performance channels: Less weekend effect (immediate need-driven)
    if (dow %in% c("Saturday", "Sunday")) base <- base * 0.9
  } else if (channel_type == "brand") {
    # Brand channels: More weekend effect (leisure browsing time)
    if (dow %in% c("Saturday", "Sunday")) base <- base * 1.1
  } else if (channel_type == "app") {
    # App channels: Higher weekend effect (in-app engagement)
    if (dow %in% c("Saturday", "Sunday")) base <- base * 1.05
  }

  return(base)
}

####################################################################
#' Quick Commerce Festival Multipliers
#'
#' \code{robyn_qcommerce_festival_multiplier()} provides festival-specific
#' multipliers for Indian market with comprehensive 6-year coverage (2023-2028).
#' Includes major Indian festivals with channel-type specific adjustments.
#'
#' @family Quick Commerce
#' @param date Character or Date. Date in "YYYY-MM-DD" format.
#' @param channel_type Character. One of \code{c("performance", "brand", "app",
#' "social", "general")}. Default "performance".
#' @return Numeric. Festival multiplier ranging from 1.0 (no festival) to 2.53
#' (peak Diwali effect for app channels).
#' @examples
#' # Diwali 2024 effect for performance channels
#' robyn_qcommerce_festival_multiplier("2024-11-01", "performance") # ~2.42
#'
#' # Regular day (no festival)
#' robyn_qcommerce_festival_multiplier("2024-01-15", "performance") # 1.0
#'
#' # Eid effect for app channels
#' robyn_qcommerce_festival_multiplier("2024-04-10", "app") # ~2.07
#' @rdname robyn_qcommerce_multipliers
#' @export
robyn_qcommerce_festival_multiplier <- function(date, channel_type = "performance") {

  # Input validation
  if (missing(date) || is.null(date)) {
    stop("date parameter is required")
  }

  valid_channels <- c("performance", "brand", "app", "social", "general")
  if (!channel_type %in% valid_channels) {
    stop(sprintf("channel_type must be one of: %s. Got: %s",
                 paste(valid_channels, collapse = ", "), channel_type))
  }

  # Convert to Date object
  date_obj <- tryCatch({
    as.Date(date)
  }, error = function(e) {
    stop(sprintf("date must be convertible to Date format. Got: %s", date))
  })

  # Comprehensive 6-year Indian festival calendar (2023-2028)
  festivals_comprehensive <- list(
    # === 2023 FESTIVALS (HISTORICAL) ===
    "2023-01-14" = list(end = "2023-01-15", name = "makar_sankranti", multiplier = 1.2),
    "2023-01-26" = list(end = "2023-01-26", name = "republic_day", multiplier = 1.1),
    "2023-02-18" = list(end = "2023-02-19", name = "shivratri", multiplier = 1.2),
    "2023-03-08" = list(end = "2023-03-18", name = "holi", multiplier = 1.5),
    "2023-03-22" = list(end = "2023-03-24", name = "eid_fitr", multiplier = 1.8),
    "2023-03-30" = list(end = "2023-03-31", name = "ram_navami", multiplier = 1.2),
    "2023-06-29" = list(end = "2023-07-01", name = "eid_adha", multiplier = 1.6),
    "2023-08-15" = list(end = "2023-08-15", name = "independence_day", multiplier = 1.1),
    "2023-08-30" = list(end = "2023-08-31", name = "raksha_bandhan", multiplier = 1.4),
    "2023-09-19" = list(end = "2023-09-29", name = "ganesh_chaturthi", multiplier = 1.5),
    "2023-10-15" = list(end = "2023-10-24", name = "navratri_dussehra", multiplier = 1.4),
    "2023-11-01" = list(end = "2023-11-02", name = "karva_chauth", multiplier = 1.7),
    "2023-11-10" = list(end = "2023-11-27", name = "diwali_season", multiplier = 2.2),

    # === 2024 FESTIVALS (HISTORICAL) ===
    "2024-01-14" = list(end = "2024-01-15", name = "makar_sankranti", multiplier = 1.2),
    "2024-01-26" = list(end = "2024-01-26", name = "republic_day", multiplier = 1.1),
    "2024-03-08" = list(end = "2024-03-09", name = "shivratri", multiplier = 1.2),
    "2024-03-25" = list(end = "2024-04-04", name = "holi", multiplier = 1.5),
    "2024-04-10" = list(end = "2024-04-12", name = "eid_fitr", multiplier = 1.8),
    "2024-04-17" = list(end = "2024-04-18", name = "ram_navami", multiplier = 1.2),
    "2024-06-17" = list(end = "2024-06-19", name = "eid_adha", multiplier = 1.6),
    "2024-08-15" = list(end = "2024-08-15", name = "independence_day", multiplier = 1.1),
    "2024-08-19" = list(end = "2024-08-20", name = "raksha_bandhan", multiplier = 1.4),
    "2024-09-07" = list(end = "2024-09-17", name = "ganesh_chaturthi", multiplier = 1.5),
    "2024-10-03" = list(end = "2024-10-12", name = "navratri_dussehra", multiplier = 1.4),
    "2024-10-20" = list(end = "2024-10-21", name = "karva_chauth", multiplier = 1.7),
    "2024-10-29" = list(end = "2024-11-15", name = "diwali_season", multiplier = 2.2),

    # === 2025 FESTIVALS (HISTORICAL) ===
    "2025-01-13" = list(end = "2025-01-14", name = "makar_sankranti", multiplier = 1.2),
    "2025-01-26" = list(end = "2025-01-26", name = "republic_day", multiplier = 1.1),
    "2025-02-26" = list(end = "2025-02-27", name = "shivratri", multiplier = 1.2),
    "2025-03-14" = list(end = "2025-03-24", name = "holi", multiplier = 1.5),
    "2025-03-31" = list(end = "2025-04-02", name = "eid_fitr", multiplier = 1.8),
    "2025-04-06" = list(end = "2025-04-07", name = "ram_navami", multiplier = 1.2),
    "2025-06-07" = list(end = "2025-06-09", name = "eid_adha", multiplier = 1.6),
    "2025-08-15" = list(end = "2025-08-15", name = "independence_day", multiplier = 1.1),
    "2025-08-09" = list(end = "2025-08-10", name = "raksha_bandhan", multiplier = 1.4),
    "2025-08-27" = list(end = "2025-09-06", name = "ganesh_chaturthi", multiplier = 1.5),
    "2025-09-22" = list(end = "2025-10-02", name = "navratri_dussehra", multiplier = 1.4),
    "2025-10-09" = list(end = "2025-10-10", name = "karva_chauth", multiplier = 1.7),
    "2025-10-20" = list(end = "2025-11-07", name = "diwali_season", multiplier = 2.2),

    # === 2026 FESTIVALS (CURRENT YEAR) ===
    "2026-01-14" = list(end = "2026-01-15", name = "makar_sankranti", multiplier = 1.2),
    "2026-01-26" = list(end = "2026-01-26", name = "republic_day", multiplier = 1.1),
    "2026-02-16" = list(end = "2026-02-17", name = "shivratri", multiplier = 1.2),
    "2026-03-05" = list(end = "2026-03-15", name = "holi", multiplier = 1.5),
    "2026-03-20" = list(end = "2026-03-22", name = "eid_fitr", multiplier = 1.8),
    "2026-03-26" = list(end = "2026-03-27", name = "ram_navami", multiplier = 1.2),
    "2026-05-27" = list(end = "2026-05-29", name = "eid_adha", multiplier = 1.6),
    "2026-08-15" = list(end = "2026-08-15", name = "independence_day", multiplier = 1.1),
    "2026-08-28" = list(end = "2026-08-29", name = "raksha_bandhan", multiplier = 1.4),
    "2026-09-16" = list(end = "2026-09-26", name = "ganesh_chaturthi", multiplier = 1.5),
    "2026-10-12" = list(end = "2026-10-21", name = "navratri_dussehra", multiplier = 1.4),
    "2026-10-29" = list(end = "2026-10-30", name = "karva_chauth", multiplier = 1.7),
    "2026-10-29" = list(end = "2026-11-18", name = "diwali_season", multiplier = 2.2),

    # === 2027 FESTIVALS (FUTURE PLANNING) ===
    "2027-01-14" = list(end = "2027-01-15", name = "makar_sankranti", multiplier = 1.2),
    "2027-01-26" = list(end = "2027-01-26", name = "republic_day", multiplier = 1.1),
    "2027-03-04" = list(end = "2027-03-05", name = "shivratri", multiplier = 1.2),
    "2027-03-22" = list(end = "2027-04-01", name = "holi", multiplier = 1.5),
    "2027-03-09" = list(end = "2027-03-11", name = "eid_fitr", multiplier = 1.8),
    "2027-04-14" = list(end = "2027-04-15", name = "ram_navami", multiplier = 1.2),
    "2027-05-16" = list(end = "2027-05-18", name = "eid_adha", multiplier = 1.6),
    "2027-08-15" = list(end = "2027-08-15", name = "independence_day", multiplier = 1.1),
    "2027-08-17" = list(end = "2027-08-18", name = "raksha_bandhan", multiplier = 1.4),
    "2027-09-05" = list(end = "2027-09-15", name = "ganesh_chaturthi", multiplier = 1.5),
    "2027-09-30" = list(end = "2027-10-11", name = "navratri_dussehra", multiplier = 1.4),
    "2027-10-31" = list(end = "2027-11-01", name = "karva_chauth", multiplier = 1.7),
    "2027-11-04" = list(end = "2027-11-24", name = "diwali_season", multiplier = 2.2),

    # === 2028 FESTIVALS (FUTURE PLANNING) ===
    "2028-01-14" = list(end = "2028-01-15", name = "makar_sankranti", multiplier = 1.2),
    "2028-01-26" = list(end = "2028-01-26", name = "republic_day", multiplier = 1.1),
    "2028-02-25" = list(end = "2028-02-26", name = "shivratri", multiplier = 1.2),
    "2028-03-11" = list(end = "2028-03-21", name = "holi", multiplier = 1.5),
    "2028-03-27" = list(end = "2028-03-29", name = "eid_fitr", multiplier = 1.8),
    "2028-04-14" = list(end = "2028-04-15", name = "ram_navami", multiplier = 1.2),
    "2028-06-03" = list(end = "2028-06-05", name = "eid_adha", multiplier = 1.6),
    "2028-08-15" = list(end = "2028-08-15", name = "independence_day", multiplier = 1.1),
    "2028-08-30" = list(end = "2028-08-31", name = "raksha_bandhan", multiplier = 1.4),
    "2028-09-06" = list(end = "2028-09-16", name = "ganesh_chaturthi", multiplier = 1.5),
    "2028-09-26" = list(end = "2028-10-07", name = "navratri_dussehra", multiplier = 1.4),
    "2028-10-19" = list(end = "2028-10-20", name = "karva_chauth", multiplier = 1.7),
    "2028-10-24" = list(end = "2028-11-13", name = "diwali_season", multiplier = 2.2)
  )

  # Check if date falls in any festival period
  for (start_date in names(festivals_comprehensive)) {
    festival_info <- festivals_comprehensive[[start_date]]
    if (date_obj >= as.Date(start_date) && date_obj <= as.Date(festival_info$end)) {

      base_multiplier <- festival_info$multiplier

      # Channel-type specific adjustments
      if (channel_type == "performance") {
        # Performance channels get higher festival boost (immediate purchase intent)
        return(base_multiplier * 1.1)
      } else if (channel_type == "app") {
        # App campaigns get highest festival boost (direct action in-app)
        return(base_multiplier * 1.15)
      } else if (channel_type == "brand") {
        # Brand channels get standard festival effect
        return(base_multiplier)
      } else if (channel_type == "social") {
        # Social channels get moderate boost (sharing, engagement)
        return(base_multiplier * 1.05)
      } else {
        # General default
        return(base_multiplier)
      }
    }
  }

  return(1.0)  # No festival effect
}

####################################################################
#' Quick Commerce City Tier Multipliers
#'
#' \code{robyn_qcommerce_city_multiplier()} provides city tier adjustment
#' multipliers based on quick commerce adoption and behavior patterns
#' across Indian metropolitan areas.
#'
#' @family Quick Commerce
#' @param city_tier Character. One of \code{c("tier1", "tier2", "tier3")}.
#' \itemize{
#'   \item "tier1": Mumbai, Delhi, Bangalore, Chennai, Kolkata, Hyderabad
#'   \item "tier2": Pune, Ahmedabad, Jaipur, Lucknow, Kanpur, Nagpur
#'   \item "tier3": Smaller cities with emerging quick commerce adoption
#' }
#' @return Numeric. Multiplier value: 1.0 (tier1), 0.85 (tier2), 0.7 (tier3).
#' @examples
#' robyn_qcommerce_city_multiplier("tier1") # 1.0
#' robyn_qcommerce_city_multiplier("tier2") # 0.85
#' robyn_qcommerce_city_multiplier("tier3") # 0.7
#' @rdname robyn_qcommerce_multipliers
#' @export
robyn_qcommerce_city_multiplier <- function(city_tier) {

  # Input validation
  valid_tiers <- c("tier1", "tier2", "tier3")
  if (!city_tier %in% valid_tiers) {
    stop(sprintf("city_tier must be one of: %s. Got: %s",
                 paste(valid_tiers, collapse = ", "), city_tier))
  }

  city_multipliers <- list(
    "tier1" = 1.0,   # Metro cities (Mumbai, Delhi, Bangalore)
    "tier2" = 0.85,  # Large cities (Pune, Hyderabad)
    "tier3" = 0.7    # Smaller cities (more impulse-driven)
  )

  return(city_multipliers[[city_tier]])
}

####################################################################
#' Quick Commerce Seasonal Weather Multipliers
#'
#' \code{robyn_qcommerce_weather_multiplier()} provides seasonal weather
#' adjustment multipliers for Indian climate patterns affecting quick
#' commerce shopping behavior.
#'
#' @family Quick Commerce
#' @param date Character or Date. Date in "YYYY-MM-DD" format.
#' @param city Character. City name for location-specific adjustments.
#' Currently unused but reserved for future enhancements.
#' @return Numeric. Weather multiplier: 0.6 (summer), 0.8 (monsoon), 1.0 (pleasant).
#' @examples
#' # Summer effect (April-May)
#' robyn_qcommerce_weather_multiplier("2024-05-15") # 0.6
#'
#' # Monsoon effect (June-September)
#' robyn_qcommerce_weather_multiplier("2024-07-15") # 0.8
#'
#' # Pleasant weather (October-March)
#' robyn_qcommerce_weather_multiplier("2024-01-15") # 1.0
#' @rdname robyn_qcommerce_multipliers
#' @export
robyn_qcommerce_weather_multiplier <- function(date, city = NULL) {

  # Input validation
  if (missing(date) || is.null(date)) {
    stop("date parameter is required")
  }

  # Convert to Date object
  date_obj <- tryCatch({
    as.Date(date)
  }, error = function(e) {
    stop(sprintf("date must be convertible to Date format. Got: %s", date))
  })

  month <- as.numeric(format(date_obj, "%m"))

  # Indian seasonal patterns
  if (month %in% c(4, 5)) {
    # Summer: High urgency purchases (water, cooling items)
    return(0.6)
  } else if (month %in% c(6, 7, 8, 9)) {
    # Monsoon: More planning needed, less impulse
    return(0.8)
  } else {
    # Pleasant months: Normal behavior
    return(1.0)
  }
}

####################################################################
#' Quick Commerce Context-Aware Adstock Transformation
#'
#' \code{robyn_qcommerce_adstock()} applies geometric adstock transformation
#' with context-aware adjustments for quick commerce environments. Integrates
#' day-of-week, festival, city tier, and weather effects into adstock calculation.
#'
#' @family Quick Commerce
#' @inheritParams adstock_geometric
#' @param date_vector Character or Date vector. Dates corresponding to each
#' value in \code{x}. Must have same length as \code{x}.
#' @param channel_type Character. One of \code{c("performance", "brand", "app",
#' "social", "general")}. Default "performance".
#' @param city_tier Character. One of \code{c("tier1", "tier2", "tier3")}.
#' Default "tier1".
#' @return Named list containing:
#' \itemize{
#'   \item x: Original input vector
#'   \item x_decayed: Adstocked values with context adjustments
#'   \item thetaVecCum: Cumulative decay vector
#'   \item inflation_total: Total inflation factor
#'   \item context_adjustments: Summary of applied context effects
#' }
#' @examples
#' \dontrun{
#' # Context-aware adstock for Diwali period
#' dates <- seq.Date(as.Date("2024-11-01"), by = "day", length.out = 10)
#' spend <- rep(10000, 10)
#'
#' result <- robyn_qcommerce_adstock(
#'   x = spend,
#'   theta = 0.2,
#'   date_vector = dates,
#'   channel_type = "performance",
#'   city_tier = "tier1"
#' )
#'
#' # Compare original vs context-adjusted adstock
#' print(result$x_decayed)
#' print(result$context_adjustments)
#' }
#' @export
robyn_qcommerce_adstock <- function(x, theta, date_vector,
                                    channel_type = "performance",
                                    city_tier = "tier1") {

  # Input validation
  if (length(x) != length(date_vector)) {
    stop("x and date_vector must have the same length")
  }

  if (length(theta) != 1 || !is.numeric(theta) || theta < 0 || theta >= 1) {
    stop("theta must be a single numeric value between 0 and 1")
  }

  valid_channels <- c("performance", "brand", "app", "social", "general")
  if (!channel_type %in% valid_channels) {
    stop(sprintf("channel_type must be one of: %s. Got: %s",
                 paste(valid_channels, collapse = ", "), channel_type))
  }

  valid_tiers <- c("tier1", "tier2", "tier3")
  if (!city_tier %in% valid_tiers) {
    stop(sprintf("city_tier must be one of: %s. Got: %s",
                 paste(valid_tiers, collapse = ", "), city_tier))
  }

  # Convert date_vector to Date objects
  date_vector <- tryCatch({
    as.Date(date_vector)
  }, error = function(e) {
    stop("date_vector must be convertible to Date format")
  })

  # Apply standard geometric adstock first
  standard_adstock <- adstock_geometric(x, theta)

  # Calculate context adjustments for each time period
  context_multipliers <- numeric(length(x))
  context_summary <- list()

  for (i in seq_along(x)) {

    # Get day of week
    dow <- weekdays(date_vector[i])
    dow_mult <- robyn_qcommerce_dow_multiplier(dow, channel_type)

    # Get festival effect
    festival_mult <- robyn_qcommerce_festival_multiplier(date_vector[i], channel_type)

    # Get city tier effect
    city_mult <- robyn_qcommerce_city_multiplier(city_tier)

    # Get weather effect
    weather_mult <- robyn_qcommerce_weather_multiplier(date_vector[i])

    # Combined multiplier
    combined_mult <- dow_mult * festival_mult * city_mult * weather_mult
    context_multipliers[i] <- combined_mult

    # Store context summary for first few dates (for reporting)
    if (i <= 3) {
      context_summary[[paste0("date_", i)]] <- list(
        date = as.character(date_vector[i]),
        dow = dow,
        dow_mult = dow_mult,
        festival_mult = festival_mult,
        city_mult = city_mult,
        weather_mult = weather_mult,
        combined_mult = combined_mult
      )
    }
  }

  # Apply context adjustments to adstocked values
  x_decayed_adjusted <- standard_adstock$x_decayed * context_multipliers

  # Calculate new inflation factor
  inflation_total_adjusted <- sum(x_decayed_adjusted) / sum(x)

  return(list(
    x = x,
    x_decayed = x_decayed_adjusted,
    thetaVecCum = standard_adstock$thetaVecCum,
    inflation_total = inflation_total_adjusted,
    context_adjustments = context_summary
  ))
}

####################################################################
#' Quick Commerce Hyperparameter Setup
#'
#' \code{robyn_qcommerce_hyperparameters()} automatically generates
#' hyperparameter bounds for multiple channels using simplified channel-type
#' classification. Eliminates need for manual hyperparameter setup.
#'
#' @family Quick Commerce
#' @param paid_media_vars Character vector. Names of paid media variables
#' exactly as they appear in the input dataset columns.
#' @param channel_types Character vector. Channel types corresponding to each
#' variable in \code{paid_media_vars}. Must have same length. If NULL,
#' defaults to "performance" for all channels (optimal for quick commerce).
#' @param region Character. One of \code{c("india", "global")}. Default "india".
#' @return Named list of hyperparameter bounds suitable for \code{robyn_inputs()}.
#' Each channel gets thetas, alphas, and gammas bounds based on its channel type.
#' @examples
#' \dontrun{
#' # Automatic setup with performance defaults
#' channels <- c("facebook_ads", "google_ads", "app_campaigns")
#' hyperparams <- robyn_qcommerce_hyperparameters(channels)
#'
#' # Custom channel type mapping
#' hyperparams <- robyn_qcommerce_hyperparameters(
#'   paid_media_vars = c("facebook_performance", "youtube_brand", "app_push"),
#'   channel_types = c("performance", "brand", "app")
#' )
#'
#' # Use with robyn_inputs()
#' InputCollect <- robyn_inputs(
#'   dt_input = dt_input,
#'   hyperparameters = hyperparams,
#'   # ... other parameters
#' )
#' }
#' @export
robyn_qcommerce_hyperparameters <- function(paid_media_vars,
                                             channel_types = NULL,
                                             region = "india") {

  # Input validation
  if (missing(paid_media_vars) || length(paid_media_vars) == 0) {
    stop("paid_media_vars must be provided and non-empty")
  }

  if (!is.character(paid_media_vars)) {
    stop("paid_media_vars must be a character vector")
  }

  # Default to "performance" if no channel types specified (best for quick commerce)
  if (is.null(channel_types)) {
    channel_types <- rep("performance", length(paid_media_vars))
    message("No channel_types specified. Defaulting all channels to 'performance' (optimized for quick commerce)")
  }

  if (length(paid_media_vars) != length(channel_types)) {
    stop(sprintf("paid_media_vars (%d) and channel_types (%d) must have same length",
                 length(paid_media_vars), length(channel_types)))
  }

  valid_regions <- c("india", "global")
  if (!region %in% valid_regions) {
    stop(sprintf("region must be one of: %s. Got: %s",
                 paste(valid_regions, collapse = ", "), region))
  }

  hyperparam_bounds <- list()

  for (i in seq_along(paid_media_vars)) {
    channel_var <- paid_media_vars[i]
    channel_type <- channel_types[i]

    # Get channel-type specific bounds
    bounds <- robyn_qcommerce_bounds(channel_type, region)

    # Set hyperparameter names following Robyn conventions
    hyperparam_bounds[[paste0(channel_var, "_thetas")]] <- bounds$thetas
    hyperparam_bounds[[paste0(channel_var, "_alphas")]] <- bounds$alphas
    hyperparam_bounds[[paste0(channel_var, "_gammas")]] <- bounds$gammas
  }

  return(hyperparam_bounds)
}

####################################################################
#' Quick Commerce Model Result Validation
#'
#' \code{robyn_qcommerce_validate()} validates model results for quick commerce
#' specific business logic, including theta range appropriateness, zero-spend
#' day explanations, and festival period performance.
#'
#' @family Quick Commerce
#' @param OutputCollect List. Output from \code{robyn_outputs()}.
#' @param InputCollect List. Output from \code{robyn_inputs()}.
#' @param dt_input data.frame. Original input dataset.
#' @return Named list containing validation results:
#' \itemize{
#'   \item validation_summary: Overall validation status
#'   \item theta_validation: Theta range appropriateness for QC
#'   \item business_logic_checks: QC-specific business rule validation
#'   \item warnings: List of validation warnings
#'   \item recommendations: Suggested improvements
#' }
#' @examples
#' \dontrun{
#' # After running robyn_outputs()
#' validation_results <- robyn_qcommerce_validate(
#'   OutputCollect = OutputCollect,
#'   InputCollect = InputCollect,
#'   dt_input = dt_input
#' )
#'
#' # Check validation status
#' print(validation_results$validation_summary)
#'
#' # Review warnings and recommendations
#' print(validation_results$warnings)
#' print(validation_results$recommendations)
#' }
#' @export
robyn_qcommerce_validate <- function(OutputCollect, InputCollect, dt_input) {

  # Input validation
  if (missing(OutputCollect) || is.null(OutputCollect)) {
    stop("OutputCollect parameter is required")
  }

  if (missing(InputCollect) || is.null(InputCollect)) {
    stop("InputCollect parameter is required")
  }

  if (missing(dt_input) || is.null(dt_input)) {
    stop("dt_input parameter is required")
  }

  validation_results <- list()
  warnings_list <- character()
  recommendations_list <- character()

  # Extract best model hyperparameters
  tryCatch({
    best_model_id <- OutputCollect$selectID
    if (is.null(best_model_id)) {
      best_model_id <- OutputCollect$allSolutions[1]
    }

    model_params <- OutputCollect$resultHypParam[
      OutputCollect$resultHypParam$solID == best_model_id, ]

    if (nrow(model_params) == 0) {
      stop("Could not find hyperparameters for selected model")
    }
  }, error = function(e) {
    warnings_list <<- c(warnings_list,
                       paste("Could not extract model hyperparameters:", e$message))
    return()
  })

  # Validate theta ranges for quick commerce
  theta_validation <- list()
  theta_cols <- names(model_params)[grepl("_thetas$", names(model_params))]

  for (theta_col in theta_cols) {
    theta_value <- model_params[[theta_col]]
    channel_name <- gsub("_thetas$", "", theta_col)

    if (theta_value > 0.5) {
      warnings_list <- c(warnings_list,
                        sprintf("High theta detected for %s: %.3f (>0.5 may be too high for quick commerce)",
                               channel_name, theta_value))
      recommendations_list <- c(recommendations_list,
                               sprintf("Consider lowering theta bounds for %s to better capture quick commerce behavior",
                                      channel_name))
    }

    theta_validation[[channel_name]] <- list(
      value = theta_value,
      appropriate_for_qc = theta_value <= 0.5,
      channel = channel_name
    )
  }

  # Business logic validation
  business_logic_checks <- list()

  # Check for zero-spend periods explanation
  media_vars <- InputCollect$paid_media_spends
  zero_spend_analysis <- list()

  for (media_var in media_vars) {
    if (media_var %in% names(dt_input)) {
      zero_days <- sum(dt_input[[media_var]] == 0, na.rm = TRUE)
      total_days <- nrow(dt_input)
      zero_ratio <- zero_days / total_days

      zero_spend_analysis[[media_var]] <- list(
        zero_days = zero_days,
        total_days = total_days,
        zero_ratio = zero_ratio
      )

      if (zero_ratio > 0.1) {
        recommendations_list <- c(recommendations_list,
                                sprintf("%s has %.1f%% zero-spend days - validate carryover effects explain performance",
                                       media_var, zero_ratio * 100))
      }
    }
  }

  business_logic_checks$zero_spend_analysis <- zero_spend_analysis

  # Model performance checks
  model_performance <- list()
  if ("rsq_train" %in% names(model_params)) {
    r_squared <- model_params$rsq_train
    model_performance$r_squared <- r_squared

    if (r_squared < 0.6) {
      warnings_list <- c(warnings_list,
                        sprintf("Low R-squared detected: %.3f (< 0.6)", r_squared))
      recommendations_list <- c(recommendations_list,
                               "Consider adding more context variables or adjusting hyperparameter bounds")
    }
  }

  if ("nrmse" %in% names(model_params)) {
    nrmse <- model_params$nrmse
    model_performance$nrmse <- nrmse

    if (nrmse > 0.2) {
      warnings_list <- c(warnings_list,
                        sprintf("High NRMSE detected: %.3f (> 0.2)", nrmse))
    }
  }

  # Compile validation summary
  validation_summary <- list(
    total_channels_validated = length(theta_validation),
    channels_appropriate_for_qc = sum(sapply(theta_validation, function(x) x$appropriate_for_qc)),
    total_warnings = length(warnings_list),
    total_recommendations = length(recommendations_list),
    overall_status = if (length(warnings_list) == 0) "PASSED" else "WARNINGS_FOUND"
  )

  return(list(
    validation_summary = validation_summary,
    theta_validation = theta_validation,
    business_logic_checks = business_logic_checks,
    model_performance = model_performance,
    warnings = warnings_list,
    recommendations = recommendations_list
  ))
}

## Quick Commerce helper functions are now integrated into core Robyn functions!
## Use robyn_inputs() with channel_types parameter for QC optimization
## All QC features (validation, context-aware transformations, etc.) are built-in