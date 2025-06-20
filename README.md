# peakPerformR

**An R package for analyzing athlete performance peaks across professional sports leagues**

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

🔗 **[Interactive Dashboard](https://github.com/elivatsaas/peakPerformR-dashboard)**

---

## Key Research Findings

Analysis of **6,000+ professional athletes** reveals dramatic differences in performance peak timing across leagues:

| League | Peak Age | Prime Duration | Players |
|--------|----------|----------------|---------|
| **Chess (Male)** | 26.4 years | **4.6 years** ⭐ | 185 |
| **Chess (Female)** | 25.6 years | **3.8 years** | 65 |
| **NBA** | 25.2 years | 3.2 years | 558 |
| **NFL** | 25.4 years | 2.6 years | 2,471 |
| **WNBA** | 25.7 years | 2.6 years | 215 |
| **MLS** | 24.1 years | 2.4 years | 274 |
| **MLB** | 26.2 years | 2.3 years | 1,516 |
| **NWSL** | 25.6 years | 2.3 years | 112 |
| **NHL** | 25.6 years | **2.2 years** 🔻 | 649 |

*⭐ Chess players maintain peak performance 2x longer than most athletes*  
*🔻 NHL players have the shortest performance peaks*

---

## Installation

```r
# Install from GitHub
devtools::install_github("elivatsaas/peakPerformR")

# Load package
library(peakPerformR)
library(dplyr)
```

---

## Main Analysis Workflow

```r
library(peakPerformR)

# Step 1: Load all sports data
data("all_sports_tidy")  # or load your CSV file

# Step 2: Process player trajectories (fits splines and creates predictions)
trajectory_results <- process_player_trajectories(
  player_data = all_sports_tidy,
  min_knots = 3,
  max_knots = 8
)

# Step 3: Extract the trajectory data
trajectories <- trajectory_results$trajectories

# Step 4: Identify primes using actual performance data
actual_primes <- identify_prime(
  data = all_sports_tidy,
  method = "actual",
  threshold_pct = 70.0,
  games_pct_threshold = 100.0
)

# Step 5: Identify primes using spline predictions
spline_primes <- identify_prime(
  data = trajectories,
  method = "predicted", 
  threshold_pct = 70.0,
  games_pct_threshold = 100.0
)

# View results
head(actual_primes)
head(spline_primes)
```

---

## League-Specific Analysis

The package includes dedicated build functions for each league:

```r
# Build specific league datasets (these create the included data)
# These functions are used internally to create the package data

# Available leagues:
# - build_all_sports() - Combined dataset
# - build_chess()      - Chess player data  
# - build_nba()        - NBA statistics
# - build_nfl()        - NFL performance data
# - build_mlb()        - MLB statistics
# - build_mls()        - MLS soccer data
# - build_nhl()        - NHL hockey stats
# - build_nwsl()       - NWSL soccer data
# - build_pwhl()       - PWHL hockey data
# - build_wnba()       - WNBA statistics

# Example: Analyze just NBA data
data("nba_tidy")
nba_analysis <- identify_prime(nba_tidy, method = "actual")

# Compare across leagues
data("all_sports_tidy")
league_comparison <- all_sports_tidy %>%
  group_by(league) %>%
  summarise(
    avg_peak_age = mean(age[performance_metric == max(performance_metric)], na.rm = TRUE),
    max_performance = max(performance_metric, na.rm = TRUE),
    player_count = n_distinct(id)
  )
```

---

## Function Parameters

### `identify_prime()`
- **data**: Player performance dataset
- **method**: "actual" (raw data) or "predicted" (spline-fitted)  
- **threshold_pct**: Performance threshold for prime boundaries (default: 70%)
- **games_pct_threshold**: Minimum games played requirement (default: 100%)

### `calculate_prime_quality_index()`
- **player_data**: Processed player dataset
- **nfl_by_position**: TRUE to analyze NFL players by position
- **tier_method**: "percentile" for percentile-based tiers

### `calculate_career_quality_index()`
- **player_data**: Full player dataset with career data
- **nfl_by_position**: FALSE for overall analysis
- **tier_method**: "percentile" for percentile-based tiers
- **exclude_positions**: Vector of positions to exclude (e.g., c("OL", "SPEC"))
- **min_seasons**: Minimum seasons required for inclusion

---

## License

MIT License - Copyright (c) 2025 Jacob Berlin and Eli Vatsaas

---

**🔗 Explore the findings interactively with our [Dashboard](https://github.com/elivatsaas/peakPerformR-dashboard)**
