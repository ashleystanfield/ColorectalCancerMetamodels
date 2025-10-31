# Technical Documentation

## Table of Contents
1. [Architecture Overview](#architecture-overview)
2. [Data Structures](#data-structures)
3. [Machine Learning Models](#machine-learning-models)
4. [Core Algorithms](#core-algorithms)
5. [Demographic Weighting System](#demographic-weighting-system)
6. [Validation Logic](#validation-logic)
7. [Performance Optimization](#performance-optimization)
8. [API Reference](#api-reference)
9. [Database Schema](#database-schema)
10. [Deployment Guide](#deployment-guide)

---

## Architecture Overview

### Application Structure

The application follows a modular Shiny architecture with clear separation of concerns:

```
┌─────────────────────────────────────────┐
│           User Interface (UI)           │
│  ┌─────────────────────────────────┐   │
│  │  Single Population Mode         │   │
│  │  Multi-Population Mode          │   │
│  │  Results Visualization          │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│         Server Logic (Server)           │
│  ┌─────────────────────────────────┐   │
│  │  Reactive Values Management     │   │
│  │  Event Handlers                 │   │
│  │  Computation Engine             │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│        Core Functions Layer             │
│  ┌─────────────────────────────────┐   │
│  │  Data Preparation               │   │
│  │  Model Training                 │   │
│  │  Prediction Generation          │   │
│  │  Validation Functions           │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│           Data Layer                    │
│  ┌─────────────────────────────────┐   │
│  │  Training Data (CSV)            │   │
│  │  Pre-computed Models (RDS)      │   │
│  │  Model Cache                    │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

### Key Components

#### 1. UI Layer
- **Global Configuration Panel**: Manages population-wide settings
- **Scenario Configuration**: Dynamic UI generation for multiple scenarios
- **Demographics Panel**: Collapsible demographic input controls
- **Results Display**: DataTables for interactive result viewing
- **Multi-Population Interface**: Recursive panel generation for sub-populations

#### 2. Server Layer
- **Reactive Values**: State management for scenarios, demographics, and results
- **Event Handlers**: Button clicks, input changes, validation triggers
- **Progress Tracking**: Modal dialogs and progress indicators
- **Download Handlers**: CSV export with comprehensive data

#### 3. Core Functions Layer
- **Data Preparation**: CSV loading, transformation, and reshaping
- **Model Training**: 180 separate models per outcome type (540 total)
- **Prediction Engine**: Weighted prediction aggregation
- **Validation**: Parameter validation and constraint checking

---

## Data Structures

### Input Data Format

#### Training Data Files
Each training data file contains:
- **Rows**: Different screening parameter combinations
- **Columns**: 9 input parameters + 180 person-specific outcomes

```
Structure:
[FIT_before] [FIT_during] [FIT_after] [COLON_before] [COLON_during] [COLON_after] 
[DIAG_before] [DIAG_during] [DIAG_after] [P_1] [P_2] ... [P_180]
```

#### Long Format Transformation
```r
# Original: Wide format
# FIT_before, FIT_during, ..., P_1, P_2, ..., P_180

# Transformed: Long format
# FIT_before, FIT_during, ..., Person, Outcome
# 8, 10, 9, ..., P_1, 45.2
# 8, 10, 9, ..., P_2, 42.1
```

### Mapping DataFrame

The `mapping_df` creates a systematic mapping of 180 persons to demographic groups:

```r
mapping_df <- expand.grid(
  Gender = c("male", "female"),           # 2 categories
  Race = c("white", "black", "other"),    # 3 categories
  AgeGroup = c("45-49", "50-54", "55-59", # 6 categories
               "60-64", "65-69", "70-74")
)
# Total: 2 × 3 × 6 = 36 combinations
# Each combination assigned 5 persons (180 / 36 = 5)
```

### Results Data Structure

```r
results_list <- list(
  baseline = list(
    cca = numeric,              # Cancer cases
    lyl = numeric,              # Life years lost
    cd = numeric,               # Cancer deaths
    model_type = character,     # ML model used
    total_pop = numeric,        # Population size
    scenario_name = character,  # Scenario identifier
    parameters = list(          # All input parameters
      total_pop, male_pct, female_pct,
      white_pct, black_pct, other_pct,
      age_45_49, age_50_54, age_55_59,
      age_60_64, age_65_69, age_70_74,
      fit_before, fit_during, fit_after,
      colon_before, colon_during, colon_after,
      diag_before, diag_during, diag_after,
      model_type
    )
  ),
  scenario_1 = list(...),
  scenario_2 = list(...),
  ...
)
```

---

## Machine Learning Models

### Model Implementation Details

#### 1. Linear Regression
```r
train_lm_models <- function(data, outcome, progress_callback = NULL)
  Formula: outcome ~ FIT_before + FIT_during + FIT_after + 
                     COLON_before + COLON_during + COLON_after + 
                     DIAG_before + DIAG_during + DIAG_after
  Method: lm()
  Minimum data points: 5
  Training time: ~15 seconds per outcome type
```

**Advantages**: 
- Fastest training and prediction
- Interpretable coefficients
- Low computational requirements

**Limitations**: 
- Assumes linear relationships
- No interaction terms

#### 2. Decision Tree (CART)
```r
train_tree_models <- function(data, outcome, progress_callback = NULL)
  Method: caret::train with method = "rpart"
  Hyperparameters: tuneLength = 3
  Cross-validation: 3-fold CV
  Minimum data points: 10
  Training time: ~45 seconds per outcome type
```

**Advantages**: 
- Handles non-linear relationships
- Automatic feature interaction detection
- Robust to outliers

**Limitations**: 
- Can overfit without proper tuning
- High variance

#### 3. Random Forest
```r
train_rf_models <- function(data, outcome, progress_callback = NULL)
  Method: caret::train with method = "rf"
  Parameters: ntree = 50, tuneLength = 2
  Cross-validation: 3-fold CV
  Minimum data points: 15
  Training time: ~90 seconds per outcome type
```

**Advantages**: 
- Excellent prediction accuracy
- Handles non-linearity and interactions
- Reduces overfitting through ensemble

**Limitations**: 
- Computationally intensive
- Less interpretable
- Longer training time

#### 4. Support Vector Regression (SVR)
```r
train_svr_models <- function(data, outcome, progress_callback = NULL)
  Method: caret::train with method = "svmRadial"
  Kernel: Radial basis function (RBF)
  Hyperparameters: tuneLength = 2
  Cross-validation: 3-fold CV
  Minimum data points: 20
  Training time: ~120 seconds per outcome type
```

**Advantages**: 
- Handles high-dimensional data
- Effective with non-linear patterns
- Good generalization

**Limitations**: 
- Computationally expensive
- Sensitive to parameter tuning
- Requires more training data

#### 5. Lasso Regression (L1 Regularization)
```r
train_lasso_models <- function(data, outcome, progress_callback = NULL)
  Method: caret::train with method = "glmnet"
  Alpha: 1 (pure Lasso)
  Lambda grid: 10^seq(-3, 0, length = 5)
  Cross-validation: 3-fold CV
  Minimum data points: 10
  Training time: ~60 seconds per outcome type
```

**Advantages**: 
- Automatic feature selection
- Prevents overfitting
- Handles multicollinearity

**Limitations**: 
- Assumes linear relationships
- May underfit if regularization too strong

#### 6. Ridge Regression (L2 Regularization)
```r
train_ridge_models <- function(data, outcome, progress_callback = NULL)
  Method: caret::train with method = "glmnet"
  Alpha: 0 (pure Ridge)
  Lambda grid: 10^seq(-3, 0, length = 5)
  Cross-validation: 3-fold CV
  Minimum data points: 10
  Training time: ~50 seconds per outcome type
```

**Advantages**: 
- Handles multicollinearity
- Stable predictions
- All features retained

**Limitations**: 
- Assumes linear relationships
- No feature selection

### Model Training Pipeline

```r
# For each outcome type (CCA, LYL, CD):
#   For each person (P_1 to P_180):
#     1. Filter data for specific person
#     2. Check minimum data requirement
#     3. Train model with cross-validation
#     4. Store model or create dummy fallback
#     5. Update progress callback

# Total models per analysis:
# 3 outcome types × 180 persons = 540 models
```

### Pre-computed Model Caching

To improve performance, models can be pre-computed and saved:

```r
# Training and saving
cca_models <- train_lm_models(data$cca, "CCA")
lyl_models <- train_lm_models(data$lyl, "LYL")
cd_models <- train_lm_models(data$cd, "CD")

models <- list(cca = cca_models, lyl = lyl_models, cd = cd_models)
saveRDS(models, "models_linear_regression.rds")

# Loading
load_precomputed_model <- function(model_type) {
  filename <- switch(model_type,
    "Linear Regression" = "models_linear_regression.rds",
    "Decision Tree" = "models_decision_tree.rds",
    # ...
  )
  readRDS(filename)
}
```

---

## Core Algorithms

### Demographic Weighting Algorithm

The weighting system distributes the total population across 180 person-specific models based on demographic composition:

```r
compute_demographic_weights <- function(scenario_inputs) {
  # Step 1: Convert percentages to proportions
  gender <- c(
    male = scenario_inputs$male_pct / 100,
    female = scenario_inputs$female_pct / 100
  )
  race <- c(
    white = scenario_inputs$white_pct / 100,
    black = scenario_inputs$black_pct / 100,
    other = scenario_inputs$other_pct / 100
  )
  age <- c(
    "45-49" = scenario_inputs$age_45_49 / 100,
    "50-54" = scenario_inputs$age_50_54 / 100,
    # ... all age groups
  )
  
  # Step 2: Initialize weight vector (180 elements)
  weights_vec <- numeric(180)
  
  # Step 3: Iterate through all demographic combinations
  for (g in names(gender)) {
    for (r in names(race)) {
      for (a in names(age)) {
        # Calculate sub-population size
        subpop <- total_pop * gender[g] * race[r] * age[a]
        
        # Find matching persons in mapping_df
        matches <- mapping_df %>%
          filter(Gender == g, Race == r, AgeGroup == a)
        
        # Distribute sub-population equally among matching persons
        if (nrow(matches) > 0) {
          per_capita <- subpop / nrow(matches)
          weights_vec[matches$P_values] <- 
            weights_vec[matches$P_values] + per_capita
        }
      }
    }
  }
  
  return(round(weights_vec, 2))
}
```

**Example Calculation**:
```
Total Population: 100,000
Male: 49%, Female: 51%
White: 60%, Black: 13%, Other: 27%
Age 45-49: 17%, Age 50-54: 17%, etc.

Sub-population (Male, White, 45-49):
= 100,000 × 0.49 × 0.60 × 0.17
= 4,998 persons

This sub-population maps to 5 persons in mapping_df
Each person gets: 4,998 / 5 = 999.6 persons
```

### Prediction Aggregation Algorithm

```r
predict_total <- function(models, input_data, weights) {
  # Step 1: Generate predictions for all 180 persons
  predictions <- sapply(paste0("P_", 1:180), function(person) {
    model <- models[[person]]
    pred <- safe_model_predict(model, input_data)
    return(pred)
  })
  
  # Step 2: Weight predictions by demographic distribution
  weighted_total <- sum(predictions * weights, na.rm = TRUE)
  
  # Step 3: Round to appropriate precision
  return(round(weighted_total, 2))
}

# Applied to each outcome:
cca_total <- predict_total(trained_models$cca, input_data, weights)
lyl_total <- predict_total(trained_models$lyl, input_data, weights)
cd_total <- predict_total(trained_models$cd, input_data, weights)
```

### Safe Prediction Wrapper

```r
safe_model_predict <- function(model, newdata) {
  tryCatch({
    if (inherits(model, "lm")) {
      predict(model, newdata = newdata)
    } else if (inherits(model, "train")) {
      predict(model, newdata = newdata)
    } else {
      0  # Fallback for dummy models
    }
  }, error = function(e) {
    0  # Return 0 on any prediction error
  })
}
```

---

## Demographic Weighting System

### Mapping Logic

The system uses a deterministic mapping that assigns 5 consecutive persons to each demographic combination:

```r
create_mapping_df <- function() {
  genders <- c("male", "female")
  races <- c("white", "black", "other")
  ages <- c("45-49", "50-54", "55-59", "60-64", "65-69", "70-74")
  
  # Create all 36 combinations (2 × 3 × 6)
  combinations <- expand.grid(
    Gender = genders,
    Race = races,
    AgeGroup = ages,
    stringsAsFactors = FALSE
  )
  
  # Assign P_values sequentially
  # P_1 to P_5: Male, White, 45-49
  # P_6 to P_10: Male, White, 50-54
  # ...
  # P_176 to P_180: Female, Other, 70-74
  combinations$P_values <- rep(1:180, length.out = nrow(combinations))
  
  return(combinations)
}
```

### Weight Distribution Example

For a population with specific demographics:

```
Population: 1,000,000
Demographics:
- Male: 49% (490,000)
- Female: 51% (510,000)
- White: 60% (600,000)
- Black: 13% (130,000)
- Other: 27% (270,000)
- Age 45-49: 17% (170,000)

Sub-population (Male, White, 45-49):
= 1,000,000 × 0.49 × 0.60 × 0.17 = 49,980 persons

Mapped to persons P_1 through P_5:
Each person weight = 49,980 / 5 = 9,996

When predicting outcomes:
- P_1 predicts: 45.2 cases
- P_2 predicts: 46.1 cases
- P_3 predicts: 44.8 cases
- P_4 predicts: 45.5 cases
- P_5 predicts: 45.3 cases

Weighted contribution from this sub-population:
= (45.2 + 46.1 + 44.8 + 45.5 + 45.3) × 9,996
= 226.9 × 9,996
= 2,268,597 cases (for this demographic group)
```

---

## Validation Logic

### Parameter Validation

#### Screening Parameter Constraints

The validation system enforces logical temporal relationships:

```r
validate_screening_parameters <- function(
  fit_before, fit_during, fit_after,
  colon_before, colon_during, colon_after,
  diag_before, diag_during, diag_after
) {
  errors <- c()
  
  # Rule: Before < After < During (for all screening types)
  
  # FIT validation
  if (fit_before >= fit_during) {
    errors <- c(errors, "FIT Before must be less than FIT During")
  }
  if (fit_after >= fit_during) {
    errors <- c(errors, "FIT After must be less than FIT During")
  }
  if (fit_before >= fit_after) {
    errors <- c(errors, "FIT Before must be less than FIT After")
  }
  
  # Similar logic for COLON and DIAG parameters...
  
  return(errors)
}
```

**Valid Example**:
```
FIT Before: 8%  →  FIT After: 9%  →  FIT During: 10%  ✓
```

**Invalid Example**:
```
FIT Before: 8%  →  FIT After: 11%  →  FIT During: 10%  ✗
(After > During violates constraint)
```

#### Demographic Validation

```r
# Gender validation
gender_total <- male_pct + female_pct
if (gender_total != 100) {
  show_error("Gender percentages must sum to 100%")
}

# Race validation
race_total <- white_pct + black_pct + other_pct
if (race_total != 100) {
  show_error("Race percentages must sum to 100%")
}

# Age validation
age_total <- age_45_49 + age_50_54 + age_55_59 + 
             age_60_64 + age_65_69 + age_70_74
if (age_total != 100) {
  show_error("Age group percentages must sum to 100%")
}
```

### Sub-Population Validation

For multi-population analysis:

```r
validate_subpopulations <- function(subpop_data, input_mode, total_population) {
  errors <- c()
  
  # Check for empty names
  empty_names <- which(is.na(subpop_data$name) | subpop_data$name == "")
  if (length(empty_names) > 0) {
    errors <- c(errors, paste("Sub-population", empty_names, 
                              "must have a name"))
  }
  
  # Check for duplicate names
  if (any(duplicated(subpop_data$name))) {
    errors <- c(errors, "Sub-population names must be unique")
  }
  
  if (input_mode == "percent") {
    # Percentage mode: must sum to 100%
    total_percent <- sum(subpop_data$value, na.rm = TRUE)
    if (abs(total_percent - 100) > 0.01) {
      errors <- c(errors, paste("Total percentages must equal 100%.",
                                "Current total:", total_percent, "%"))
    }
  } else {
    # Absolute mode: must sum to total population
    total_absolute <- sum(subpop_data$value, na.rm = TRUE)
    if (total_absolute != total_population) {
      errors <- c(errors, paste("Total counts must equal total population.",
                                "Current:", total_absolute,
                                "Expected:", total_population))
    }
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}
```

---

## Performance Optimization

### Model Caching Strategy

```r
# Initialize cache
model_cache <- reactiveVal(list())

# Cache key generation
cache_key <- paste(model_type, digest::digest(data), sep = "_")

# Check cache before training
cached_models <- model_cache()
if (!is.null(cached_models[[cache_key]])) {
  return(cached_models[[cache_key]])
}

# After training, store in cache
cached_models[[cache_key]] <- trained_models
model_cache(cached_models)
```

### Pre-computed Models

Pre-computing models dramatically improves performance:

**Without Pre-computed Models**:
- Linear Regression: 30-60 seconds
- Random Forest: 2-3 minutes
- SVR: 2-4 minutes

**With Pre-computed Models**:
- All models: 5-10 seconds (loading from disk)

### Debouncing Baseline Calculations

To prevent excessive recalculation when users adjust sliders:

```r
# Create debounced reactive
baseline_trigger <- debounce(baseline_inputs_reactive, 1000)

# Only recalculate after 1 second of inactivity
observeEvent(baseline_trigger(), {
  # Recalculate baseline
})
```

### Progress Tracking

```r
withProgress(message = 'Training models...', value = 0, {
  progress_callback <- function(progress, detail) {
    incProgress(progress/3, detail = detail)
  }
  
  # CCA models
  setProgress(0, detail = "Training Cancer Cases models...")
  cca_models <- train_lm_models(data$cca, "CCA", progress_callback)
  
  # LYL models
  setProgress(0.33, detail = "Training Life Years Lost models...")
  lyl_models <- train_lm_models(data$lyl, "LYL", progress_callback)
  
  # CD models
  setProgress(0.66, detail = "Training Cancer Deaths models...")
  cd_models <- train_lm_models(data$cd, "CD", progress_callback)
})
```

---

## API Reference

### Core Functions

#### data_prep()
```r
data_prep(file_path, outcome_name)

Parameters:
  file_path (character): Path to CSV file
  outcome_name (character): Name for outcome column (CCA, LYL, or CD)

Returns:
  data.frame: Long-format data with Person and Outcome columns

Example:
  cca_data <- data_prep("cancer_cases.csv", "CCA")
```

#### create_mapping_df()
```r
create_mapping_df()

Returns:
  data.frame: Mapping of 180 persons to demographic groups
  
Columns:
  - Gender: "male" or "female"
  - Race: "white", "black", or "other"
  - AgeGroup: "45-49", "50-54", ..., "70-74"
  - P_values: 1 to 180
```

#### compute_demographic_weights()
```r
compute_demographic_weights(scenario_inputs)

Parameters:
  scenario_inputs (list): Contains demographic percentages and total population

Returns:
  numeric vector (length 180): Weight for each person

Example:
  weights <- compute_demographic_weights(list(
    total_pop = 100000,
    male_pct = 49,
    female_pct = 51,
    # ... other demographics
  ))
```

#### validate_screening_parameters()
```r
validate_screening_parameters(
  fit_before, fit_during, fit_after,
  colon_before, colon_during, colon_after,
  diag_before, diag_during, diag_after
)

Returns:
  character vector: Error messages (empty if valid)

Example:
  errors <- validate_screening_parameters(8, 10, 9, 48, 55, 52, 7, 10, 8)
  if (length(errors) > 0) {
    print(errors)
  }
```

### Model Training Functions

All model training functions follow this signature:

```r
train_*_models(data, outcome, progress_callback = NULL)

Parameters:
  data (data.frame): Training data in long format
  outcome (character): "CCA", "LYL", or "CD"
  progress_callback (function): Optional callback for progress updates

Returns:
  list: Named list of 180 trained models (P_1 through P_180)

Available functions:
  - train_lm_models()
  - train_tree_models()
  - train_rf_models()
  - train_svr_models()
  - train_lasso_models()
  - train_ridge_models()
```

### Prediction Functions

#### safe_model_predict()
```r
safe_model_predict(model, newdata)

Parameters:
  model: Trained model object
  newdata (data.frame): Input data for prediction

Returns:
  numeric: Predicted value (0 on error)

Features:
  - Handles multiple model types (lm, train)
  - Error-safe with fallback to 0
  - Prevents application crashes from model failures
```

---

## Database Schema

While the application doesn't use a traditional database, it manages data through structured objects:

### In-Memory Data Structures

#### Reactive Values
```r
# Single population
scenario_results <- reactiveVal(list())
scenario_names <- reactiveVal(c("Scenario 1", ...))
model_cache <- reactiveVal(list())
demographics_visible <- reactiveVal(TRUE)
baseline_computing <- reactiveVal(FALSE)

# Multi-population
subpop_definitions <- reactiveVal(data.frame())
subpops_validated <- reactiveVal(FALSE)
subpop_results <- reactiveVal(list())
subpop_states <- reactiveValues()
```

#### Results Storage Schema
```r
results <- list(
  baseline = list(
    cca = 12345.67,
    lyl = 54321.98,
    cd = 987.65,
    model_type = "Linear Regression",
    total_pop = 100000,
    scenario_name = "Usual Care (Baseline)",
    parameters = list(
      total_pop = 100000,
      male_pct = 49,
      female_pct = 51,
      white_pct = 60,
      black_pct = 13,
      other_pct = 27,
      age_45_49 = 17,
      age_50_54 = 17,
      age_55_59 = 16,
      age_60_64 = 16,
      age_65_69 = 16,
      age_70_74 = 18,
      fit_before = 8,
      fit_during = 8,
      fit_after = 8,
      colon_before = 48,
      colon_during = 48,
      colon_after = 48,
      diag_before = 7,
      diag_during = 7,
      diag_after = 7,
      model_type = "Linear Regression"
    )
  ),
  scenario_1 = list(...),
  scenario_2 = list(...)
)
```

### Export Data Format

CSV exports include all parameters and results:

```
Scenario_Name,Model_Type,Total_Population,Cancer_Cases,Life_Years_Lost,
Cancer_Deaths,Male_Percent,Female_Percent,White_Percent,Black_Percent,
Other_Race_Percent,Age_45_49_Percent,Age_50_54_Percent,Age_55_59_Percent,
Age_60_64_Percent,Age_65_69_Percent,Age_70_74_Percent,FIT_Before,
FIT_During,FIT_After,Colonoscopy_Before,Colonoscopy_During,
Colonoscopy_After,Diagnostic_Before,Diagnostic_During,Diagnostic_After,
Analysis_Date,Scenario_Type
```

---

## Deployment Guide

### Local Deployment

#### Development Environment
```r
# Install dependencies
install.packages(c("shiny", "readr", "dplyr", "tidyr", "purrr", 
                   "caret", "glmnet", "DT", "shinyBS", "digest",
                   "randomForest", "e1071", "rpart", "kernlab"))

# Run application
shiny::runApp("path/to/app.R")
```

#### Production Environment
```r
# Run on specific host and port
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```

### Shiny Server Deployment

1. **Install Shiny Server**
```bash
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb
sudo gdebi shiny-server-1.5.20.1002-amd64.deb
```

2. **Deploy Application**
```bash
sudo mkdir /srv/shiny-server/colorectal-cancer-metamodel
sudo cp -R /path/to/app/* /srv/shiny-server/colorectal-cancer-metamodel/
sudo chown -R shiny:shiny /srv/shiny-server/colorectal-cancer-metamodel
```

3. **Configure Shiny Server** (`/etc/shiny-server/shiny-server.conf`)
```
server {
  listen 3838;
  
  location /colorectal-cancer-metamodel {
    app_dir /srv/shiny-server/colorectal-cancer-metamodel;
    log_dir /var/log/shiny-server;
    
    # Increase timeouts for long-running computations
    app_idle_timeout 300;
    app_init_timeout 120;
  }
}
```

4. **Restart Server**
```bash
sudo systemctl restart shiny-server
```

### Docker Deployment

**Dockerfile**:
```dockerfile
FROM rocker/shiny:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'readr', 'dplyr', 'tidyr', 'purrr', \
    'caret', 'glmnet', 'DT', 'shinyBS', 'digest', \
    'randomForest', 'e1071', 'rpart', 'kernlab'))"

# Copy application files
COPY app.R /srv/shiny-server/
COPY *.csv /srv/shiny-server/
COPY *.rds /srv/shiny-server/

# Expose port
EXPOSE 3838

# Run application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]
```

**Build and Run**:
```bash
docker build -t colorectal-cancer-metamodel .
docker run -p 3838:3838 colorectal-cancer-metamodel
```

### Cloud Deployment (shinyapps.io)

```r
# Install rsconnect
install.packages("rsconnect")

# Configure account
rsconnect::setAccountInfo(
  name = "your-account",
  token = "your-token",
  secret = "your-secret"
)

# Deploy application
rsconnect::deployApp(
  appDir = "path/to/app",
  appName = "colorectal-cancer-metamodel",
  forceUpdate = TRUE
)
```

### Performance Tuning

#### Memory Optimization
```r
# Limit concurrent users
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB max upload

# Control worker processes
options(shiny.sanitize.errors = FALSE)
```

#### Caching Strategy
```r
# Use memoise for expensive operations
library(memoise)
cached_train_models <- memoise(train_models_with_progress)
```

#### Load Balancing
For high-traffic deployments, use multiple Shiny Server instances behind a load balancer (nginx, HAProxy).

---

## Troubleshooting

### Common Technical Issues

#### Memory Issues
**Symptom**: Application crashes with large populations
**Solution**: 
- Increase server RAM
- Use pre-computed models
- Implement pagination for results

#### Model Loading Failures
**Symptom**: "Model training failed" errors
**Solution**:
- Check data file integrity
- Verify sufficient data points for model type
- Use fallback to Linear Regression

#### Network Timeouts
**Symptom**: Application unresponsive during long computations
**Solution**:
- Increase app_idle_timeout in Shiny Server config
- Implement asynchronous processing
- Add progress indicators

#### Data File Issues
**Symptom**: "File not found" errors
**Solution**:
- Verify CSV files are in correct directory
- Check file permissions
- Validate CSV format and column names

---

## Version History

### Version 1.0.0
- Initial release
- Single and multi-population analysis
- Six ML model options
- Pre-computed model support
- Comprehensive validation
- Enhanced export capabilities

---

## Contributing to Technical Documentation

When contributing code changes, please update this documentation:

1. **New Functions**: Add to API Reference
2. **Algorithm Changes**: Update Core Algorithms section
3. **Data Structure Changes**: Update Data Structures section
4. **Performance Improvements**: Document in Performance Optimization
5. **Bug Fixes**: Add to Troubleshooting section

---

## References

- [Shiny Documentation](https://shiny.rstudio.com/)
- [caret Package Guide](https://topepo.github.io/caret/)
- [Random Forest Theory](https://www.stat.berkeley.edu/~breiman/RandomForests/)
- [Support Vector Machines](https://scikit-learn.org/stable/modules/svm.html)
- [Regularization in Regression](https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html)

---

*Last Updated: 2025-10-31*
