# Technical Documentation: Metamodel Decision Tool

## Table of Contents
1. [Architecture Overview](#architecture-overview)
2. [Data Model](#data-model)
3. [Machine Learning Implementation](#machine-learning-implementation)
4. [Algorithm Specifications](#algorithm-specifications)
5. [Function Reference](#function-reference)
6. [Performance Optimization](#performance-optimization)
7. [Validation Framework](#validation-framework)
8. [Customization Guide](#customization-guide)

## Architecture Overview

### System Design
```
┌─────────────────────────────────────────────────────────────────┐
│                    Shiny Application Layer                      │
├─────────────────────────────────────────────────────────────────┤
│  UI Components          │  Server Logic                         │
│  ├── Global Config      │  ├── Reactive Values                  │
│  ├── Scenario Panels    │  ├── Model Training                   │
│  ├── Results Display    │  ├── Prediction Engine                │
│  └── Download Handler   │  └── Validation Logic                 │
├─────────────────────────────────────────────────────────────────┤
│                    Data Processing Layer                        │
│  ├── CSV Parsing        │  ├── Demographic Mapping              │
│  ├── Data Validation    │  ├── Weight Calculation               │
│  └── Format Transform   │  └── Result Aggregation               │
├─────────────────────────────────────────────────────────────────┤
│                 Machine Learning Layer                         │
│  ├── Model Factory      │  ├── Training Pipeline                │
│  ├── Prediction Engine  │  ├── Model Caching                    │
│  └── Error Handling     │  └── Performance Monitoring           │
├─────────────────────────────────────────────────────────────────┤
│                      Data Storage Layer                        │
│  ├── Training Data      │  ├── Model Cache                      │
│  ├── Mapping Tables     │  └── Results Storage                  │
└─────────────────────────────────────────────────────────────────┘
```

### Core Technologies
- **Frontend**: Shiny UI with shinyBS for enhanced components
- **Backend**: R with reactive programming paradigm
- **ML Framework**: caret package with multiple algorithm backends
- **Data Processing**: tidyverse ecosystem (dplyr, tidyr, purrr)
- **Validation**: Custom parameter validation framework

## Data Model

### Input Data Structure

#### Training Data Format
Each CSV file contains:
```
Row Structure: [FIT_before, FIT_during, FIT_after, COLON_before, COLON_during, 
               COLON_after, DIAG_before, DIAG_during, DIAG_after, P_1, P_2, ..., P_180]
```

#### Person-Type Mapping
```r
create_mapping_df <- function() {
  genders <- c("male", "female")                    # 2 categories
  races <- c("white", "black", "other")            # 3 categories  
  ages <- c("45-49", "50-54", "55-59", 
            "60-64", "65-69", "70-74")             # 6 categories
  
  # Total combinations: 2 × 3 × 6 = 36 unique profiles
  # Replicated 5 times: 36 × 5 = 180 person-types
}
```

### Data Flow Pipeline

#### 1. Data Ingestion
```r
data_prep <- function(file_path, outcome_name) {
  # Read CSV with proper column structure
  # Transform wide to long format
  # Add row identifiers
  # Return structured data frame
}
```

#### 2. Demographic Weighting
```r
compute_demographic_weights <- function(scenario_inputs) {
  # Calculate population weights for each of 180 person-types
  # Based on user-specified demographic distributions
  # Returns weight vector for prediction aggregation
}
```

#### 3. Prediction Aggregation
```r
predict_total <- function(models) {
  # Generate predictions for all 180 person-types
  # Apply demographic weights
  # Sum to population-level estimate
}
```

## Machine Learning Implementation

### Model Training Architecture

#### Training Pipeline
```r
train_models_with_progress <- function(data, model_type) {
  # 1. Check model cache for existing trained models
  # 2. Initialize progress tracking
  # 3. Train models for each outcome (CCA, LYL, CD)
  # 4. Cache results for future use
  # 5. Return trained model objects
}
```

#### Individual Model Training
```r
train_[model]_models <- function(data, outcome, progress_callback) {
  # For each of 180 person-types:
  #   1. Extract person-specific data
  #   2. Validate sufficient data points
  #   3. Train model with cross-validation
  #   4. Store model or create dummy fallback
  #   5. Update progress indicator
}
```

### Model Specifications

#### Linear Regression
```r
# Implementation: Base R lm()
# Formula: outcome ~ FIT_before + FIT_during + FIT_after + 
#                   COLON_before + COLON_during + COLON_after + 
#                   DIAG_before + DIAG_during + DIAG_after
# Minimum data: 5 observations per person-type
# Training time: ~15 seconds
```

#### Decision Tree
```r
# Implementation: caret with rpart method
# Cross-validation: 3-fold CV
# Tuning: tuneLength = 3
# Minimum data: 10 observations per person-type
# Training time: ~45 seconds
```

#### Random Forest
```r
# Implementation: caret with rf method
# Trees: ntree = 50 (optimized for speed)
# Cross-validation: 3-fold CV
# Tuning: tuneLength = 2
# Minimum data: 15 observations per person-type
# Training time: ~90 seconds
```

#### Support Vector Regression
```r
# Implementation: caret with svmRadial method
# Kernel: Radial basis function
# Cross-validation: 3-fold CV
# Tuning: tuneLength = 2
# Minimum data: 20 observations per person-type
# Training time: ~120 seconds
```

#### Lasso Regression
```r
# Implementation: caret with glmnet method
# Alpha: 1 (pure Lasso)
# Lambda grid: 10^seq(-3, 0, length = 5)
# Cross-validation: 3-fold CV
# Minimum data: 10 observations per person-type
# Training time: ~60 seconds
```

#### Ridge Regression
```r
# Implementation: caret with glmnet method
# Alpha: 0 (pure Ridge)
# Lambda grid: 10^seq(-3, 0, length = 5)
# Cross-validation: 3-fold CV
# Minimum data: 10 observations per person-type
# Training time: ~50 seconds
```

## Algorithm Specifications

### Prediction Algorithm
```r
Algorithm: Population-Level Outcome Prediction
Input: Trained models (M), Scenario parameters (P), Demographic weights (W)
Output: Aggregated outcome predictions

1. FOR each person-type i in [1, 180]:
   a. Extract model Mi for current outcome
   b. Generate prediction Pi using parameters P
   c. Apply demographic weight Wi
   
2. Aggregate: Total = Σ(Pi × Wi) for i in [1, 180]

3. Return rounded total outcome
```

### Caching Algorithm
```r
Algorithm: Model Cache Management
Input: Model type (T), Data digest (D)
Output: Cached or newly trained models

1. Generate cache key: K = hash(T, D)
2. IF cache contains key K:
   a. Return cached models
3. ELSE:
   a. Train new models
   b. Store in cache with key K
   c. Return trained models
```

### Validation Algorithm
```r
Algorithm: Parameter Validation
Input: Before (B), During (D), After (A) parameters for each screening type
Output: Validation errors or success

FOR each screening type (FIT, COLON, DIAG):
  1. Check: B < D (Before less than During)
  2. Check: A < D (After less than During)  
  3. Check: B < A (Before less than After)
  4. Collect any violations

Return: List of validation errors (empty if valid)
```

## Function Reference

### Core Data Functions

#### `data_prep(file_path, outcome_name)`
**Purpose**: Load and transform training data
**Parameters**:
- `file_path`: Path to CSV training file
- `outcome_name`: Name for outcome column ("CCA", "LYL", "CD")
**Returns**: Long-format data frame with person-type structure

#### `create_mapping_df()`
**Purpose**: Generate demographic mapping table
**Parameters**: None
**Returns**: Data frame mapping person-types to demographic characteristics

#### `compute_demographic_weights(scenario_inputs)`
**Purpose**: Calculate population weights for demographic aggregation
**Parameters**: 
- `scenario_inputs`: List containing demographic percentages
**Returns**: Vector of 180 weights for person-types

### Validation Functions

#### `validate_screening_parameters(...)`
**Purpose**: Validate screening parameter logical consistency
**Parameters**: All 9 screening parameters (before/during/after for each type)
**Returns**: Vector of validation error messages (empty if valid)

#### `get_age_group(age)`
**Purpose**: Convert numeric age to age group category
**Parameters**: 
- `age`: Numeric age value
**Returns**: Factor with age group category

### Model Training Functions

#### `train_[model]_models(data, outcome, progress_callback)`
**Purpose**: Train specific model type for all person-types
**Parameters**:
- `data`: Training data for specific outcome
- `outcome`: Outcome name ("CCA", "LYL", "CD")
- `progress_callback`: Function for progress updates
**Returns**: List of 180 trained models

#### `safe_model_predict(model, newdata)`
**Purpose**: Robust prediction with error handling
**Parameters**:
- `model`: Trained model object
- `newdata`: Data frame with prediction inputs
**Returns**: Numeric prediction (0 if error)

### Utility Functions

#### `get_estimated_time(model_type, data_size)`
**Purpose**: Estimate model training time
**Parameters**:
- `model_type`: String identifying model algorithm
- `data_size`: Number of training observations
**Returns**: Human-readable time estimate

#### `create_dummy_model(method)`
**Purpose**: Generate fallback model for insufficient data
**Parameters**:
- `method`: Model type identifier
**Returns**: Dummy model object with predict capability

## Performance Optimization

### Computational Complexity

#### Training Complexity
| Model | Time Complexity | Space Complexity | Scalability |
|-------|-----------------|------------------|-------------|
| Linear Regression | O(p³) | O(p²) | Excellent |
| Decision Tree | O(n log n × p) | O(n) | Good |
| Random Forest | O(k × n log n × p) | O(k × n) | Moderate |
| SVR | O(n³) | O(n²) | Poor |
| Lasso/Ridge | O(n × p²) | O(p²) | Good |

Where: n = observations, p = parameters, k = trees

#### Memory Management
```r
# Model caching reduces memory usage through:
# 1. Shared model objects across scenarios
# 2. Lazy loading of training data
# 3. Garbage collection after training
# 4. Efficient reactive invalidation
```

### Performance Monitoring

#### Training Time Estimation
```r
base_times <- list(
  "Linear Regression" = 15,     # seconds
  "Decision Tree" = 45,
  "Random Forest" = 90,
  "Support Vector Regression" = 120,
  "Lasso Regression" = 60,
  "Ridge Regression" = 50
)

# Adjusted for data size and system performance
estimated_time = base_time × (data_size / 1000) × system_multiplier
```

#### Cache Hit Optimization
```r
# Cache key generation:
cache_key <- paste(model_type, digest::digest(data), sep = "_")

# Cache hit rate optimization:
# - Identical scenarios reuse cached models
# - Data preprocessing standardization
# - Deterministic random seeds
```

## Validation Framework

### Parameter Validation Rules

#### Screening Rate Constraints
```r
# For each screening type (FIT, COLON, DIAG):
Before_Rate < During_Rate    # Pandemic increases screening
After_Rate < During_Rate     # Recovery reduces from peak
Before_Rate < After_Rate     # Recovery exceeds pre-pandemic

# Numeric constraints:
FIT: [0, 30]           # Percentage bounds
COLON: [30, 70]        # Realistic colonoscopy rates
DIAG: [0, 90]          # Diagnostic procedure rates
```

#### Demographic Validation
```r
# Percentage sum validation:
male_pct + female_pct = 100
white_pct + black_pct + other_pct = 100
sum(all_age_group_pcts) = 100

# Individual bounds:
All percentages: [0, 100]
Population size: >= 1000
```

#### Data Quality Validation
```r
# Training data requirements:
minimum_observations_per_person_type = 5   # For linear models
minimum_observations_per_person_type = 20  # For SVR

# Missing data handling:
if (insufficient_data) {
  use_dummy_model()
} else {
  train_actual_model()
}
```

### Error Handling Strategy

#### Graceful Degradation
```r
# Model training failure hierarchy:
1. Try requested model with full data
2. If insufficient data → use dummy model
3. If training fails → fallback to simpler model
4. If all fails → return zero prediction with warning
```

#### User Feedback
```r
# Validation feedback levels:
Error: Blocks execution, requires user correction
Warning: Allows execution, notifies potential issues  
Info: Provides guidance, no intervention required
Success: Confirms valid configuration
```

## Customization Guide

### Adding New Models

#### 1. Create Training Function
```r
train_newmodel_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > minimum_required) {
        # Implement your model training here
        models[[person]] <- train_your_model(person_data)
      } else {
        models[[person]] <- create_dummy_model("your_method")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("your_method")
    })
    
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i))
    }
  }
  return(models)
}
```

#### 2. Update Model Selection
```r
# In UI:
selectInput("global_model_type", "Model Type:",
            choices = c("Linear Regression",
                       "Decision Tree", 
                       "Random Forest",
                       "Support Vector Regression",
                       "Lasso Regression",
                       "Ridge Regression",
                       "Your New Model"),    # Add here
            selected = "Linear Regression")

# In Server:
your_new_models <- switch(model_type,
                         "Your New Model" = train_newmodel_models(data$cca, "CCA", progress_callback),
                         # ... existing models
                         )
```

### Modifying Demographic Categories

#### 1. Update Mapping Function
```r
create_mapping_df <- function() {
  # Modify these vectors for new categories:
  genders <- c("male", "female", "other")           # Add gender categories
  races <- c("white", "black", "hispanic", "asian", "other")  # Add race categories
  ages <- c("40-44", "45-49", "50-54", "55-59",    # Modify age ranges
            "60-64", "65-69", "70-74", "75-79")
  
  # Update total person-types calculation
  total_combinations <- length(genders) × length(races) × length(ages)
}
```

#### 2. Update UI Components
```r
# Add corresponding sliders/inputs for new categories
# Update validation logic for new percentage constraints
# Modify weight calculation algorithms
```

### Adding New Outcomes

#### 1. Prepare Training Data
```r
# Add new CSV file: edited_INFORMS[new_outcome]_averages_combined.csv
# Update data loading logic in training_data() reactive
```

#### 2. Extend Model Training
```r
# Add training for new outcome in train_models_with_progress()
new_outcome_models <- switch(model_type,
                            "Linear Regression" = train_lm_models(data$new_outcome, "NEW", progress_callback),
                            # ... for all model types
                            )
```

#### 3. Update Results Display
```r
# Modify results structure to include new outcome
# Update comparison table columns
# Extend download functionality
```

### Performance Tuning

#### Model-Specific Optimizations
```r
# Random Forest tuning:
ntree = 50          # Reduce for speed, increase for accuracy
tuneLength = 2      # Reduce for speed, increase for optimization

# Cross-validation tuning:
number = 3          # Reduce for speed, increase for robustness
repeats = 1         # Add repeats for more stable estimates

# Grid search tuning:
tuneGrid = expand.grid(alpha = c(0.1, 1), 
                      lambda = 10^seq(-2, 0, length = 3))  # Reduce grid size
```

#### Memory Optimization
```r
# Large population handling:
if (population > 500000) {
  # Use simpler models automatically
  # Implement progress chunking
  # Add memory monitoring
}
```

---

## Development Notes

### Code Structure Principles
- **Modularity**: Each function has single responsibility
- **Error Resilience**: Graceful handling of edge cases
- **Performance**: Caching and optimization throughout
- **Maintainability**: Clear naming and documentation

### Testing Recommendations
- Unit tests for core functions
- Integration tests for model training
- Performance benchmarks for optimization
- User acceptance testing for UI workflows

### Future Enhancements
- Parallel model training for large populations
- Additional demographic stratifications
- Real-time model performance monitoring
- Advanced visualization capabilities
