library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(glmnet)
library(DT)
library(shinyBS) # For tooltips
library(digest)
library(randomForest)    # ADD THIS - Required for Random Forest predictions
library(e1071)           # ADD THIS - Required for SVR predictions
library(rpart)
library(kernlab)

## ---------------------------
## Data Preparation Functions
## ---------------------------
data_prep <- function(file_path, outcome_name) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  df <- read_csv(file_path, col_names = FALSE, skip = 1)
  input_cols <- c("FIT_before", "FIT_during", "FIT_after",
                  "COLON_before", "COLON_during", "COLON_after",
                  "DIAG_before", "DIAG_during", "DIAG_after")
  person_cols <- paste0("P_", 1:180)
  colnames(df) <- c(input_cols, person_cols)
  df$row_id <- seq_len(nrow(df))
  df_long <- pivot_longer(df, cols = all_of(person_cols),
                          names_to = "Person", values_to = outcome_name)
  return(df_long)
}

# Create mapping_df (this was missing in original code)
create_mapping_df <- function() {
  genders <- c("male", "female")
  races <- c("white", "black", "other")
  ages <- c("45-49", "50-54", "55-59", "60-64", "65-69", "70-74")
  
  # Create all combinations
  combinations <- expand.grid(
    Gender = genders,
    Race = races,
    AgeGroup = ages,
    stringsAsFactors = FALSE
  )
  
  # Assign P_values (1 to 180) to each combination
  combinations$P_values <- rep(1:180, length.out = nrow(combinations))
  
  return(combinations)
}

get_age_group <- function(age) {
  cut(age,
      breaks = c(44, 49, 54, 59, 64, 69, 74),
      labels = c("45-49", "50-54", "55-59", "60-64", "65-69", "70-74"),
      right = TRUE)
}

## ---------------------------
## Parameter Validation Functions
## ---------------------------
validate_screening_parameters <- function(fit_before, fit_during, fit_after,
                                          colon_before, colon_during, colon_after,
                                          diag_before, diag_during, diag_after) {
  errors <- c()
  
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
  
  # COLON validation
  if (colon_before >= colon_during) {
    errors <- c(errors, "COLON Before must be less than COLON During")
  }
  if (colon_after >= colon_during) {
    errors <- c(errors, "COLON After must be less than COLON During")
  }
  if (colon_before >= colon_after) {
    errors <- c(errors, "COLON Before must be less than COLON After")
  }
  
  # DIAG validation
  if (diag_before >= diag_during) {
    errors <- c(errors, "DIAG Before must be less than DIAG During")
  }
  if (diag_after >= diag_during) {
    errors <- c(errors, "DIAG After must be less than DIAG During")
  }
  if (diag_before >= diag_after) {
    errors <- c(errors, "DIAG Before must be less than DIAG After")
  }
  
  return(errors)
}

## ---------------------------
## Sub-Population Validation Functions
## ---------------------------
validate_subpopulations <- function(subpop_data, input_mode, total_population) {
  errors <- c()
  
  if (nrow(subpop_data) == 0) {
    return(list(valid = FALSE, errors = "No sub-populations defined"))
  }
  
  # Check for empty names
  empty_names <- which(is.na(subpop_data$name) | subpop_data$name == "")
  if (length(empty_names) > 0) {
    errors <- c(errors, paste("Sub-population", empty_names, "must have a name"))
  }
  
  # Check for duplicate names
  duplicate_names <- duplicated(subpop_data$name[!is.na(subpop_data$name)])
  if (any(duplicate_names)) {
    errors <- c(errors, "Sub-population names must be unique")
  }
  
  if (input_mode == "percent") {
    # Check percentage validation
    total_percent <- sum(subpop_data$value, na.rm = TRUE)
    if (abs(total_percent - 100) > 0.01) {
      errors <- c(errors, paste("Total percentages must equal 100%. Current total:", round(total_percent, 2), "%"))
    }
    
    # Check individual percentages
    invalid_percent <- which(subpop_data$value <= 0 | subpop_data$value > 100)
    if (length(invalid_percent) > 0) {
      errors <- c(errors, paste("Sub-population", invalid_percent, "percentage must be between 0.1 and 100"))
    }
  } else {
    # Check absolute number validation
    total_absolute <- sum(subpop_data$value, na.rm = TRUE)
    if (total_absolute != total_population) {
      errors <- c(errors, paste("Total sub-population counts must equal total population.",
                                "Current total:", format(total_absolute, big.mark = ","),
                                "Expected:", format(total_population, big.mark = ",")))
    }
    
    # Check individual counts
    invalid_absolute <- which(subpop_data$value <= 0 | subpop_data$value > total_population)
    if (length(invalid_absolute) > 0) {
      errors <- c(errors, paste("Sub-population", invalid_absolute, "count must be between 1 and total population"))
    }
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

## ---------------------------
## Enhanced Model Training Functions with Time Estimation
## ---------------------------
# Time estimation function
get_estimated_time <- function(model_type, data_size) {
  base_times <- list(
    "Linear Regression" = 15,
    "Decision Tree" = 45,
    "Random Forest" = 90,
    "Support Vector Regression" = 120,
    "Lasso Regression" = 60,
    "Ridge Regression" = 50
  )
  
  # Adjust based on data size (rough estimation)
  multiplier <- max(1, data_size / 1000)
  estimated_seconds <- base_times[[model_type]] * multiplier
  
  if (estimated_seconds < 60) {
    return(paste(round(estimated_seconds), "seconds"))
  } else {
    return(paste(round(estimated_seconds / 60, 1), "minutes"))
  }
}

# Create dummy model for fallback
create_dummy_model <- function(method = "lm") {
  if (method == "lm") {
    dummy <- list(coefficients = rep(0, 10), fitted.values = 0)
    class(dummy) <- "lm"
  } else {
    # For caret models, create a simple structure
    dummy <- list(
      method = method,
      finalModel = list(coefficients = rep(0, 10)),
      pred = function(x) rep(0, nrow(x))
    )
    class(dummy) <- "train"
  }
  return(dummy)
}

# Safe prediction wrapper
safe_model_predict <- function(model, newdata) {
  tryCatch({
    if (inherits(model, "lm")) {
      predict(model, newdata = newdata)
    } else if (inherits(model, "train")) {
      predict(model, newdata = newdata)
    } else {
      0
    }
  }, error = function(e) {
    0
  })
}

# Linear Regression Training
train_lm_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 5) {  # Need minimum data points
        formula_str <- paste(outcome, "~ FIT_before + FIT_during + FIT_after + COLON_before + COLON_during + COLON_after + DIAG_before + DIAG_during + DIAG_after")
        models[[person]] <- lm(as.formula(formula_str), data = person_data)
      } else {
        models[[person]] <- create_dummy_model("lm")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("lm")
    })
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

# Decision Tree Training
train_tree_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 10) {  # Need more data for trees
        formula_str <- paste(outcome, "~ FIT_before + FIT_during + FIT_after + COLON_before + COLON_during + COLON_after + DIAG_before + DIAG_during + DIAG_after")
        
        models[[person]] <- train(
          as.formula(formula_str),
          data = person_data,
          method = "rpart",
          tuneLength = 3,
          trControl = trainControl(method = "cv", number = 3, verboseIter = FALSE)
        )
      } else {
        models[[person]] <- create_dummy_model("rpart")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("rpart")
    })
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

# Random Forest Training
train_rf_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 15) {  # Need more data for RF
        formula_str <- paste(outcome, "~ FIT_before + FIT_during + FIT_after + COLON_before + COLON_during + COLON_after + DIAG_before + DIAG_during + DIAG_after")
        
        models[[person]] <- train(
          as.formula(formula_str),
          data = person_data,
          method = "rf",
          ntree = 50,
          tuneLength = 2,  # Reduced for speed
          trControl = trainControl(method = "cv", number = 3, verboseIter = FALSE)
        )
      } else {
        models[[person]] <- create_dummy_model("rf")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("rf")
    })
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

# Support Vector Regression Training
train_svr_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 20) {  # SVR needs more data
        formula_str <- paste(outcome, "~ FIT_before + FIT_during + FIT_after + COLON_before + COLON_during + COLON_after + DIAG_before + DIAG_during + DIAG_after")
        
        models[[person]] <- train(
          as.formula(formula_str),
          data = person_data,
          method = "svmRadial",
          tuneLength = 2,  # Reduced for speed
          trControl = trainControl(method = "cv", number = 3, verboseIter = FALSE)
        )
      } else {
        models[[person]] <- create_dummy_model("svmRadial")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("svmRadial")
    })
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

# Lasso Regression Training
train_lasso_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 10) {
        formula_str <- paste(outcome, "~ FIT_before + FIT_during + FIT_after + COLON_before + COLON_during + COLON_after + DIAG_before + DIAG_during + DIAG_after")
        
        models[[person]] <- train(
          as.formula(formula_str),
          data = person_data,
          method = "glmnet",
          tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-3, 0, length = 5)),  # Reduced grid
          trControl = trainControl(method = "cv", number = 3, verboseIter = FALSE)
        )
      } else {
        models[[person]] <- create_dummy_model("glmnet")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("glmnet")
    })
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

# Ridge Regression Training
train_ridge_models <- function(data, outcome, progress_callback = NULL) {
  models <- list()
  total_persons <- 180
  
  for (i in 1:total_persons) {
    person <- paste0("P_", i)
    tryCatch({
      person_data <- data %>% filter(Person == person)
      
      if (nrow(person_data) > 10) {
        formula_str <- paste(outcome, "~ FIT_before + FIT_during + FIT_after + COLON_before + COLON_during + COLON_after + DIAG_before + DIAG_during + DIAG_after")
        
        models[[person]] <- train(
          as.formula(formula_str),
          data = person_data,
          method = "glmnet",
          tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 0, length = 5)),  # Reduced grid
          trControl = trainControl(method = "cv", number = 3, verboseIter = FALSE)
        )
      } else {
        models[[person]] <- create_dummy_model("glmnet")
      }
    }, error = function(e) {
      models[[person]] <- create_dummy_model("glmnet")
    })
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(i / total_persons, paste("Training person", i, "of", total_persons))
    }
  }
  return(models)
}

## ---------------------------
## Multi-Population Helper Functions
## ---------------------------
# Create sub-population analysis results
create_subpop_summary <- function(subpop_results, subpop_definitions) {
  if (length(subpop_results) == 0) return(NULL)
  
  summary_data <- data.frame()
  
  for (subpop_id in names(subpop_results)) {
    subpop_data <- subpop_results[[subpop_id]]
    subpop_info <- subpop_definitions[subpop_definitions$id == subpop_id, ]
    
    if (nrow(subpop_info) == 0) next
    
    # Add baseline if exists
    if (!is.null(subpop_data[["baseline"]])) {
      baseline <- subpop_data[["baseline"]]
      summary_data <- rbind(summary_data, data.frame(
        SubPopulation = subpop_info$name,
        Population_Size = subpop_info$population,
        Analysis_Type = "Baseline",
        Scenario_Name = baseline$scenario_name,
        Model_Type = baseline$model_type,
        Cancer_Cases = baseline$cca,
        Life_Years_Lost = baseline$lyl,
        Cancer_Deaths = baseline$cd,
        stringsAsFactors = FALSE
      ))
    }
    
    # Add scenarios
    scenario_keys <- names(subpop_data)[grepl("^scenario_", names(subpop_data))]
    for (scenario_key in scenario_keys) {
      scenario <- subpop_data[[scenario_key]]
      summary_data <- rbind(summary_data, data.frame(
        SubPopulation = subpop_info$name,
        Population_Size = subpop_info$population,
        Analysis_Type = "Scenario",
        Scenario_Name = scenario$scenario_name,
        Model_Type = scenario$model_type,
        Cancer_Cases = scenario$cca,
        Life_Years_Lost = scenario$lyl,
        Cancer_Deaths = scenario$cd,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_data)
}

## ---------------------------
## UI Definition
## ---------------------------
ui <- fluidPage(
  titlePanel("Metamodel Decision Tool for Estimating Colorectal Cancer Health Outcomes"),
  
  # Modal for processing notifications
  div(id = "processing-modal", class = "modal", style = "display: none; position: fixed; z-index: 9999; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5);",
      div(class = "modal-content", style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); background-color: #007bff; color: white; padding: 40px; border-radius: 15px; text-align: center; box-shadow: 0 8px 32px rgba(0,0,0,0.3);",
          div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;",
              span(id = "modal-message", "Processing...")
          ),
          div(style = "font-size: 16px; margin-bottom: 30px;",
              span(id = "modal-detail", "Please wait while we process your request...")
          ),
          div(class = "spinner", style = "border: 4px solid rgba(255,255,255,0.3); border-radius: 50%; border-top: 4px solid white; width: 50px; height: 50px; animation: spin 1s linear infinite; margin: 0 auto;")
      )
  ),
  
  # Add CSS for spinner animation
  tags$head(
    tags$style(HTML("
      @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
      }
    "))
  ),
  
  # JavaScript for modal control
  tags$script(HTML("
  Shiny.addCustomMessageHandler('showModal', function(data) {
      document.getElementById('modal-message').innerText = data.message;
      document.getElementById('modal-detail').innerText = data.detail;
      document.getElementById('processing-modal').style.display = 'block';
  });
  
  Shiny.addCustomMessageHandler('hideModal', function(data) {
      document.getElementById('processing-modal').style.display = 'none';
  });")),
  
  # Analysis Mode Selection
  fluidRow(
    column(12,
           wellPanel(
             h4("Analysis Mode Selection"),
             radioButtons("analysis_mode", "Choose Analysis Type:",
                          choices = list(
                            "Single Population Analysis" = "single",
                            "Multi-Population Comparative Analysis" = "multi"
                          ),
                          selected = "single",
                          inline = TRUE),
             conditionalPanel(
               condition = "input.analysis_mode == 'multi'",
               p("Multi-population mode allows you to define sub-populations and run separate analyses for each, with comprehensive cross-population comparisons.", style = "color: #555; font-style: italic;")
             )
           )
    )
  ),
  
  # Multi-Population Setup Panel
  conditionalPanel(
    condition = "input.analysis_mode == 'multi'",
    fluidRow(
      column(12,
             wellPanel(
               h4("Sub-Population Setup"),
               fluidRow(
                 column(4,
                        numericInput("total_population_multi", "Total Population",
                                     value = 1000000, min = 1000)
                 ),
                 column(4,
                        numericInput("num_subpops", "Number of Sub-Populations",
                                     value = 2, min = 2, max = 10, step = 1)
                 ),
                 column(4,
                        div(style = "margin-top: 25px;",
                            radioButtons("subpop_input_mode", "Input Mode:",
                                         choices = list("Percentages (%)" = "percent",
                                                        "Absolute Numbers" = "absolute"),
                                         selected = "percent",
                                         inline = TRUE)
                        )
                 )
               ),
               
               hr(),
               h5("Define Sub-Populations"),
               uiOutput("subpop_inputs_ui"),
               
               fluidRow(
                 column(6,
                        actionButton("validate_subpops", "Validate Sub-Populations",
                                     class = "btn-primary")
                 ),
                 column(6,
                        uiOutput("subpop_validation_message")
                 )
               )
             )
      )
    ),
    
    # Multi-Population Analysis Interface
    conditionalPanel(
      condition = "output.subpops_validated",
      fluidRow(
        column(12,
               wellPanel(
                 h3("Sub-Population Analysis"),
                 p("Each sub-population has its own complete configuration interface. Expand any sub-population to configure its settings and run analyses."),
                 
                 uiOutput("subpop_analysis_panels")
               )
        )
      ),
      
      # Multi-Population Results
      fluidRow(
        column(12,
               wellPanel(
                 h3("Comprehensive Multi-Population Results"),
                 
                 tabsetPanel(id = "multi_results_tabs",
                             tabPanel("Individual Results",
                                      DTOutput("individual_results_table")
                             ),
                             tabPanel("Cross-Population Comparison",
                                      DTOutput("cross_comparison_table")
                             ),
                             tabPanel("Aggregated Analysis",
                                      DTOutput("aggregated_results_table")
                             ),
                             tabPanel("Summary Statistics",
                                      DTOutput("summary_stats_table")
                             )
                 ),
                 
                 br(),
                 downloadButton("download_multi_analysis",
                                "Download Complete Multi-Population Analysis",
                                class = "btn-primary btn-lg")
               )
        )
      )
    )
  ),
  
  # Single Population Mode (your current app)
  conditionalPanel(
    condition = "input.analysis_mode == 'single'",
    
    # Global Configuration Panel
    fluidRow(
      column(12,
             wellPanel(
               h4("Global Configuration"),
               fluidRow(
                 # Population Input
                 column(3,
                        div(
                          numericInput("global_total_pop", "Total Population",
                                       value = 100000, min = 1000),
                          bsTooltip("global_total_pop",
                                    "Enter the total population size for analysis (minimum 1,000)",
                                    placement = "top", trigger = "hover")
                        )
                 ),
                 # Model Selection
                 column(3,
                        div(
                          selectInput("global_model_type", "Model Type for All Scenarios:",
                                      choices = c("Linear Regression",
                                                  "Decision Tree",
                                                  "Random Forest",
                                                  "Support Vector Regression",
                                                  "Lasso Regression",
                                                  "Ridge Regression"),
                                      selected = "Linear Regression"),
                          bsTooltip("global_model_type",
                                    "Select the machine learning model to use for all scenarios",
                                    placement = "top", trigger = "hover")
                        )
                 ),
                 # Census Defaults Button
                 column(3,
                        div(style = "margin-top: 25px;",
                            actionButton("use_global_census", "Use U.S. Census Defaults",
                                         class = "btn-info btn-sm"),
                            bsTooltip("use_global_census",
                                      "Apply U.S. Census demographic defaults",
                                      placement = "top", trigger = "hover")
                        )
                 ),
                 # Clear Button
                 column(3,
                        div(style = "margin-top: 25px;",
                            actionButton("clear_global", "Clear All Values",
                                         class = "btn-warning btn-sm"),
                            bsTooltip("clear_global",
                                      "Reset all inputs to default values",
                                      placement = "top", trigger = "hover")
                        )
                 )
               ),
               
               hr(),
               
               # Global Before Parameters Section
               h5("Baseline Screening Parameters - Applied to All Scenarios"),
               fluidRow(
                 column(4,
                        h6("FIT Parameters"),
                        div(
                          numericInput("global_fit_before", "FIT Before",
                                       value = 8, min = 0, max = 30),
                          bsTooltip("global_fit_before",
                                    "FIT screening rate before (Range: 0-30%)",
                                    placement = "top", trigger = "hover")
                        )
                 ),
                 column(4,
                        h6("Colonoscopy Parameters"),
                        div(
                          numericInput("global_colon_before", "COLON Before",
                                       value = 48, min = 30, max = 70),
                          bsTooltip("global_colon_before",
                                    "Colonoscopy screening rate before (Range: 30-70%)",
                                    placement = "top", trigger = "hover")
                        )
                 ),
                 column(4,
                        h6("Diagnostic Parameters"),
                        div(
                          numericInput("global_diag_before", "Diagnostic Before",
                                       value = 7, min = 0, max = 90),
                          bsTooltip("global_diag_before",
                                    "Diagnostic screening rate before (Range: 0-90%)",
                                    placement = "top", trigger = "hover")
                        )
                 )
               ),
               
               hr(),
               
               # Demographics Toggle Button
               fluidRow(
                 column(12,
                        div(style = "margin: 10px 0;",
                            actionButton("toggle_demographics", "Show/Hide Demographics",
                                         class = "btn-outline-secondary btn-sm",
                                         icon = icon("chevron-down"))
                        )
                 )
               ),
               
               # Demographics Section
               conditionalPanel(
                 condition = "output.show_demographics",
                 hr(),
                 h5("Population Demographics - Applied to All Scenarios"),
                 fluidRow(
                   column(6,
                          fluidRow(
                            column(6,
                                   h6("Gender Distribution (%)"),
                                   div(
                                     sliderInput("global_male_pct", "Male", min = 0, max = 100, value = 49),
                                     bsTooltip("global_male_pct",
                                               "Percentage of male population",
                                               placement = "top", trigger = "hover")
                                   ),
                                   div(
                                     sliderInput("global_female_pct", "Female", min = 0, max = 100, value = 51),
                                     bsTooltip("global_female_pct",
                                               "Percentage of female population",
                                               placement = "top", trigger = "hover")
                                   ),
                                   div(
                                     uiOutput("gender_validation"),
                                     style = "margin-top: 10px;"
                                   )
                            ),
                            column(6,
                                   h6("Race Distribution (%)"),
                                   div(
                                     sliderInput("global_white_pct", "White", min = 0, max = 100, value = 60),
                                     bsTooltip("global_white_pct",
                                               "Percentage of White population",
                                               placement = "top", trigger = "hover")
                                   ),
                                   div(
                                     sliderInput("global_black_pct", "Black", min = 0, max = 100, value = 13),
                                     bsTooltip("global_black_pct",
                                               "Percentage of Black population",
                                               placement = "top", trigger = "hover")
                                   ),
                                   div(
                                     sliderInput("global_other_pct", "Other", min = 0, max = 100, value = 27),
                                     bsTooltip("global_other_pct",
                                               "Percentage of Other races population",
                                               placement = "top", trigger = "hover")
                                   ),
                                   div(
                                     uiOutput("race_validation"),
                                     style = "margin-top: 10px;"
                                   )
                            )
                          )
                   ),
                   column(6,
                          h6("Age Group Distribution (%)"),
                          div(
                            sliderInput("global_age_45_49", "45–49", min = 0, max = 100, value = 17),
                            bsTooltip("global_age_45_49",
                                      "Percentage of population aged 45-49",
                                      placement = "top", trigger = "hover")
                          ),
                          div(
                            sliderInput("global_age_50_54", "50–54", min = 0, max = 100, value = 17),
                            bsTooltip("global_age_50_54",
                                      "Percentage of population aged 50-54",
                                      placement = "top", trigger = "hover")
                          ),
                          div(
                            sliderInput("global_age_55_59", "55–59", min = 0, max = 100, value = 16),
                            bsTooltip("global_age_55_59",
                                      "Percentage of population aged 55-59",
                                      placement = "top", trigger = "hover")
                          ),
                          div(
                            sliderInput("global_age_60_64", "60–64", min = 0, max = 100, value = 16),
                            bsTooltip("global_age_60_64",
                                      "Percentage of population aged 60-64",
                                      placement = "top", trigger = "hover")
                          ),
                          div(
                            sliderInput("global_age_65_69", "65–69", min = 0, max = 100, value = 16),
                            bsTooltip("global_age_65_69",
                                      "Percentage of population aged 65-69",
                                      placement = "top", trigger = "hover")
                          ),
                          div(
                            sliderInput("global_age_70_74", "70–74", min = 0, max = 100, value = 18),
                            bsTooltip("global_age_70_74",
                                      "Percentage of population aged 70-74",
                                      placement = "top", trigger = "hover")
                          ),
                          div(
                            uiOutput("age_validation"),
                            style = "margin-top: 10px;"
                          )
                   )
                 )
               )
             )
      )
    ),
    
    # Scenario Selection Panel
    fluidRow(
      column(12,
             wellPanel(
               h4("Scenario Configuration"),
               fluidRow(
                 column(4,
                        numericInput("num_scenarios", "Number of Scenarios to Compare:",
                                     value = 1, min = 1, max = 5, step = 1)
                 ),
                 column(8,
                        conditionalPanel(
                          condition = "input.num_scenarios > 1",
                          div(style = "margin-top: 25px;",
                              helpText("You can compare up to 5 scenarios side by side. Give each scenario a meaningful name and configure them separately.")
                          )
                        )
                 )
               ),
               
               # Scenario Names Section
               conditionalPanel(
                 condition = "input.num_scenarios >= 1",
                 hr(),
                 h5("Scenario Names"),
                 uiOutput("scenario_names_ui"),
                 actionButton("apply_names", "Apply Scenario Names", class = "btn-info btn-sm"),
                 helpText("Give your scenarios meaningful names (e.g., 'Current Policy', 'Enhanced Screening', 'Budget Option')")
               )
             )
      )
    ),
    
    # Dynamic Scenario Panels
    uiOutput("scenario_panels"),
    
    # Results Comparison Section
    fluidRow(
      column(12,
             wellPanel(
               h3("Results Comparison"),
               conditionalPanel(
                 condition = "output.baseline_status",
                 div(
                   style = "background-color: #d4edda; border: 2px solid #007bff; border-radius: 5px; padding: 15px; margin: 10px 0; text-align: center;box-shadow: 0px 4px 8px rgba(0,0,0,0.2);",
                   h5("🔄 Computing baseline for newly selected model...", style = "color: #004085;font-weight: bold; font-size: 18px; margin: 0;")
                 )
               ),
               DTOutput("comparison_table"),
               br(),
               downloadButton("download_comparison", "Download Comprehensive Analysis", class = "btn-primary")
             )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load pre-computed models cache
  precomputed_models <- reactiveVal(list())
  
  # Single Population Reactive values
  baseline_computing <- reactiveVal(FALSE)
  demographics_visible <- reactiveVal(TRUE)
  scenario_results <- reactiveVal(list())
  model_cache <- reactiveVal(list()) # Cache models to avoid retraining
  scenario_names <- reactiveVal(c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"))
  mapping_df <- create_mapping_df()
  
  # Multi-Population Reactive values
  subpop_definitions <- reactiveVal(data.frame())
  subpops_validated <- reactiveVal(FALSE)
  subpop_results <- reactiveVal(list())
  
  # Storage for each sub-population's individual states
  subpop_states <- reactiveValues()
  
  # ADD THIS HELPER FUNCTION HERE:
  get_model_load_time <- function(model_type) {
    times <- list(
      "Linear Regression" = "30 seconds - 1 minute",
      "Decision Tree" = "1-2 minutes",
      "Random Forest" = "2-3 minutes",
      "Support Vector Regression" = "2-4 minutes",
      "Lasso Regression" = "1-2 minutes",
      "Ridge Regression" = "1-2 minutes"
    )
    return(times[[model_type]])
  }
  
  # Output definitions
  output$baseline_status <- reactive({
    baseline_computing()
  })
  outputOptions(output, "baseline_status", suspendWhenHidden = FALSE)
  
  output$show_demographics <- reactive({
    demographics_visible()
  })
  outputOptions(output, "show_demographics", suspendWhenHidden = FALSE)
  
  output$subpops_validated <- reactive({
    subpops_validated()
  })
  outputOptions(output, "subpops_validated", suspendWhenHidden = FALSE)
  
  # Function to load pre-computed model
  load_precomputed_model <- function(model_type) {
    cache <- precomputed_models()
    
    # Check if already loaded
    if (!is.null(cache[[model_type]])) {
      return(cache[[model_type]])
    }
    
    # Load from file
    filename <- switch(model_type,
                       "Linear Regression" = "models_linear_regression.rds",
                       "Decision Tree" = "models_decision_tree.rds",
                       "Random Forest" = "models_random_forest.rds",
                       "Support Vector Regression" = "models_support_vector_regression.rds",
                       "Lasso Regression" = "models_lasso_regression.rds",
                       "Ridge Regression" = "models_ridge_regression.rds"
    )
    
    if (file.exists(filename)) {
      models <- readRDS(filename)
      
      # Cache the loaded models
      cache[[model_type]] <- models
      precomputed_models(cache)
      
      return(models)
    } else {
      showNotification(paste("Pre-computed model file not found:", filename),
                       type = "warning", duration = 5)
      return(NULL)
    }
  }
  
  # Load and prepare training data
  training_data <- reactive({
    files <- c("edited_INFORMScancer_after_averages_combined.csv",
               "edited_INFORMSlife_years_averages_combined.csv",
               "edited_INFORMScancer_death_averages_combined.csv")
    
    # Check if files exist
    missing_files <- files[!file.exists(files)]
    if (length(missing_files) > 0) {
      showNotification(paste("Missing files:", paste(missing_files, collapse = ", ")),
                       type = "error", duration = 10)
      return(NULL)
    }
    
    withProgress(message = 'Loading training data...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Loading cancer cases data")
        cca_data <- data_prep(files[1], "CCA")
        
        incProgress(0.6, detail = "Loading life years data")
        lyl_data <- data_prep(files[2], "LYL")
        
        incProgress(0.9, detail = "Loading cancer deaths data")
        cd_data <- data_prep(files[3], "CD")
        
        list(cca = cca_data, lyl = lyl_data, cd = cd_data)
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
        return(NULL)
      })
    })
  })
  
  # Train models with progress callback - UPDATED TO USE PRE-COMPUTED MODELS
  train_models_with_progress <- function(data, model_type) {
    # Try to load pre-computed models first
    models <- load_precomputed_model(model_type)
    
    if (!is.null(models)) {
      return(models)
    }
    
    # Fallback to live training if pre-computed models not available
    showNotification("Pre-computed models not found. Using simplified live training.",
                     type = "warning", duration = 5)
    
    # Your existing training code as fallback
    if (is.null(data)) return(NULL)
    
    # Check cache first
    cache_key <- paste(model_type, digest::digest(data), sep = "_")
    cached_models <- model_cache()
    if (!is.null(cached_models[[cache_key]])) {
      return(cached_models[[cache_key]])
    }
    
    # Estimate time
    estimated_time <- get_estimated_time(model_type, sum(sapply(data, nrow)))
    
    models <- NULL
    withProgress(message = paste('Training', model_type, 'models...'),
                 detail = paste('Estimated time:', estimated_time), value = 0, {
                   
                   progress_callback <- function(progress, detail) {
                     incProgress(progress/3, detail = detail)
                   }
                   
                   tryCatch({
                     # Train CCA models
                     setProgress(0, detail = "Training Cancer Cases models...")
                     cca_models <- switch(model_type,
                                          "Linear Regression" = train_lm_models(data$cca, "CCA", progress_callback),
                                          "Decision Tree" = train_tree_models(data$cca, "CCA", progress_callback),
                                          "Random Forest" = train_rf_models(data$cca, "CCA", progress_callback),
                                          "Support Vector Regression" = train_svr_models(data$cca, "CCA", progress_callback),
                                          "Lasso Regression" = train_lasso_models(data$cca, "CCA", progress_callback),
                                          "Ridge Regression" = train_ridge_models(data$cca, "CCA", progress_callback))
                     
                     # Train LYL models
                     setProgress(0.33, detail = "Training Life Years Lost models...")
                     lyl_models <- switch(model_type,
                                          "Linear Regression" = train_lm_models(data$lyl, "LYL", progress_callback),
                                          "Decision Tree" = train_tree_models(data$lyl, "LYL", progress_callback),
                                          "Random Forest" = train_rf_models(data$lyl, "LYL", progress_callback),
                                          "Support Vector Regression" = train_svr_models(data$lyl, "LYL", progress_callback),
                                          "Lasso Regression" = train_lasso_models(data$lyl, "LYL", progress_callback),
                                          "Ridge Regression" = train_ridge_models(data$lyl, "LYL", progress_callback))
                     
                     # Train CD models
                     setProgress(0.66, detail = "Training Cancer Deaths models...")
                     cd_models <- switch(model_type,
                                         "Linear Regression" = train_lm_models(data$cd, "CD", progress_callback),
                                         "Decision Tree" = train_tree_models(data$cd, "CD", progress_callback),
                                         "Random Forest" = train_rf_models(data$cd, "CD", progress_callback),
                                         "Support Vector Regression" = train_svr_models(data$cd, "CD", progress_callback),
                                         "Lasso Regression" = train_lasso_models(data$cd, "CD", progress_callback),
                                         "Ridge Regression" = train_ridge_models(data$cd, "CD", progress_callback))
                     
                     models <- list(cca = cca_models, lyl = lyl_models, cd = cd_models)
                     
                     # Cache the models
                     cached_models[[cache_key]] <- models
                     model_cache(cached_models)
                     
                   }, error = function(e) {
                     showNotification(paste("Training failed:", e$message), type = "error")
                     return(NULL)
                   })
                 })
    
    return(models)
  }
  
  # =======================================================================================
  # MULTI-POPULATION SETUP LOGIC
  # =======================================================================================
  
  # Generate sub-population inputs UI
  output$subpop_inputs_ui <- renderUI({
    num_subpops <- input$num_subpops
    if (is.null(num_subpops) || num_subpops < 1) return(NULL)
    
    input_mode <- input$subpop_input_mode
    total_pop <- input$total_population_multi
    
    if (is.null(input_mode) || is.null(total_pop)) return(NULL)
    
    subpop_inputs <- lapply(1:num_subpops, function(i) {
      fluidRow(
        column(6,
               textInput(paste0("subpop_name_", i),
                         paste("Sub-Population", i, "Name:"),
                         value = paste("Sub-Population", i))
        ),
        column(6,
               if (input_mode == "percent") {
                 numericInput(paste0("subpop_percent_", i),
                              paste("Percentage of Total (%)"),
                              value = round(100/num_subpops, 1),
                              min = 0.1, max = 100, step = 0.1)
               } else {
                 numericInput(paste0("subpop_absolute_", i),
                              paste("Population Count"),
                              value = round(total_pop/num_subpops),
                              min = 1, max = total_pop)
               }
        )
      )
    })
    
    do.call(tagList, subpop_inputs)
  })
  
  # Validate sub-populations
  observeEvent(input$validate_subpops, {
    num_subpops <- input$num_subpops
    input_mode <- input$subpop_input_mode
    total_pop <- input$total_population_multi
    
    if (is.null(num_subpops) || is.null(input_mode) || is.null(total_pop)) {
      output$subpop_validation_message <- renderUI({
        div(style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
            strong("⚠️ Error: ", style = "color: #721c24;"),
            span("Please fill in all basic setup fields first.", style = "color: #721c24;")
        )
      })
      return()
    }
    
    # Collect sub-population data
    subpop_data <- data.frame(
      id = paste0("subpop_", 1:num_subpops),
      name = character(num_subpops),
      value = numeric(num_subpops),
      population = numeric(num_subpops),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:num_subpops) {
      name_input <- input[[paste0("subpop_name_", i)]]
      if (input_mode == "percent") {
        value_input <- input[[paste0("subpop_percent_", i)]]
        population <- round(total_pop * value_input / 100)
      } else {
        value_input <- input[[paste0("subpop_absolute_", i)]]
        population <- value_input
      }
      
      subpop_data$name[i] <- if (is.null(name_input)) "" else name_input
      subpop_data$value[i] <- if (is.null(value_input)) 0 else value_input
      subpop_data$population[i] <- population
    }
    
    # Validate
    validation_result <- validate_subpopulations(subpop_data, input_mode, total_pop)
    
    if (validation_result$valid) {
      # Store validated sub-populations
      subpop_definitions(subpop_data)
      subpops_validated(TRUE)
      
      # Initialize sub-population states
      for (i in 1:num_subpops) {
        subpop_id <- subpop_data$id[i]
        subpop_states[[paste0(subpop_id, "_demo_visible")]] <- FALSE
        subpop_states[[paste0(subpop_id, "_scenario_results")]] <- list()
        subpop_states[[paste0(subpop_id, "_scenario_names")]] <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5")
      }
      
      # Clear any existing results
      subpop_results(list())
      
      output$subpop_validation_message <- renderUI({
        div(style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
            strong("✓ Success: ", style = "color: #155724;"),
            span("Sub-populations validated successfully!", style = "color: #155724;")
        )
      })
      
      showNotification("Sub-populations validated! You can now configure analysis for each sub-population.",
                       type = "message", duration = 3)
    } else {
      subpops_validated(FALSE)
      output$subpop_validation_message <- renderUI({
        div(style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
            strong("⚠️ Validation Errors: ", style = "color: #721c24;"),
            HTML(paste("• ", validation_result$errors, collapse = "<br>"))
        )
      })
    }
  })
  
  # =======================================================================================
  # ENHANCED SUB-POPULATION INTERFACE CREATION
  # =======================================================================================
  
  # Generate sub-population analysis panels with full single-population interface
  output$subpop_analysis_panels <- renderUI({
    subpop_defs <- subpop_definitions()
    if (nrow(subpop_defs) == 0) return(NULL)
    
    panels <- lapply(1:nrow(subpop_defs), function(i) {
      subpop_info <- subpop_defs[i, ]
      subpop_id <- subpop_info$id
      
      div(
        wellPanel(
          fluidRow(
            column(10,
                   h4(paste(subpop_info$name, "-", format(subpop_info$population, big.mark = ",", scientific = FALSE)),
                      style = "color: #2c3e50; margin: 0;")
            ),
            column(2,
                   div(style = "text-align: right; margin-top: 5px;",
                       actionButton(paste0("toggle_subpop_", i), "Expand/Collapse",
                                    class = "btn-outline-secondary btn-sm",
                                    icon = icon("chevron-down"))
                   )
            )
          ),
          
          conditionalPanel(
            condition = paste0("output['show_subpop_", i, "']"),
            hr(),
            create_full_subpop_interface(subpop_id, subpop_info$population, subpop_info$name)
          )
        )
      )
    })
    
    do.call(tagList, panels)
  })
  
  # Function to create complete single-population-like interface for each sub-population
  create_full_subpop_interface <- function(subpop_id, subpop_population, subpop_name) {
    tagList(
      # Global Configuration for this sub-population
      wellPanel(
        h5(paste("Configuration for", subpop_name), style = "color: #2c3e50; border-bottom: 1px solid #3498db; padding-bottom: 5px;"),
        fluidRow(
          # Population (readonly - already defined)
          column(3,
                 div(
                   tags$label("Population Size"),
                   div(style = "background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 0.25rem; padding: 0.375rem 0.75rem; font-weight: bold;",
                       format(subpop_population, big.mark = ","))
                 )
          ),
          # Model Selection
          column(3,
                 div(
                   selectInput(paste0(subpop_id, "_model_type"), "Model Type:",
                               choices = c("Linear Regression",
                                           "Decision Tree",
                                           "Random Forest",
                                           "Support Vector Regression",
                                           "Lasso Regression",
                                           "Ridge Regression"),
                               selected = "Linear Regression"),
                   bsTooltip(paste0(subpop_id, "_model_type"),
                             "Select the machine learning model for this sub-population",
                             placement = "top", trigger = "hover")
                 )
          ),
          # Census Defaults Button
          column(3,
                 div(style = "margin-top: 25px;",
                     actionButton(paste0(subpop_id, "_use_census"), "Use U.S. Census Defaults",
                                  class = "btn-info btn-sm"),
                     bsTooltip(paste0(subpop_id, "_use_census"),
                               "Apply U.S. Census demographic defaults",
                               placement = "top", trigger = "hover")
                 )
          ),
          # Clear Button
          column(3,
                 div(style = "margin-top: 25px;",
                     actionButton(paste0(subpop_id, "_clear_all"), "Clear All Values",
                                  class = "btn-warning btn-sm"),
                     bsTooltip(paste0(subpop_id, "_clear_all"),
                               "Reset all inputs to default values",
                               placement = "top", trigger = "hover")
                 )
          )
        ),
        
        hr(),
        
        # Baseline Parameters
        h6("Baseline Screening Parameters"),
        fluidRow(
          column(4,
                 h6("FIT Parameters"),
                 div(
                   numericInput(paste0(subpop_id, "_fit_before"), "FIT Before",
                                value = 8, min = 0, max = 30),
                   bsTooltip(paste0(subpop_id, "_fit_before"),
                             "FIT screening rate before (Range: 0-30%)",
                             placement = "top", trigger = "hover")
                 )
          ),
          column(4,
                 h6("Colonoscopy Parameters"),
                 div(
                   numericInput(paste0(subpop_id, "_colon_before"), "COLON Before",
                                value = 48, min = 30, max = 70),
                   bsTooltip(paste0(subpop_id, "_colon_before"),
                             "Colonoscopy screening rate before (Range: 30-70%)",
                             placement = "top", trigger = "hover")
                 )
          ),
          column(4,
                 h6("Diagnostic Parameters"),
                 div(
                   numericInput(paste0(subpop_id, "_diag_before"), "Diagnostic Before",
                                value = 7, min = 0, max = 90),
                   bsTooltip(paste0(subpop_id, "_diag_before"),
                             "Diagnostic screening rate before (Range: 0-90%)",
                             placement = "top", trigger = "hover")
                 )
          )
        ),
        
        hr(),
        
        # Demographics Toggle
        fluidRow(
          column(12,
                 div(style = "margin: 10px 0;",
                     actionButton(paste0(subpop_id, "_toggle_demographics"), "Show/Hide Demographics",
                                  class = "btn-outline-secondary btn-sm",
                                  icon = icon("chevron-down"))
                 )
          )
        ),
        
        # Demographics Section
        conditionalPanel(
          condition = paste0("output['", subpop_id, "_show_demographics']"),
          hr(),
          h6("Population Demographics"),
          create_demographics_section(subpop_id)
        )
      ),
      
      # Scenario Configuration
      wellPanel(
        h5("Scenario Configuration", style = "color: #2c3e50; border-bottom: 1px solid #3498db; padding-bottom: 5px;"),
        fluidRow(
          column(4,
                 numericInput(paste0(subpop_id, "_num_scenarios"), "Number of Scenarios to Compare:",
                              value = 1, min = 1, max = 5, step = 1)
          ),
          column(8,
                 conditionalPanel(
                   condition = paste0("input['", subpop_id, "_num_scenarios'] > 1"),
                   div(style = "margin-top: 25px;",
                       helpText("You can compare up to 5 scenarios side by side.")
                   )
                 )
          )
        ),
        
        # Scenario Names Section
        conditionalPanel(
          condition = paste0("input['", subpop_id, "_num_scenarios'] >= 1"),
          hr(),
          h6("Scenario Names"),
          uiOutput(paste0(subpop_id, "_scenario_names_ui")),
          actionButton(paste0(subpop_id, "_apply_names"), "Apply Scenario Names", class = "btn-info btn-sm"),
          helpText("Give your scenarios meaningful names")
        )
      ),
      
      # Dynamic Scenario Panels
      uiOutput(paste0(subpop_id, "_scenario_panels")),
      
      # Results Section
      wellPanel(
        h5("Results Comparison", style = "color: #2c3e50; border-bottom: 1px solid #3498db; padding-bottom: 5px;"),
        conditionalPanel(
          condition = paste0("output['", subpop_id, "_baseline_status']"),
          div(
            style = "background-color: #d4edda; border: 2px solid #007bff; border-radius: 5px; padding: 15px; margin: 10px 0; text-align: center;",
            h6("🔄 Computing baseline...", style = "color: #004085;font-weight: bold; margin: 0;")
          )
        ),
        DTOutput(paste0(subpop_id, "_comparison_table")),
        br(),
        downloadButton(paste0(subpop_id, "_download_comparison"), 
                       paste("Download", subpop_name, "Analysis"), 
                       class = "btn-primary btn-sm")
      )
    )
  }
  
  # Function to create demographics section
  create_demographics_section <- function(subpop_id) {
    fluidRow(
      column(6,
             fluidRow(
               column(6,
                      h6("Gender Distribution (%)"),
                      div(
                        sliderInput(paste0(subpop_id, "_male_pct"), "Male", min = 0, max = 100, value = 49),
                        bsTooltip(paste0(subpop_id, "_male_pct"),
                                  "Percentage of male population",
                                  placement = "top", trigger = "hover")
                      ),
                      div(
                        sliderInput(paste0(subpop_id, "_female_pct"), "Female", min = 0, max = 100, value = 51),
                        bsTooltip(paste0(subpop_id, "_female_pct"),
                                  "Percentage of female population",
                                  placement = "top", trigger = "hover")
                      ),
                      div(
                        uiOutput(paste0(subpop_id, "_gender_validation")),
                        style = "margin-top: 10px;"
                      )
               ),
               column(6,
                      h6("Race Distribution (%)"),
                      div(
                        sliderInput(paste0(subpop_id, "_white_pct"), "White", min = 0, max = 100, value = 60),
                        bsTooltip(paste0(subpop_id, "_white_pct"),
                                  "Percentage of White population",
                                  placement = "top", trigger = "hover")
                      ),
                      div(
                        sliderInput(paste0(subpop_id, "_black_pct"), "Black", min = 0, max = 100, value = 13),
                        bsTooltip(paste0(subpop_id, "_black_pct"),
                                  "Percentage of Black population",
                                  placement = "top", trigger = "hover")
                      ),
                      div(
                        sliderInput(paste0(subpop_id, "_other_pct"), "Other", min = 0, max = 100, value = 27),
                        bsTooltip(paste0(subpop_id, "_other_pct"),
                                  "Percentage of Other races population",
                                  placement = "top", trigger = "hover")
                      ),
                      div(
                        uiOutput(paste0(subpop_id, "_race_validation")),
                        style = "margin-top: 10px;"
                      )
               )
             )
      ),
      column(6,
             h6("Age Group Distribution (%)"),
             div(
               sliderInput(paste0(subpop_id, "_age_45_49"), "45–49", min = 0, max = 100, value = 17),
               bsTooltip(paste0(subpop_id, "_age_45_49"),
                         "Percentage of population aged 45-49",
                         placement = "top", trigger = "hover")
             ),
             div(
               sliderInput(paste0(subpop_id, "_age_50_54"), "50–54", min = 0, max = 100, value = 17),
               bsTooltip(paste0(subpop_id, "_age_50_54"),
                         "Percentage of population aged 50-54",
                         placement = "top", trigger = "hover")
             ),
             div(
               sliderInput(paste0(subpop_id, "_age_55_59"), "55–59", min = 0, max = 100, value = 16),
               bsTooltip(paste0(subpop_id, "_age_55_59"),
                         "Percentage of population aged 55-59",
                         placement = "top", trigger = "hover")
             ),
             div(
               sliderInput(paste0(subpop_id, "_age_60_64"), "60–64", min = 0, max = 100, value = 16),
               bsTooltip(paste0(subpop_id, "_age_60_64"),
                         "Percentage of population aged 60-64",
                         placement = "top", trigger = "hover")
             ),
             div(
               sliderInput(paste0(subpop_id, "_age_65_69"), "65–69", min = 0, max = 100, value = 16),
               bsTooltip(paste0(subpop_id, "_age_65_69"),
                         "Percentage of population aged 65-69",
                         placement = "top", trigger = "hover")
             ),
             div(
               sliderInput(paste0(subpop_id, "_age_70_74"), "70–74", min = 0, max = 100, value = 18),
               bsTooltip(paste0(subpop_id, "_age_70_74"),
                         "Percentage of population aged 70-74",
                         placement = "top", trigger = "hover")
             ),
             div(
               uiOutput(paste0(subpop_id, "_age_validation")),
               style = "margin-top: 10px;"
             )
      )
    )
  }
  
  # =======================================================================================
  # SUB-POPULATION OBSERVERS AND REACTIVE FUNCTIONS
  # =======================================================================================
  
  # Create toggle states and observers for sub-populations
  observe({
    subpop_defs <- subpop_definitions()
    if (nrow(subpop_defs) == 0) return()
    
    for (i in 1:nrow(subpop_defs)) {
      local({
        subpop_index <- i
        subpop_info <- subpop_defs[subpop_index, ]
        subpop_id <- subpop_info$id
        
        # Main panel toggle
        panel_toggle_state <- reactiveVal(FALSE)
        
        output[[paste0("show_subpop_", subpop_index)]] <- reactive({
          panel_toggle_state()
        })
        outputOptions(output, paste0("show_subpop_", subpop_index), suspendWhenHidden = FALSE)
        
        observeEvent(input[[paste0("toggle_subpop_", subpop_index)]], {
          current_state <- panel_toggle_state()
          panel_toggle_state(!current_state)
          
          if (!current_state) {
            updateActionButton(session, paste0("toggle_subpop_", subpop_index),
                               icon = icon("chevron-up"))
          } else {
            updateActionButton(session, paste0("toggle_subpop_", subpop_index),
                               icon = icon("chevron-down"))
          }
        })
        
        # Demographics toggle
        demo_toggle_state <- reactiveVal(FALSE)
        
        output[[paste0(subpop_id, "_show_demographics")]] <- reactive({
          demo_toggle_state()
        })
        outputOptions(output, paste0(subpop_id, "_show_demographics"), suspendWhenHidden = FALSE)
        
        observeEvent(input[[paste0(subpop_id, "_toggle_demographics")]], {
          current_state <- demo_toggle_state()
          demo_toggle_state(!current_state)
          
          if (!current_state) {
            updateActionButton(session, paste0(subpop_id, "_toggle_demographics"),
                               icon = icon("chevron-up"))
          } else {
            updateActionButton(session, paste0(subpop_id, "_toggle_demographics"),
                               icon = icon("chevron-down"))
          }
        })
        
        # Baseline status
        baseline_computing_state <- reactiveVal(FALSE)
        
        output[[paste0(subpop_id, "_baseline_status")]] <- reactive({
          baseline_computing_state()
        })
        outputOptions(output, paste0(subpop_id, "_baseline_status"), suspendWhenHidden = FALSE)
        
        # Validation functions for this sub-population
        subpop_gender_total <- reactive({
          male_pct <- input[[paste0(subpop_id, "_male_pct")]]
          female_pct <- input[[paste0(subpop_id, "_female_pct")]]
          if (is.null(male_pct) || is.null(female_pct)) return(NULL)
          male_pct + female_pct
        })
        
        subpop_race_total <- reactive({
          white_pct <- input[[paste0(subpop_id, "_white_pct")]]
          black_pct <- input[[paste0(subpop_id, "_black_pct")]]
          other_pct <- input[[paste0(subpop_id, "_other_pct")]]
          if (is.null(white_pct) || is.null(black_pct) || is.null(other_pct)) return(NULL)
          white_pct + black_pct + other_pct
        })
        
        subpop_age_total <- reactive({
          age_inputs <- c(
            input[[paste0(subpop_id, "_age_45_49")]],
            input[[paste0(subpop_id, "_age_50_54")]],
            input[[paste0(subpop_id, "_age_55_59")]],
            input[[paste0(subpop_id, "_age_60_64")]],
            input[[paste0(subpop_id, "_age_65_69")]],
            input[[paste0(subpop_id, "_age_70_74")]]
          )
          if (any(sapply(age_inputs, is.null))) return(NULL)
          sum(age_inputs)
        })
        
        # Validation outputs
        output[[paste0(subpop_id, "_gender_validation")]] <- renderUI({
          total <- subpop_gender_total()
          if (!is.null(total) && total != 100) {
            div(
              style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px; margin: 5px 0;",
              strong("⚠️ Gender Error: ", style = "color: #721c24;"),
              span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
            )
          }
        })
        
        output[[paste0(subpop_id, "_race_validation")]] <- renderUI({
          total <- subpop_race_total()
          if (!is.null(total) && total != 100) {
            div(
              style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px; margin: 5px 0;",
              strong("⚠️ Race Error: ", style = "color: #721c24;"),
              span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
            )
          }
        })
        
        output[[paste0(subpop_id, "_age_validation")]] <- renderUI({
          total <- subpop_age_total()
          if (!is.null(total) && total != 100) {
            div(
              style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px; margin: 5px 0;",
              strong("⚠️ Age Groups Error: ", style = "color: #721c24;"),
              span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
            )
          }
        })
        
        # Helper functions for this sub-population
        get_subpop_scenario_name <- function(scenario_num) {
          current_names <- subpop_states[[paste0(subpop_id, "_scenario_names")]]
          if (is.null(current_names)) current_names <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5")
          if (scenario_num <= length(current_names)) {
            return(current_names[scenario_num])
          } else {
            return(paste("Scenario", scenario_num))
          }
        }
        
        compute_subpop_demographic_weights <- function(scenario_inputs) {
          gender <- c(male = scenario_inputs$male_pct / 100, female = scenario_inputs$female_pct / 100)
          race <- c(white = scenario_inputs$white_pct / 100, black = scenario_inputs$black_pct / 100, other = scenario_inputs$other_pct / 100)
          age <- c(
            "45-49" = scenario_inputs$age_45_49 / 100,
            "50-54" = scenario_inputs$age_50_54 / 100,
            "55-59" = scenario_inputs$age_55_59 / 100,
            "60-64" = scenario_inputs$age_60_64 / 100,
            "65-69" = scenario_inputs$age_65_69 / 100,
            "70-74" = scenario_inputs$age_70_74 / 100
          )
          
          weights_vec <- numeric(180)
          for (g in names(gender)) {
            for (r in names(race)) {
              for (a in names(age)) {
                subpop <- subpop_info$population * gender[g] * race[r] * age[a]
                matches <- mapping_df %>%
                  filter(Gender == g, Race == r, AgeGroup == a)
                if (nrow(matches) > 0) {
                  per_capita <- subpop / nrow(matches)
                  weights_vec[matches$P_values] <- weights_vec[matches$P_values] + per_capita
                }
              }
            }
          }
          round(weights_vec, 2)
        }
        
        get_subpop_scenario_inputs <- function(scenario_num) {
          list(
            total_pop = subpop_info$population,
            male_pct = input[[paste0(subpop_id, "_male_pct")]],
            female_pct = input[[paste0(subpop_id, "_female_pct")]],
            white_pct = input[[paste0(subpop_id, "_white_pct")]],
            black_pct = input[[paste0(subpop_id, "_black_pct")]],
            other_pct = input[[paste0(subpop_id, "_other_pct")]],
            age_45_49 = input[[paste0(subpop_id, "_age_45_49")]],
            age_50_54 = input[[paste0(subpop_id, "_age_50_54")]],
            age_55_59 = input[[paste0(subpop_id, "_age_55_59")]],
            age_60_64 = input[[paste0(subpop_id, "_age_60_64")]],
            age_65_69 = input[[paste0(subpop_id, "_age_65_69")]],
            age_70_74 = input[[paste0(subpop_id, "_age_70_74")]],
            fit_before = input[[paste0(subpop_id, "_fit_before")]],
            fit_during = input[[paste0("fit_during_", subpop_id, "_", scenario_num)]],
            fit_after = input[[paste0("fit_after_", subpop_id, "_", scenario_num)]],
            colon_before = input[[paste0(subpop_id, "_colon_before")]],
            colon_during = input[[paste0("colon_during_", subpop_id, "_", scenario_num)]],
            colon_after = input[[paste0("colon_after_", subpop_id, "_", scenario_num)]],
            diag_before = input[[paste0(subpop_id, "_diag_before")]],
            diag_during = input[[paste0("diag_during_", subpop_id, "_", scenario_num)]],
            diag_after = input[[paste0("diag_after_", subpop_id, "_", scenario_num)]],
            model_type = input[[paste0(subpop_id, "_model_type")]]
          )
        }
        
        # Button observers
        observeEvent(input[[paste0(subpop_id, "_use_census")]], {
          updateSliderInput(session, paste0(subpop_id, "_male_pct"), value = 49)
          updateSliderInput(session, paste0(subpop_id, "_female_pct"), value = 51)
          updateSliderInput(session, paste0(subpop_id, "_white_pct"), value = 60)
          updateSliderInput(session, paste0(subpop_id, "_black_pct"), value = 13)
          updateSliderInput(session, paste0(subpop_id, "_other_pct"), value = 27)
          updateSliderInput(session, paste0(subpop_id, "_age_45_49"), value = 17)
          updateSliderInput(session, paste0(subpop_id, "_age_50_54"), value = 17)
          updateSliderInput(session, paste0(subpop_id, "_age_55_59"), value = 16)
          updateSliderInput(session, paste0(subpop_id, "_age_60_64"), value = 16)
          updateSliderInput(session, paste0(subpop_id, "_age_65_69"), value = 16)
          updateSliderInput(session, paste0(subpop_id, "_age_70_74"), value = 18)
          
          showNotification(paste("Census defaults applied to", subpop_info$name), type = "message", duration = 2)
        })
        
        observeEvent(input[[paste0(subpop_id, "_clear_all")]], {
          updateSelectInput(session, paste0(subpop_id, "_model_type"), selected = "Linear Regression")
          updateNumericInput(session, paste0(subpop_id, "_fit_before"), value = 8)
          updateNumericInput(session, paste0(subpop_id, "_colon_before"), value = 48)
          updateNumericInput(session, paste0(subpop_id, "_diag_before"), value = 7)
          updateSliderInput(session, paste0(subpop_id, "_male_pct"), value = 50)
          updateSliderInput(session, paste0(subpop_id, "_female_pct"), value = 50)
          updateSliderInput(session, paste0(subpop_id, "_white_pct"), value = 60)
          updateSliderInput(session, paste0(subpop_id, "_black_pct"), value = 13)
          updateSliderInput(session, paste0(subpop_id, "_other_pct"), value = 27)
          updateSliderInput(session, paste0(subpop_id, "_age_45_49"), value = 17)
          updateSliderInput(session, paste0(subpop_id, "_age_50_54"), value = 17)
          updateSliderInput(session, paste0(subpop_id, "_age_55_59"), value = 16)
          updateSliderInput(session, paste0(subpop_id, "_age_60_64"), value = 16)
          updateSliderInput(session, paste0(subpop_id, "_age_65_69"), value = 16)
          updateSliderInput(session, paste0(subpop_id, "_age_70_74"), value = 18)
          
          # Clear scenario results
          subpop_states[[paste0(subpop_id, "_scenario_results")]] <- list()
          
          showNotification(paste("All values cleared for", subpop_info$name), type = "message", duration = 2)
        })
        
        # Generate baseline prediction automatically when demographics change
        subpop_baseline_inputs_reactive <- reactive({
          list(
            total_pop = subpop_info$population,
            male_pct = input[[paste0(subpop_id, "_male_pct")]],
            female_pct = input[[paste0(subpop_id, "_female_pct")]],
            white_pct = input[[paste0(subpop_id, "_white_pct")]],
            black_pct = input[[paste0(subpop_id, "_black_pct")]],
            other_pct = input[[paste0(subpop_id, "_other_pct")]],
            age_45_49 = input[[paste0(subpop_id, "_age_45_49")]],
            age_50_54 = input[[paste0(subpop_id, "_age_50_54")]],
            age_55_59 = input[[paste0(subpop_id, "_age_55_59")]],
            age_60_64 = input[[paste0(subpop_id, "_age_60_64")]],
            age_65_69 = input[[paste0(subpop_id, "_age_65_69")]],
            age_70_74 = input[[paste0(subpop_id, "_age_70_74")]],
            fit_before = input[[paste0(subpop_id, "_fit_before")]],
            fit_during = input[[paste0(subpop_id, "_fit_before")]],
            fit_after = input[[paste0(subpop_id, "_fit_before")]],
            colon_before = input[[paste0(subpop_id, "_colon_before")]],
            colon_during = input[[paste0(subpop_id, "_colon_before")]],
            colon_after = input[[paste0(subpop_id, "_colon_before")]],
            diag_before = input[[paste0(subpop_id, "_diag_before")]],
            diag_during = input[[paste0(subpop_id, "_diag_before")]],
            diag_after = input[[paste0(subpop_id, "_diag_before")]],
            model_type = input[[paste0(subpop_id, "_model_type")]]
          )
        })
        
        subpop_baseline_trigger <- debounce(subpop_baseline_inputs_reactive, 1000)
        
        observeEvent(subpop_baseline_trigger(), {
          baseline_inputs <- subpop_baseline_trigger()
          
          # Check if we have all required inputs and valid demographics
          if (!is.null(baseline_inputs$fit_before) && !is.null(baseline_inputs$colon_before) &&
              !is.null(baseline_inputs$diag_before) && !is.null(baseline_inputs$total_pop) &&
              !is.null(baseline_inputs$model_type) && 
              subpop_gender_total() == 100 && subpop_race_total() == 100 && subpop_age_total() == 100) {
            
            baseline_computing_state(TRUE)
            
            # Generate baseline prediction
            generate_subpop_baseline_prediction(subpop_id, baseline_inputs)
            
            baseline_computing_state(FALSE)
          }
        })
        
        # Generate baseline prediction function
        generate_subpop_baseline_prediction <- function(subpop_id, baseline_inputs) {
          data <- training_data()
          if (is.null(data)) return()
          
          # Train models for baseline
          trained_models <- train_models_with_progress(data, baseline_inputs$model_type)
          if (is.null(trained_models)) return()
          
          # Compute demographic weights
          current_weights <- compute_subpop_demographic_weights(baseline_inputs)
          
          # Prepare input data
          input_data <- data.frame(
            FIT_before = baseline_inputs$fit_before,
            FIT_during = baseline_inputs$fit_during,
            FIT_after = baseline_inputs$fit_after,
            COLON_before = baseline_inputs$colon_before,
            COLON_during = baseline_inputs$colon_during,
            COLON_after = baseline_inputs$colon_after,
            DIAG_before = baseline_inputs$diag_before,
            DIAG_during = baseline_inputs$diag_during,
            DIAG_after = baseline_inputs$diag_after
          )
          
          # Make predictions
          predict_total <- function(models) {
            preds <- sapply(paste0("P_", 1:180), function(p) {
              safe_model_predict(models[[p]], input_data)
            })
            sum(preds * current_weights, na.rm = TRUE)
          }
          
          baseline_results <- list(
            cca = round(predict_total(trained_models$cca), 2),
            lyl = round(predict_total(trained_models$lyl), 2),
            cd = round(predict_total(trained_models$cd), 2),
            model_type = baseline_inputs$model_type,
            total_pop = baseline_inputs$total_pop,
            scenario_name = "Usual Care (Baseline)",
            parameters = baseline_inputs
          )
          
          # Store baseline results
          current_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
          if (is.null(current_results)) current_results <- list()
          current_results[["baseline"]] <- baseline_results
          subpop_states[[paste0(subpop_id, "_scenario_results")]] <- current_results
        }
        
        # Scenario names UI
        output[[paste0(subpop_id, "_scenario_names_ui")]] <- renderUI({
          num_scenarios <- input[[paste0(subpop_id, "_num_scenarios")]]
          if (is.null(num_scenarios) || num_scenarios < 1) return(NULL)
          
          current_names <- subpop_states[[paste0(subpop_id, "_scenario_names")]]
          if (is.null(current_names)) current_names <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5")
          
          name_inputs <- lapply(1:num_scenarios, function(i) {
            fluidRow(
              column(2,
                     div(style = "margin-top: 5px;", strong(paste("Scenario", i, ":")))
              ),
              column(10,
                     textInput(paste0(subpop_id, "_scenario_name_", i), NULL,
                               value = if(i <= length(current_names)) current_names[i] else paste("Scenario", i),
                               placeholder = paste("Enter name for scenario", i))
              )
            )
          })
          
          do.call(tagList, name_inputs)
        })
        
        # Apply scenario names
        observeEvent(input[[paste0(subpop_id, "_apply_names")]], {
          num_scenarios <- input[[paste0(subpop_id, "_num_scenarios")]]
          if (is.null(num_scenarios)) return()
          
          new_names <- sapply(1:num_scenarios, function(i) {
            name_input <- input[[paste0(subpop_id, "_scenario_name_", i)]]
            if (is.null(name_input) || name_input == "") {
              paste("Scenario", i)
            } else {
              name_input
            }
          })
          
          # Pad with default names if needed
          all_names <- c(new_names, paste("Scenario", (length(new_names) + 1):5))
          subpop_states[[paste0(subpop_id, "_scenario_names")]] <- all_names[1:5]
          
          showNotification(paste("Scenario names updated for", subpop_info$name), type = "message", duration = 2)
        })
        
        # Dynamic Scenario Panels
        output[[paste0(subpop_id, "_scenario_panels")]] <- renderUI({
          num_scenarios <- input[[paste0(subpop_id, "_num_scenarios")]]
          if (is.null(num_scenarios) || num_scenarios < 1) return(NULL)
          
          scenario_panels <- lapply(1:num_scenarios, function(i) {
            create_subpop_scenario_panel(subpop_id, i)
          })
          
          do.call(tagList, scenario_panels)
        })
        
        # Create individual scenario panel for sub-population
        create_subpop_scenario_panel <- function(subpop_id, scenario_num) {
          scenario_name <- get_subpop_scenario_name(scenario_num)
          
          fluidRow(
            column(12,
                   wellPanel(
                     h6(scenario_name, style = "color: #2c3e50; border-bottom: 1px solid #3498db; padding-bottom: 5px;"),
                     fluidRow(
                       # Left Column - During/After Parameters
                       column(6,
                              h6("During Parameters"),
                              fluidRow(
                                column(6,
                                       div(
                                         numericInput(paste0("fit_during_", subpop_id, "_", scenario_num), "FIT During",
                                                      value = 10, min = 0, max = 30),
                                         bsTooltip(paste0("fit_during_", subpop_id, "_", scenario_num),
                                                   "FIT screening rate during (Range: 0-30%). Must be > Before value",
                                                   placement = "top", trigger = "hover")
                                       ),
                                       div(
                                         numericInput(paste0("colon_during_", subpop_id, "_", scenario_num), "COLON During",
                                                      value = 55, min = 30, max = 70),
                                         bsTooltip(paste0("colon_during_", subpop_id, "_", scenario_num),
                                                   "Colonoscopy rate during (Range: 30-70%). Must be > Before value",
                                                   placement = "top", trigger = "hover")
                                       ),
                                       div(
                                         numericInput(paste0("diag_during_", subpop_id, "_", scenario_num), "Diagnostic During",
                                                      value = 10, min = 0, max = 90),
                                         bsTooltip(paste0("diag_during_", subpop_id, "_", scenario_num),
                                                   "Diagnostic rate during (Range: 0-90%). Must be > Before value",
                                                   placement = "top", trigger = "hover")
                                       )
                                ),
                                column(6,
                                       h6("After Parameters"),
                                       div(
                                         numericInput(paste0("fit_after_", subpop_id, "_", scenario_num), "FIT After",
                                                      value = 9, min = 0, max = 30),
                                         bsTooltip(paste0("fit_after_", subpop_id, "_", scenario_num),
                                                   "FIT screening rate after (Range: 0-30%). Must be > Before and < During",
                                                   placement = "top", trigger = "hover")
                                       ),
                                       div(
                                         numericInput(paste0("colon_after_", subpop_id, "_", scenario_num), "COLON After",
                                                      value = 52, min = 30, max = 70),
                                         bsTooltip(paste0("colon_after_", subpop_id, "_", scenario_num),
                                                   "Colonoscopy rate after (Range: 30-70%). Must be > Before and < During",
                                                   placement = "top", trigger = "hover")
                                       ),
                                       div(
                                         numericInput(paste0("diag_after_", subpop_id, "_", scenario_num), "Diagnostic After",
                                                      value = 8, min = 0, max = 90),
                                         bsTooltip(paste0("diag_after_", subpop_id, "_", scenario_num),
                                                   "Diagnostic rate after (Range: 0-90%). Must be > Before and < During",
                                                   placement = "top", trigger = "hover")
                                       )
                                )
                              )
                       ),
                       
                       # Right Column - Run button and Results
                       column(6,
                              # Run button and status
                              div(style = "margin-top: 20px;",
                                  actionButton(paste0("run_sim_", subpop_id, "_", scenario_num),
                                               paste("Run Prediction for", get_subpop_scenario_name(scenario_num)),
                                               class = "btn-success",
                                               style = "width: 100%;")
                              ),
                              br(),
                              
                              # Validation messages
                              uiOutput(paste0("validation_messages_", subpop_id, "_", scenario_num)),
                              
                              # Status and Results
                              verbatimTextOutput(paste0("status_", subpop_id, "_", scenario_num)),
                              
                              conditionalPanel(
                                condition = paste0("output['status_", subpop_id, "_", scenario_num, "'] && output['status_", subpop_id, "_", scenario_num, "'].indexOf('Prediction Complete') > -1"),
                                wellPanel(
                                  style = "background-color: #d4edda; border-color: #c3e6cb;",
                                  h6("Results for", get_subpop_scenario_name(scenario_num), style = "color: #155724;"),
                                  verbatimTextOutput(paste0("results_", subpop_id, "_", scenario_num))
                                )
                              )
                       )
                     )
                   )
            )
          )
        }
        
        # Generate validation messages and run simulation observers for each scenario
        observe({
          num_scenarios <- input[[paste0(subpop_id, "_num_scenarios")]]
          if (is.null(num_scenarios)) return()
          
          for (i in 1:num_scenarios) {
            local({
              scenario_num <- i
              
              # Create validation message output
              output[[paste0("validation_messages_", subpop_id, "_", scenario_num)]] <- renderUI({
                scenario_inputs <- get_subpop_scenario_inputs(scenario_num)
                
                if (is.null(scenario_inputs$fit_during) || is.null(scenario_inputs$fit_after) ||
                    is.null(scenario_inputs$colon_during) || is.null(scenario_inputs$colon_after) ||
                    is.null(scenario_inputs$diag_during) || is.null(scenario_inputs$diag_after)) {
                  return(NULL)
                }
                
                errors <- validate_screening_parameters(
                  scenario_inputs$fit_before, scenario_inputs$fit_during, scenario_inputs$fit_after,
                  scenario_inputs$colon_before, scenario_inputs$colon_during, scenario_inputs$colon_after,
                  scenario_inputs$diag_before, scenario_inputs$diag_during, scenario_inputs$diag_after
                )
                
                if (length(errors) > 0) {
                  div(
                    style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
                    h6("⚠️ Parameter Validation Errors:", style = "color: #721c24; margin-bottom: 5px;"),
                    HTML(paste("• ", errors, collapse = "<br>"))
                  )
                } else {
                  div(
                    style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
                    h6("✓ All parameters are valid", style = "color: #155724;")
                  )
                }
              })
              
              # Create observer for run simulation button
              observeEvent(input[[paste0("run_sim_", subpop_id, "_", scenario_num)]], {
                scenario_inputs <- get_subpop_scenario_inputs(scenario_num)
                
                if (is.null(scenario_inputs$total_pop) || scenario_inputs$total_pop <= 0) {
                  showNotification("Please enter a valid total population", type = "error")
                  return()
                }
                
                # Validate parameters
                errors <- validate_screening_parameters(
                  scenario_inputs$fit_before, scenario_inputs$fit_during, scenario_inputs$fit_after,
                  scenario_inputs$colon_before, scenario_inputs$colon_during, scenario_inputs$colon_after,
                  scenario_inputs$diag_before, scenario_inputs$diag_during, scenario_inputs$diag_after
                )
                
                if (length(errors) > 0) {
                  showNotification(paste("Parameter validation failed:", paste(errors, collapse = "; ")),
                                   type = "error", duration = 8)
                  return()
                }
                
                # Show modal for scenario processing
                session$sendCustomMessage("showModal", list(
                  message = paste("Processing", get_scenario_name(scenario_num)),
                  detail = paste("Loading", scenario_inputs$model_type, "models and generating predictions...",
                                 "\n⏱️ Estimated time:", get_model_load_time(scenario_inputs$model_type))
                ))
                
                # Update status
                output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                  paste("Loading models for", get_subpop_scenario_name(scenario_num), "...")
                })
                
                # Get training data
                data <- training_data()
                if (is.null(data)) {
                  session$sendCustomMessage("hideModal", list())
                  output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                    "Error: Training data not available. Check data files."
                  })
                  return()
                }
                
                # Train models
                output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                  paste("Loading", scenario_inputs$model_type, "models for", get_subpop_scenario_name(scenario_num), "...")
                })
                
                trained_models <- train_models_with_progress(data, scenario_inputs$model_type)
                
                if (is.null(trained_models)) {
                  session$sendCustomMessage("hideModal", list())
                  output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                    "Error: Model loading/training failed."
                  })
                  return()
                }
                
                # Get weights
                current_weights <- compute_subpop_demographic_weights(scenario_inputs)
                
                if (is.null(current_weights) || all(current_weights == 0)) {
                  session$sendCustomMessage("hideModal", list())
                  output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                    "Error: Please enter population values."
                  })
                  return()
                }
                
                # Prepare input data
                input_data <- data.frame(
                  FIT_before = scenario_inputs$fit_before,
                  FIT_during = scenario_inputs$fit_during,
                  FIT_after = scenario_inputs$fit_after,
                  COLON_before = scenario_inputs$colon_before,
                  COLON_during = scenario_inputs$colon_during,
                  COLON_after = scenario_inputs$colon_after,
                  DIAG_before = scenario_inputs$diag_before,
                  DIAG_during = scenario_inputs$diag_during,
                  DIAG_after = scenario_inputs$diag_after
                )
                
                # Make predictions
                output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                  paste("Generating predictions for", get_subpop_scenario_name(scenario_num), "...")
                })
                
                predict_total <- function(models) {
                  preds <- sapply(paste0("P_", 1:180), function(p) {
                    safe_model_predict(models[[p]], input_data)
                  })
                  sum(preds * current_weights, na.rm = TRUE)
                }
                
                results <- list(
                  cca = round(predict_total(trained_models$cca), 2),
                  lyl = round(predict_total(trained_models$lyl), 2),
                  cd = round(predict_total(trained_models$cd), 2),
                  model_type = scenario_inputs$model_type,
                  total_pop = scenario_inputs$total_pop,
                  scenario_name = get_subpop_scenario_name(scenario_num),
                  parameters = scenario_inputs
                )
                
                # Store results
                current_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
                if (is.null(current_results)) current_results <- list()
                current_results[[paste0("scenario_", scenario_num)]] <- results
                subpop_states[[paste0(subpop_id, "_scenario_results")]] <- current_results
                
                # Hide modal and update status
                session$sendCustomMessage("hideModal", list())
                
                # Update status and results
                output[[paste0("status_", subpop_id, "_", scenario_num)]] <- renderText({
                  "✓ Prediction Complete!"
                })
                
                output[[paste0("results_", subpop_id, "_", scenario_num)]] <- renderText({
                  paste(
                    "Cancer Cases:", format(results$cca, big.mark = ","), "\n",
                    "Life Years Lost:", format(results$lyl, big.mark = ","), "\n",
                    "Cancer Deaths:", format(results$cd, big.mark = ","), "\n",
                    "Model Used:", results$model_type, "\n",
                    "Population:", format(results$total_pop, big.mark = ",")
                  )
                })
              })
            })
          }
        })
        
        # Comparison table for this sub-population
        output[[paste0(subpop_id, "_comparison_table")]] <- renderDT({
          results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
          if (is.null(results) || length(results) == 0) return(NULL)
          
          comparison_data <- data.frame(
            Scenario = character(),
            Model_Type = character(),
            Population = numeric(),
            Cancer_Cases = numeric(),
            Life_Years_Lost = numeric(),
            Cancer_Deaths = numeric(),
            stringsAsFactors = FALSE
          )
          
          # Add baseline first
          if (!is.null(results[["baseline"]])) {
            comparison_data <- rbind(comparison_data, data.frame(
              Scenario = results[["baseline"]]$scenario_name,
              Model_Type = results[["baseline"]]$model_type,
              Population = results[["baseline"]]$total_pop,
              Cancer_Cases = results[["baseline"]]$cca,
              Life_Years_Lost = results[["baseline"]]$lyl,
              Cancer_Deaths = results[["baseline"]]$cd,
              stringsAsFactors = FALSE
            ))
          }
          
          # Add scenario results
          num_scenarios <- input[[paste0(subpop_id, "_num_scenarios")]]
          if (!is.null(num_scenarios)) {
            for (i in 1:num_scenarios) {
              scenario_key <- paste0("scenario_", i)
              if (!is.null(results[[scenario_key]])) {
                scenario_display_name <- if (!is.null(results[[scenario_key]]$scenario_name)) {
                  results[[scenario_key]]$scenario_name
                } else {
                  get_subpop_scenario_name(i)
                }
                
                comparison_data <- rbind(comparison_data, data.frame(
                  Scenario = scenario_display_name,
                  Model_Type = results[[scenario_key]]$model_type,
                  Population = results[[scenario_key]]$total_pop,
                  Cancer_Cases = results[[scenario_key]]$cca,
                  Life_Years_Lost = results[[scenario_key]]$lyl,
                  Cancer_Deaths = results[[scenario_key]]$cd,
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
          
          if (nrow(comparison_data) == 0) return(NULL)
          
          # Format numbers with commas
          comparison_data$Population <- format(comparison_data$Population, big.mark = ",")
          comparison_data$Cancer_Cases <- format(comparison_data$Cancer_Cases, big.mark = ",")
          comparison_data$Life_Years_Lost <- format(comparison_data$Life_Years_Lost, big.mark = ",")
          comparison_data$Cancer_Deaths <- format(comparison_data$Cancer_Deaths, big.mark = ",")
          
          datatable(comparison_data,
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      pageLength = -1
                    ),
                    rownames = FALSE) %>%
            formatStyle(columns = c(1:6),
                        backgroundColor = 'rgba(0,123,255,0.1)',
                        border = '1px solid #ddd') %>%
            formatStyle(
              "Scenario",
              target = "row",
              backgroundColor = styleEqual("Usual Care (Baseline)", "rgba(255,193,7,0.2)")
            )
        })
        
        # Download handler for this sub-population
        output[[paste0(subpop_id, "_download_comparison")]] <- downloadHandler(
          filename = function() paste0(subpop_info$name, "_analysis_", Sys.Date(), ".csv"),
          content = function(file) {
            results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
            if (is.null(results) || length(results) == 0) return(NULL)
            
            # Similar comprehensive export as single population
            comprehensive_data <- data.frame(
              Sub_Population = character(),
              Scenario_Name = character(),
              Model_Type = character(),
              Total_Population = numeric(),
              Cancer_Cases = numeric(),
              Life_Years_Lost = numeric(),
              Cancer_Deaths = numeric(),
              # Demographics
              Male_Percent = numeric(),
              Female_Percent = numeric(),
              White_Percent = numeric(),
              Black_Percent = numeric(),
              Other_Race_Percent = numeric(),
              Age_45_49_Percent = numeric(),
              Age_50_54_Percent = numeric(),
              Age_55_59_Percent = numeric(),
              Age_60_64_Percent = numeric(),
              Age_65_69_Percent = numeric(),
              Age_70_74_Percent = numeric(),
              # Screening Parameters
              FIT_Before = numeric(),
              FIT_During = numeric(),
              FIT_After = numeric(),
              Colonoscopy_Before = numeric(),
              Colonoscopy_During = numeric(),
              Colonoscopy_After = numeric(),
              Diagnostic_Before = numeric(),
              Diagnostic_During = numeric(),
              Diagnostic_After = numeric(),
              # Analysis metadata
              Analysis_Date = character(),
              Scenario_Type = character(),
              stringsAsFactors = FALSE
            )
            
            # Add baseline if exists
            if (!is.null(results[["baseline"]])) {
              baseline <- results[["baseline"]]
              comprehensive_data <- rbind(comprehensive_data, data.frame(
                Sub_Population = subpop_info$name,
                Scenario_Name = baseline$scenario_name,
                Model_Type = baseline$model_type,
                Total_Population = baseline$total_pop,
                Cancer_Cases = baseline$cca,
                Life_Years_Lost = baseline$lyl,
                Cancer_Deaths = baseline$cd,
                # Demographics
                Male_Percent = baseline$parameters$male_pct,
                Female_Percent = baseline$parameters$female_pct,
                White_Percent = baseline$parameters$white_pct,
                Black_Percent = baseline$parameters$black_pct,
                Other_Race_Percent = baseline$parameters$other_pct,
                Age_45_49_Percent = baseline$parameters$age_45_49,
                Age_50_54_Percent = baseline$parameters$age_50_54,
                Age_55_59_Percent = baseline$parameters$age_55_59,
                Age_60_64_Percent = baseline$parameters$age_60_64,
                Age_65_69_Percent = baseline$parameters$age_65_69,
                Age_70_74_Percent = baseline$parameters$age_70_74,
                # Screening Parameters
                FIT_Before = baseline$parameters$fit_before,
                FIT_During = baseline$parameters$fit_during,
                FIT_After = baseline$parameters$fit_after,
                Colonoscopy_Before = baseline$parameters$colon_before,
                Colonoscopy_During = baseline$parameters$colon_during,
                Colonoscopy_After = baseline$parameters$colon_after,
                Diagnostic_Before = baseline$parameters$diag_before,
                Diagnostic_During = baseline$parameters$diag_during,
                Diagnostic_After = baseline$parameters$diag_after,
                # Analysis metadata
                Analysis_Date = as.character(Sys.Date()),
                Scenario_Type = "Baseline",
                stringsAsFactors = FALSE
              ))
            }
            
            # Add scenarios
            num_scenarios <- input[[paste0(subpop_id, "_num_scenarios")]]
            if (!is.null(num_scenarios)) {
              for (i in 1:num_scenarios) {
                scenario_key <- paste0("scenario_", i)
                if (!is.null(results[[scenario_key]])) {
                  scenario <- results[[scenario_key]]
                  scenario_display_name <- if (!is.null(scenario$scenario_name)) {
                    scenario$scenario_name
                  } else {
                    get_subpop_scenario_name(i)
                  }
                  
                  comprehensive_data <- rbind(comprehensive_data, data.frame(
                    Sub_Population = subpop_info$name,
                    Scenario_Name = scenario_display_name,
                    Model_Type = scenario$model_type,
                    Total_Population = scenario$total_pop,
                    Cancer_Cases = scenario$cca,
                    Life_Years_Lost = scenario$lyl,
                    Cancer_Deaths = scenario$cd,
                    # Demographics
                    Male_Percent = scenario$parameters$male_pct,
                    Female_Percent = scenario$parameters$female_pct,
                    White_Percent = scenario$parameters$white_pct,
                    Black_Percent = scenario$parameters$black_pct,
                    Other_Race_Percent = scenario$parameters$other_pct,
                    Age_45_49_Percent = scenario$parameters$age_45_49,
                    Age_50_54_Percent = scenario$parameters$age_50_54,
                    Age_55_59_Percent = scenario$parameters$age_55_59,
                    Age_60_64_Percent = scenario$parameters$age_60_64,
                    Age_65_69_Percent = scenario$parameters$age_65_69,
                    Age_70_74_Percent = scenario$parameters$age_70_74,
                    # Screening Parameters
                    FIT_Before = scenario$parameters$fit_before,
                    FIT_During = scenario$parameters$fit_during,
                    FIT_After = scenario$parameters$fit_after,
                    Colonoscopy_Before = scenario$parameters$colon_before,
                    Colonoscopy_During = scenario$parameters$colon_during,
                    Colonoscopy_After = scenario$parameters$colon_after,
                    Diagnostic_Before = scenario$parameters$diag_before,
                    Diagnostic_During = scenario$parameters$diag_during,
                    Diagnostic_After = scenario$parameters$diag_after,
                    # Analysis metadata
                    Analysis_Date = as.character(Sys.Date()),
                    Scenario_Type = paste("Comparison_Scenario", i),
                    stringsAsFactors = FALSE
                  ))
                }
              }
            }
            
            write.csv(comprehensive_data, file, row.names = FALSE, na = "")
          }
        )
      })
    }
  })
  
  # =======================================================================================
  # SINGLE POPULATION LOGIC (YOUR ORIGINAL CODE WITH MINOR ADAPTATIONS)
  # =======================================================================================
  
  # Demographics toggle observer
  observeEvent(input$toggle_demographics, {
    current_state <- demographics_visible()
    demographics_visible(!current_state)
    
    # Update button icon
    if (!current_state) {
      updateActionButton(session, "toggle_demographics",
                         icon = icon("chevron-up"))
    } else {
      updateActionButton(session, "toggle_demographics",
                         icon = icon("chevron-down"))
    }
  })
  
  gender_total <- reactive({
    input$global_male_pct + input$global_female_pct
  })
  
  race_total <- reactive({
    input$global_white_pct + input$global_black_pct + input$global_other_pct
  })
  
  age_total <- reactive({
    input$global_age_45_49 + input$global_age_50_54 + input$global_age_55_59 +
      input$global_age_60_64 + input$global_age_65_69 + input$global_age_70_74
  })
  
  # Validation outputs
  output$gender_validation <- renderUI({
    total <- gender_total()
    if (!is.null(total) && total != 100) {
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px; margin: 5px 0;",
        strong("⚠️ Gender Error: ", style = "color: #721c24;"),
        span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
      )
    }
  })
  
  output$race_validation <- renderUI({
    total <- race_total()
    if (!is.null(total) && total != 100) {
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px; margin: 5px 0;",
        strong("⚠️ Race Error: ", style = "color: #721c24;"),
        span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
      )
    }
  })
  
  output$age_validation <- renderUI({
    total <- age_total()
    if (!is.null(total) && total != 100) {
      div(
        style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 8px; margin: 5px 0;",
        strong("⚠️ Age Groups Error: ", style = "color: #721c24;"),
        span(paste("Total =", total, "%. Must equal 100%."), style = "color: #721c24;")
      )
    }
  })
  
  # Observer for demographic changes that triggers recalculation
  observe({
    # List of all demographic inputs
    demographic_inputs <- list(
      input$global_male_pct, input$global_female_pct,
      input$global_white_pct, input$global_black_pct, input$global_other_pct,
      input$global_age_45_49, input$global_age_50_54, input$global_age_55_59,
      input$global_age_60_64, input$global_age_65_69, input$global_age_70_74
    )
    
    # Only proceed if all inputs are available
    if (all(!sapply(demographic_inputs, is.null))) {
      # Clear all existing scenario results to force recalculation
      scenario_results(list())
      
      # Show notification about demographic update
      showNotification("Demographics updated - recalculating all scenarios...",
                       type = "message", duration = 3)
    }
  })
  
  # Generate baseline scenario automatically
  baseline_inputs_reactive <- reactive({
    list(
      total_pop = input$global_total_pop,
      male_pct = input$global_male_pct,
      female_pct = input$global_female_pct,
      white_pct = input$global_white_pct,
      black_pct = input$global_black_pct,
      other_pct = input$global_other_pct,
      age_45_49 = input$global_age_45_49,
      age_50_54 = input$global_age_50_54,
      age_55_59 = input$global_age_55_59,
      age_60_64 = input$global_age_60_64,
      age_65_69 = input$global_age_65_69,
      age_70_74 = input$global_age_70_74,
      fit_before = input$global_fit_before,
      fit_during = input$global_fit_before,
      fit_after = input$global_fit_before,
      colon_before = input$global_colon_before,
      colon_during = input$global_colon_before,
      colon_after = input$global_colon_before,
      diag_before = input$global_diag_before,
      diag_during = input$global_diag_before,
      diag_after = input$global_diag_before,
      model_type = input$global_model_type
    )
  })
  
  # Use observeEvent instead of observe, and debounce it
  baseline_trigger <- debounce(baseline_inputs_reactive, 1000) # 1 second delay
  
  observeEvent(baseline_trigger(), {
    baseline_inputs <- baseline_trigger()
    
    # Check if we have all required inputs
    if (!is.null(baseline_inputs$fit_before) && !is.null(baseline_inputs$colon_before) &&
        !is.null(baseline_inputs$diag_before) && !is.null(baseline_inputs$total_pop) &&
        !is.null(baseline_inputs$model_type) && gender_total() == 100 && race_total() == 100 && age_total() == 100) {
      
      session$sendCustomMessage("showModal", list(
        message = "Computing Baseline Model",
        detail = paste("Loading", baseline_inputs$model_type, "models and processing baseline predictions...",
                       "\n⏱️ Estimated time:", get_model_load_time(baseline_inputs$model_type))
      ))
      
      # Generate baseline prediction
      generate_baseline_prediction(baseline_inputs)
      
      # Hide modal
      session$sendCustomMessage("hideModal", list())
    }
  })
  
  # Function to generate baseline prediction
  generate_baseline_prediction <- function(baseline_inputs) {
    data <- training_data()
    if (is.null(data)) return()
    
    # Train models for baseline
    trained_models <- train_models_with_progress(data, baseline_inputs$model_type)
    if (is.null(trained_models)) return()
    
    # Compute demographic weights
    current_weights <- compute_demographic_weights(baseline_inputs)
    
    # Prepare input data
    input_data <- data.frame(
      FIT_before = baseline_inputs$fit_before,
      FIT_during = baseline_inputs$fit_during,
      FIT_after = baseline_inputs$fit_after,
      COLON_before = baseline_inputs$colon_before,
      COLON_during = baseline_inputs$colon_during,
      COLON_after = baseline_inputs$colon_after,
      DIAG_before = baseline_inputs$diag_before,
      DIAG_during = baseline_inputs$diag_during,
      DIAG_after = baseline_inputs$diag_after
    )
    
    # Make predictions
    predict_total <- function(models) {
      preds <- sapply(paste0("P_", 1:180), function(p) {
        safe_model_predict(models[[p]], input_data)
      })
      sum(preds * current_weights, na.rm = TRUE)
    }
    
    baseline_results <- list(
      cca = round(predict_total(trained_models$cca), 2),
      lyl = round(predict_total(trained_models$lyl), 2),
      cd = round(predict_total(trained_models$cd), 2),
      model_type = baseline_inputs$model_type,
      total_pop = baseline_inputs$total_pop,
      scenario_name = "Usual Care (Baseline)",
      # Store all parameters for download
      parameters = baseline_inputs
    )
    
    # Store baseline results
    current_results <- scenario_results()
    current_results[["baseline"]] <- baseline_results
    scenario_results(current_results)
  }
  
  # Generate scenario names UI
  output$scenario_names_ui <- renderUI({
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios) || num_scenarios < 1) return(NULL)
    
    current_names <- scenario_names()
    
    name_inputs <- lapply(1:num_scenarios, function(i) {
      fluidRow(
        column(2,
               div(style = "margin-top: 5px;", strong(paste("Scenario", i, ":")))
        ),
        column(10,
               textInput(paste0("scenario_name_", i), NULL,
                         value = if(i <= length(current_names)) current_names[i] else paste("Scenario", i),
                         placeholder = paste("Enter name for scenario", i))
        )
      )
    })
    
    do.call(tagList, name_inputs)
  })
  
  # Apply scenario names
  observeEvent(input$apply_names, {
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios)) return()
    
    new_names <- sapply(1:num_scenarios, function(i) {
      name_input <- input[[paste0("scenario_name_", i)]]
      if (is.null(name_input) || name_input == "") {
        paste("Scenario", i)
      } else {
        name_input
      }
    })
    
    # Pad with default names if needed
    all_names <- c(new_names, paste("Scenario", (length(new_names) + 1):5))
    scenario_names(all_names[1:5])
    
    showNotification("Scenario names updated!", type = "message", duration = 2)
  })
  
  # Get current scenario name
  get_scenario_name <- function(scenario_num) {
    current_names <- scenario_names()
    if (scenario_num <= length(current_names)) {
      return(current_names[scenario_num])
    } else {
      return(paste("Scenario", scenario_num))
    }
  }
  
  # Global census defaults
  observeEvent(input$use_global_census, {
    updateSliderInput(session, "global_male_pct", value = 49)
    updateSliderInput(session, "global_female_pct", value = 51)
    updateSliderInput(session, "global_white_pct", value = 60)
    updateSliderInput(session, "global_black_pct", value = 13)
    updateSliderInput(session, "global_other_pct", value = 27)
    updateSliderInput(session, "global_age_45_49", value = 17)
    updateSliderInput(session, "global_age_50_54", value = 17)
    updateSliderInput(session, "global_age_55_59", value = 16)
    updateSliderInput(session, "global_age_60_64", value = 16)
    updateSliderInput(session, "global_age_65_69", value = 16)
    updateSliderInput(session, "global_age_70_74", value = 18)
    
    showNotification("Census defaults applied!", type = "message", duration = 2)
  })
  
  # Global clear values
  observeEvent(input$clear_global, {
    updateNumericInput(session, "global_total_pop", value = 100000)
    updateSelectInput(session, "global_model_type", selected = "Linear Regression")
    updateNumericInput(session, "global_fit_before", value = 8)
    updateNumericInput(session, "global_colon_before", value = 48)
    updateNumericInput(session, "global_diag_before", value = 7)
    updateSliderInput(session, "global_male_pct", value = 50)
    updateSliderInput(session, "global_female_pct", value = 50)
    updateSliderInput(session, "global_white_pct", value = 60)
    updateSliderInput(session, "global_black_pct", value = 13)
    updateSliderInput(session, "global_other_pct", value = 27)
    updateSliderInput(session, "global_age_45_49", value = 17)
    updateSliderInput(session, "global_age_50_54", value = 17)
    updateSliderInput(session, "global_age_55_59", value = 16)
    updateSliderInput(session, "global_age_60_64", value = 16)
    updateSliderInput(session, "global_age_65_69", value = 16)
    updateSliderInput(session, "global_age_70_74", value = 18)
    
    # Clear scenario results
    scenario_results(list())
    
    showNotification("All values cleared!", type = "message", duration = 2)
  })
  
  
  output$scenario_panels <- renderUI({
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios) || num_scenarios < 1) return(NULL)
    
    scenario_panels <- lapply(1:num_scenarios, function(i) {
      create_scenario_panel(i)
    })
    
    do.call(tagList, scenario_panels)
  })
  
  # Function to create individual scenario panel
  create_scenario_panel <- function(scenario_num) {
    scenario_name <- get_scenario_name(scenario_num)
    
    fluidRow(
      column(12,
             wellPanel(
               h3(scenario_name, style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
               fluidRow(
                 # Left Column - During/After Parameters
                 column(6,
                        h5("During Parameters"),
                        fluidRow(
                          column(6,
                                 div(
                                   numericInput(paste0("fit_during_", scenario_num), "FIT During",
                                                value = 10, min = 0, max = 30),
                                   bsTooltip(paste0("fit_during_", scenario_num),
                                             "FIT screening rate during (Range: 0-30%). Must be > Before value",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("colon_during_", scenario_num), "COLON During",
                                                value = 55, min = 30, max = 70),
                                   bsTooltip(paste0("colon_during_", scenario_num),
                                             "Colonoscopy rate during (Range: 30-70%). Must be > Before value",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("diag_during_", scenario_num), "Diagnostic During",
                                                value = 10, min = 0, max = 90),
                                   bsTooltip(paste0("diag_during_", scenario_num),
                                             "Diagnostic rate during (Range: 0-90%). Must be > Before value",
                                             placement = "top", trigger = "hover")
                                 )
                          ),
                          column(6,
                                 h6("After Parameters"),
                                 div(
                                   numericInput(paste0("fit_after_", scenario_num), "FIT After",
                                                value = 9, min = 0, max = 30),
                                   bsTooltip(paste0("fit_after_", scenario_num),
                                             "FIT screening rate after (Range: 0-30%). Must be > Before and < During",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("colon_after_", scenario_num), "COLON After",
                                                value = 52, min = 30, max = 70),
                                   bsTooltip(paste0("colon_after_", scenario_num),
                                             "Colonoscopy rate after (Range: 30-70%). Must be > Before and < During",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("diag_after_", scenario_num), "Diagnostic After",
                                                value = 8, min = 0, max = 90),
                                   bsTooltip(paste0("diag_after_", scenario_num),
                                             "Diagnostic rate after (Range: 0-90%). Must be > Before and < During",
                                             placement = "top", trigger = "hover")
                                 )
                          )
                        )
                 ),
                 
                 # Right Column - Run button and Results
                 column(6,
                        # Run button and status
                        div(style = "margin-top: 20px;",
                            actionButton(paste0("run_sim_", scenario_num),
                                         paste("Run Prediction for", get_scenario_name(scenario_num)),
                                         class = "btn-success btn-lg",
                                         style = "width: 100%;")
                        ),
                        br(),
                        
                        # Validation messages
                        uiOutput(paste0("validation_messages_", scenario_num)),
                        
                        # Status and Results
                        verbatimTextOutput(paste0("status_", scenario_num)),
                        
                        conditionalPanel(
                          condition = paste0("output['status_", scenario_num, "'] && output['status_", scenario_num, "'].indexOf('Prediction Complete') > -1"),
                          wellPanel(
                            style = "background-color: #d4edda; border-color: #c3e6cb;",
                            h5("Results for", get_scenario_name(scenario_num), style = "color: #155724;"),
                            verbatimTextOutput(paste0("results_", scenario_num))
                          )
                        )
                 )
               )
             )
      )
    )
  }
  
  
  # Compute demographic weights for a scenario
  compute_demographic_weights <- function(scenario_inputs) {
    gender <- c(male = scenario_inputs$male_pct / 100, female = scenario_inputs$female_pct / 100)
    race <- c(white = scenario_inputs$white_pct / 100, black = scenario_inputs$black_pct / 100, other = scenario_inputs$other_pct / 100)
    age <- c(
      "45-49" = scenario_inputs$age_45_49 / 100,
      "50-54" = scenario_inputs$age_50_54 / 100,
      "55-59" = scenario_inputs$age_55_59 / 100,
      "60-64" = scenario_inputs$age_60_64 / 100,
      "65-69" = scenario_inputs$age_65_69 / 100,
      "70-74" = scenario_inputs$age_70_74 / 100
    )
    
    weights_vec <- numeric(180)
    for (g in names(gender)) {
      for (r in names(race)) {
        for (a in names(age)) {
          subpop <- scenario_inputs$total_pop * gender[g] * race[r] * age[a]
          matches <- mapping_df %>%
            filter(Gender == g, Race == r, AgeGroup == a)
          if (nrow(matches) > 0) {
            per_capita <- subpop / nrow(matches)
            weights_vec[matches$P_values] <- weights_vec[matches$P_values] + per_capita
          }
        }
      }
    }
    round(weights_vec, 2)
  }
  
  # Helper function to get scenario inputs
  get_scenario_inputs <- function(scenario_num) {
    list(
      total_pop = input$global_total_pop,
      male_pct = input$global_male_pct,
      female_pct = input$global_female_pct,
      white_pct = input$global_white_pct,
      black_pct = input$global_black_pct,
      other_pct = input$global_other_pct,
      age_45_49 = input$global_age_45_49,
      age_50_54 = input$global_age_50_54,
      age_55_59 = input$global_age_55_59,
      age_60_64 = input$global_age_60_64,
      age_65_69 = input$global_age_65_69,
      age_70_74 = input$global_age_70_74,
      fit_before = input$global_fit_before,
      fit_during = input[[paste0("fit_during_", scenario_num)]],
      fit_after = input[[paste0("fit_after_", scenario_num)]],
      colon_before = input$global_colon_before,
      colon_during = input[[paste0("colon_during_", scenario_num)]],
      colon_after = input[[paste0("colon_after_", scenario_num)]],
      diag_before = input$global_diag_before,
      diag_during = input[[paste0("diag_during_", scenario_num)]],
      diag_after = input[[paste0("diag_after_", scenario_num)]],
      model_type = input$global_model_type
    )
  }
  
  # Generate validation messages and run simulation observers for each scenario
  observe({
    num_scenarios <- input$num_scenarios
    if (is.null(num_scenarios)) return()
    
    for (i in 1:num_scenarios) {
      local({
        scenario_num <- i
        
        # Create validation message output
        output[[paste0("validation_messages_", scenario_num)]] <- renderUI({
          scenario_inputs <- get_scenario_inputs(scenario_num)
          
          if (is.null(scenario_inputs$fit_during) || is.null(scenario_inputs$fit_after) ||
              is.null(scenario_inputs$colon_during) || is.null(scenario_inputs$colon_after) ||
              is.null(scenario_inputs$diag_during) || is.null(scenario_inputs$diag_after)) {
            return(NULL)
          }
          
          errors <- validate_screening_parameters(
            scenario_inputs$fit_before, scenario_inputs$fit_during, scenario_inputs$fit_after,
            scenario_inputs$colon_before, scenario_inputs$colon_during, scenario_inputs$colon_after,
            scenario_inputs$diag_before, scenario_inputs$diag_during, scenario_inputs$diag_after
          )
          
          if (length(errors) > 0) {
            div(
              style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
              h6("⚠️ Parameter Validation Errors:", style = "color: #721c24; margin-bottom: 5px;"),
              HTML(paste("• ", errors, collapse = "<br>"))
            )
          } else {
            div(
              style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px; padding: 10px; margin: 10px 0;",
              h6("✓ All parameters are valid", style = "color: #155724;")
            )
          }
        })
        
        # Create observer for run simulation button
        observeEvent(input[[paste0("run_sim_", scenario_num)]], {
          scenario_inputs <- get_scenario_inputs(scenario_num)
          
          if (is.null(scenario_inputs$total_pop) || scenario_inputs$total_pop <= 0) {
            showNotification("Please enter a valid total population", type = "error")
            return()
          }
          
          # Validate parameters
          errors <- validate_screening_parameters(
            scenario_inputs$fit_before, scenario_inputs$fit_during, scenario_inputs$fit_after,
            scenario_inputs$colon_before, scenario_inputs$colon_during, scenario_inputs$colon_after,
            scenario_inputs$diag_before, scenario_inputs$diag_during, scenario_inputs$diag_after
          )
          
          if (length(errors) > 0) {
            showNotification(paste("Parameter validation failed:", paste(errors, collapse = "; ")),
                             type = "error", duration = 8)
            return()
          }
          
          # Show modal for scenario processing
          session$sendCustomMessage("showModal", list(
            message = paste("Processing", get_subpop_scenario_name(scenario_num), "for", subpop_info$name),
            detail = paste("Loading", scenario_inputs$model_type, "models and generating predictions...",
                           "\n⏱️ Estimated time:", get_model_load_time(scenario_inputs$model_type))
          ))
          
          # Update status
          output[[paste0("status_", scenario_num)]] <- renderText({
            paste("Loading models for", get_scenario_name(scenario_num), "...")
          })
          
          # Get training data
          data <- training_data()
          if (is.null(data)) {
            session$sendCustomMessage("hideModal", list())
            output[[paste0("status_", scenario_num)]] <- renderText({
              "Error: Training data not available. Check data files."
            })
            return()
          }
          
          # Train models
          output[[paste0("status_", scenario_num)]] <- renderText({
            paste("Loading", scenario_inputs$model_type, "models for", get_scenario_name(scenario_num), "...")
          })
          
          trained_models <- train_models_with_progress(data, scenario_inputs$model_type)
          
          if (is.null(trained_models)) {
            session$sendCustomMessage("hideModal", list())
            output[[paste0("status_", scenario_num)]] <- renderText({
              "Error: Model loading/training failed."
            })
            return()
          }
          
          # Get weights
          current_weights <- compute_demographic_weights(scenario_inputs)
          
          if (is.null(current_weights) || all(current_weights == 0)) {
            session$sendCustomMessage("hideModal", list())
            output[[paste0("status_", scenario_num)]] <- renderText({
              "Error: Please enter population values."
            })
            return()
          }
          
          # Prepare input data
          input_data <- data.frame(
            FIT_before = scenario_inputs$fit_before,
            FIT_during = scenario_inputs$fit_during,
            FIT_after = scenario_inputs$fit_after,
            COLON_before = scenario_inputs$colon_before,
            COLON_during = scenario_inputs$colon_during,
            COLON_after = scenario_inputs$colon_after,
            DIAG_before = scenario_inputs$diag_before,
            DIAG_during = scenario_inputs$diag_during,
            DIAG_after = scenario_inputs$diag_after
          )
          
          # Make predictions
          output[[paste0("status_", scenario_num)]] <- renderText({
            paste("Generating predictions for", get_scenario_name(scenario_num), "...")
          })
          
          predict_total <- function(models) {
            preds <- sapply(paste0("P_", 1:180), function(p) {
              safe_model_predict(models[[p]], input_data)
            })
            sum(preds * current_weights, na.rm = TRUE)
          }
          
          results <- list(
            cca = round(predict_total(trained_models$cca), 2),
            lyl = round(predict_total(trained_models$lyl), 2),
            cd = round(predict_total(trained_models$cd), 2),
            model_type = scenario_inputs$model_type,
            total_pop = scenario_inputs$total_pop,
            scenario_name = get_scenario_name(scenario_num),
            # Store all parameters for download
            parameters = scenario_inputs
          )
          
          # Store results
          current_results <- scenario_results()
          current_results[[paste0("scenario_", scenario_num)]] <- results
          scenario_results(current_results)
          
          # Hide modal and update status
          session$sendCustomMessage("hideModal", list())
          
          # Update status and results
          output[[paste0("status_", scenario_num)]] <- renderText({
            "✓ Prediction Complete!"
          })
          
          output[[paste0("results_", scenario_num)]] <- renderText({
            paste(
              "Cancer Cases:", format(results$cca, big.mark = ","), "\n",
              "Life Years Lost:", format(results$lyl, big.mark = ","), "\n",
              "Cancer Deaths:", format(results$cd, big.mark = ","), "\n",
              "Model Used:", results$model_type, "\n",
              "Population:", format(results$total_pop, big.mark = ",")
            )
          })
        })
      })
    }
  })
  
  # Comparison table (existing code)
  output$comparison_table <- renderDT({
    results <- scenario_results()
    if (length(results) == 0) return(NULL)
    
    comparison_data <- data.frame(
      Scenario = character(),
      Model_Type = character(),
      Population = numeric(),
      Cancer_Cases = numeric(),
      Life_Years_Lost = numeric(),
      Cancer_Deaths = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Add baseline first
    if (!is.null(results[["baseline"]])) {
      comparison_data <- rbind(comparison_data, data.frame(
        Scenario = results[["baseline"]]$scenario_name,
        Model_Type = results[["baseline"]]$model_type,
        Population = results[["baseline"]]$total_pop,
        Cancer_Cases = results[["baseline"]]$cca,
        Life_Years_Lost = results[["baseline"]]$lyl,
        Cancer_Deaths = results[["baseline"]]$cd,
        stringsAsFactors = FALSE
      ))
    }
    
    # Add scenario results
    for (i in 1:input$num_scenarios) {
      scenario_key <- paste0("scenario_", i)
      if (!is.null(results[[scenario_key]])) {
        scenario_display_name <- if (!is.null(results[[scenario_key]]$scenario_name)) {
          results[[scenario_key]]$scenario_name
        } else {
          get_scenario_name(i)
        }
        
        comparison_data <- rbind(comparison_data, data.frame(
          Scenario = scenario_display_name,
          Model_Type = results[[scenario_key]]$model_type,
          Population = results[[scenario_key]]$total_pop,
          Cancer_Cases = results[[scenario_key]]$cca,
          Life_Years_Lost = results[[scenario_key]]$lyl,
          Cancer_Deaths = results[[scenario_key]]$cd,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if (nrow(comparison_data) == 0) return(NULL)
    
    # Format numbers with commas
    comparison_data$Population <- format(comparison_data$Population, big.mark = ",")
    comparison_data$Cancer_Cases <- format(comparison_data$Cancer_Cases, big.mark = ",")
    comparison_data$Life_Years_Lost <- format(comparison_data$Life_Years_Lost, big.mark = ",")
    comparison_data$Cancer_Deaths <- format(comparison_data$Cancer_Deaths, big.mark = ",")
    
    datatable(comparison_data,
              options = list(
                dom = 't',
                scrollX = TRUE,
                pageLength = -1
              ),
              rownames = FALSE) %>%
      formatStyle(columns = c(1:6),
                  backgroundColor = 'rgba(0,123,255,0.1)',
                  border = '1px solid #ddd') %>%
      formatStyle(
        "Scenario",
        target = "row",
        backgroundColor = styleEqual("Usual Care (Baseline)", "rgba(255,193,7,0.2)")
      )
  })
  
  # Enhanced download comparison results with all parameters
  output$download_comparison <- downloadHandler(
    filename = function() paste0("comprehensive_scenario_analysis_", Sys.Date(), ".csv"),
    content = function(file) {
      results <- scenario_results()
      if (length(results) == 0) return(NULL)
      
      comprehensive_data <- data.frame(
        Scenario_Name = character(),
        Model_Type = character(),
        Total_Population = numeric(),
        Cancer_Cases = numeric(),
        Life_Years_Lost = numeric(),
        Cancer_Deaths = numeric(),
        # Demographics
        Male_Percent = numeric(),
        Female_Percent = numeric(),
        White_Percent = numeric(),
        Black_Percent = numeric(),
        Other_Race_Percent = numeric(),
        Age_45_49_Percent = numeric(),
        Age_50_54_Percent = numeric(),
        Age_55_59_Percent = numeric(),
        Age_60_64_Percent = numeric(),
        Age_65_69_Percent = numeric(),
        Age_70_74_Percent = numeric(),
        # Screening Parameters
        FIT_Before = numeric(),
        FIT_During = numeric(),
        FIT_After = numeric(),
        Colonoscopy_Before = numeric(),
        Colonoscopy_During = numeric(),
        Colonoscopy_After = numeric(),
        Diagnostic_Before = numeric(),
        Diagnostic_During = numeric(),
        Diagnostic_After = numeric(),
        # Analysis metadata
        Analysis_Date = character(),
        Scenario_Type = character(),
        stringsAsFactors = FALSE
      )
      
      # Add baseline first
      if (!is.null(results[["baseline"]])) {
        baseline <- results[["baseline"]]
        comprehensive_data <- rbind(comprehensive_data, data.frame(
          Scenario_Name = baseline$scenario_name,
          Model_Type = baseline$model_type,
          Total_Population = baseline$total_pop,
          Cancer_Cases = baseline$cca,
          Life_Years_Lost = baseline$lyl,
          Cancer_Deaths = baseline$cd,
          # Demographics
          Male_Percent = baseline$parameters$male_pct,
          Female_Percent = baseline$parameters$female_pct,
          White_Percent = baseline$parameters$white_pct,
          Black_Percent = baseline$parameters$black_pct,
          Other_Race_Percent = baseline$parameters$other_pct,
          Age_45_49_Percent = baseline$parameters$age_45_49,
          Age_50_54_Percent = baseline$parameters$age_50_54,
          Age_55_59_Percent = baseline$parameters$age_55_59,
          Age_60_64_Percent = baseline$parameters$age_60_64,
          Age_65_69_Percent = baseline$parameters$age_65_69,
          Age_70_74_Percent = baseline$parameters$age_70_74,
          # Screening Parameters
          FIT_Before = baseline$parameters$fit_before,
          FIT_During = baseline$parameters$fit_during,
          FIT_After = baseline$parameters$fit_after,
          Colonoscopy_Before = baseline$parameters$colon_before,
          Colonoscopy_During = baseline$parameters$colon_during,
          Colonoscopy_After = baseline$parameters$colon_after,
          Diagnostic_Before = baseline$parameters$diag_before,
          Diagnostic_During = baseline$parameters$diag_during,
          Diagnostic_After = baseline$parameters$diag_after,
          # Analysis metadata
          Analysis_Date = as.character(Sys.Date()),
          Scenario_Type = "Baseline",
          stringsAsFactors = FALSE
        ))
      }
      
      # Add scenario results
      for (i in 1:input$num_scenarios) {
        scenario_key <- paste0("scenario_", i)
        if (!is.null(results[[scenario_key]])) {
          scenario <- results[[scenario_key]]
          scenario_display_name <- if (!is.null(scenario$scenario_name)) {
            scenario$scenario_name
          } else {
            get_scenario_name(i)
          }
          
          comprehensive_data <- rbind(comprehensive_data, data.frame(
            Scenario_Name = scenario_display_name,
            Model_Type = scenario$model_type,
            Total_Population = scenario$total_pop,
            Cancer_Cases = scenario$cca,
            Life_Years_Lost = scenario$lyl,
            Cancer_Deaths = scenario$cd,
            # Demographics
            Male_Percent = scenario$parameters$male_pct,
            Female_Percent = scenario$parameters$female_pct,
            White_Percent = scenario$parameters$white_pct,
            Black_Percent = scenario$parameters$black_pct,
            Other_Race_Percent = scenario$parameters$other_pct,
            Age_45_49_Percent = scenario$parameters$age_45_49,
            Age_50_54_Percent = scenario$parameters$age_50_54,
            Age_55_59_Percent = scenario$parameters$age_55_59,
            Age_60_64_Percent = scenario$parameters$age_60_64,
            Age_65_69_Percent = scenario$parameters$age_65_69,
            Age_70_74_Percent = scenario$parameters$age_70_74,
            # Screening Parameters
            FIT_Before = scenario$parameters$fit_before,
            FIT_During = scenario$parameters$fit_during,
            FIT_After = scenario$parameters$fit_after,
            Colonoscopy_Before = scenario$parameters$colon_before,
            Colonoscopy_During = scenario$parameters$colon_during,
            Colonoscopy_After = scenario$parameters$colon_after,
            Diagnostic_Before = scenario$parameters$diag_before,
            Diagnostic_During = scenario$parameters$diag_during,
            Diagnostic_After = scenario$parameters$diag_after,
            # Analysis metadata
            Analysis_Date = as.character(Sys.Date()),
            Scenario_Type = paste("Comparison_Scenario", i),
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Add summary statistics at the end
      if (nrow(comprehensive_data) > 1) {
        # Calculate differences from baseline
        baseline_row <- comprehensive_data[comprehensive_data$Scenario_Type == "Baseline", ]
        if (nrow(baseline_row) > 0) {
          # Add empty row for separation
          empty_row <- comprehensive_data[1, ]
          empty_row[1, ] <- NA
          empty_row$Scenario_Name <- "--- SUMMARY STATISTICS ---"
          comprehensive_data <- rbind(comprehensive_data, empty_row)
          
          # Add differences from baseline for each comparison scenario
          comparison_scenarios <- comprehensive_data[comprehensive_data$Scenario_Type != "Baseline" &
                                                       !is.na(comprehensive_data$Cancer_Cases), ]
          
          for (i in 1:nrow(comparison_scenarios)) {
            diff_row <- comparison_scenarios[i, ]
            diff_row$Scenario_Name <- paste("DIFFERENCE:", comparison_scenarios[i, ]$Scenario_Name, "vs Baseline")
            diff_row$Cancer_Cases <- comparison_scenarios[i, ]$Cancer_Cases - baseline_row$Cancer_Cases
            diff_row$Life_Years_Lost <- comparison_scenarios[i, ]$Life_Years_Lost - baseline_row$Life_Years_Lost
            diff_row$Cancer_Deaths <- comparison_scenarios[i, ]$Cancer_Deaths - baseline_row$Cancer_Deaths
            diff_row$Scenario_Type <- "Difference_Analysis"
            
            # Clear demographic and parameter columns for difference rows
            demo_params_cols <- c("Male_Percent", "Female_Percent", "White_Percent", "Black_Percent",
                                  "Other_Race_Percent", "Age_45_49_Percent", "Age_50_54_Percent",
                                  "Age_55_59_Percent", "Age_60_64_Percent", "Age_65_69_Percent",
                                  "Age_70_74_Percent", "FIT_Before", "FIT_During",
                                  "FIT_After", "Colonoscopy_Before", "Colonoscopy_During",
                                  "Colonoscopy_After", "Diagnostic_Before", "Diagnostic_During",
                                  "Diagnostic_After")
            diff_row[demo_params_cols] <- NA
            
            comprehensive_data <- rbind(comprehensive_data, diff_row)
          }
        }
      }
      
      write.csv(comprehensive_data, file, row.names = FALSE, na = "")
    }
  )
  
  # =======================================================================================
  # MULTI-POPULATION RESULTS TABLES 
  # =======================================================================================
  
  # Individual results table - enhanced to show all sub-population results
  output$individual_results_table <- renderDT({
    subpop_defs <- subpop_definitions()
    if (nrow(subpop_defs) == 0) {
      return(data.frame(Message = "No sub-populations defined yet. Please define sub-populations first."))
    }
    
    # Aggregate results from all sub-populations
    all_results <- data.frame(
      SubPopulation = character(),
      Scenario_Name = character(),
      Model_Type = character(),
      Population_Size = numeric(),
      Cancer_Cases = numeric(),
      Life_Years_Lost = numeric(),
      Cancer_Deaths = numeric(),
      Analysis_Type = character(),
      stringsAsFactors = FALSE
    )
    
    has_results <- FALSE
    
    for (i in 1:nrow(subpop_defs)) {
      subpop_info <- subpop_defs[i, ]
      subpop_id <- subpop_info$id
      subpop_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
      
      if (!is.null(subpop_results) && length(subpop_results) > 0) {
        has_results <- TRUE
        
        # Add baseline
        if (!is.null(subpop_results[["baseline"]])) {
          baseline <- subpop_results[["baseline"]]
          all_results <- rbind(all_results, data.frame(
            SubPopulation = subpop_info$name,
            Scenario_Name = baseline$scenario_name,
            Model_Type = baseline$model_type,
            Population_Size = subpop_info$population,
            Cancer_Cases = baseline$cca,
            Life_Years_Lost = baseline$lyl,
            Cancer_Deaths = baseline$cd,
            Analysis_Type = "Baseline",
            stringsAsFactors = FALSE
          ))
        }
        
        # Add scenarios
        scenario_keys <- names(subpop_results)[grepl("^scenario_", names(subpop_results))]
        for (scenario_key in scenario_keys) {
          scenario <- subpop_results[[scenario_key]]
          all_results <- rbind(all_results, data.frame(
            SubPopulation = subpop_info$name,
            Scenario_Name = scenario$scenario_name,
            Model_Type = scenario$model_type,
            Population_Size = subpop_info$population,
            Cancer_Cases = scenario$cca,
            Life_Years_Lost = scenario$lyl,
            Cancer_Deaths = scenario$cd,
            Analysis_Type = "Scenario",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    if (!has_results) {
      return(data.frame(Message = "No results available yet. Run analysis for sub-populations first."))
    }
    
    # Format numbers with commas
    all_results$Population_Size <- format(all_results$Population_Size, big.mark = ",")
    all_results$Cancer_Cases <- format(all_results$Cancer_Cases, big.mark = ",")
    all_results$Life_Years_Lost <- format(all_results$Life_Years_Lost, big.mark = ",")
    all_results$Cancer_Deaths <- format(all_results$Cancer_Deaths, big.mark = ",")
    
    datatable(all_results,
              options = list(
                dom = 'Bfrtip',
                scrollX = TRUE,
                pageLength = 25,
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE) %>%
      formatStyle(columns = c(1:ncol(all_results)),
                  backgroundColor = 'rgba(0,123,255,0.05)',
                  border = '1px solid #ddd') %>%
      formatStyle(
        "Analysis_Type",
        target = "row",
        backgroundColor = styleEqual("Baseline", "rgba(255,193,7,0.2)")
      )
  })
  
  # Cross-comparison table - enhanced functionality
  output$cross_comparison_table <- renderDT({
    subpop_defs <- subpop_definitions()
    if (nrow(subpop_defs) == 0) {
      return(data.frame(Message = "No sub-populations defined yet."))
    }
    
    # Create cross-comparison matrix
    comparison_data <- data.frame()
    
    for (i in 1:nrow(subpop_defs)) {
      subpop_info <- subpop_defs[i, ]
      subpop_id <- subpop_info$id
      subpop_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
      
      if (!is.null(subpop_results) && !is.null(subpop_results[["baseline"]])) {
        baseline <- subpop_results[["baseline"]]
        
        # Calculate rates per 100,000 population for comparison
        cancer_rate <- round((baseline$cca / subpop_info$population) * 100000, 1)
        lyl_rate <- round((baseline$lyl / subpop_info$population) * 100000, 1)
        death_rate <- round((baseline$cd / subpop_info$population) * 100000, 1)
        
        comparison_data <- rbind(comparison_data, data.frame(
          SubPopulation = subpop_info$name,
          Population_Size = format(subpop_info$population, big.mark = ","),
          Model_Used = baseline$model_type,
          Cancer_Cases_Total = format(baseline$cca, big.mark = ","),
          Cancer_Rate_Per_100k = cancer_rate,
          Life_Years_Lost_Total = format(baseline$lyl, big.mark = ","),
          LYL_Rate_Per_100k = lyl_rate,
          Cancer_Deaths_Total = format(baseline$cd, big.mark = ","),
          Death_Rate_Per_100k = death_rate,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if (nrow(comparison_data) == 0) {
      return(data.frame(Message = "No baseline results available for cross-comparison."))
    }
    
    datatable(comparison_data,
              options = list(
                dom = 'Bfrtip',
                scrollX = TRUE,
                pageLength = -1,
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              caption = "Cross-Population Baseline Comparison (Rates per 100,000 population)") %>%
      formatStyle(columns = c(1:ncol(comparison_data)),
                  backgroundColor = 'rgba(0,123,255,0.05)',
                  border = '1px solid #ddd')
  })
  
  # Aggregated analysis table
  output$aggregated_results_table <- renderDT({
    subpop_defs <- subpop_definitions()
    if (nrow(subpop_defs) == 0) {
      return(data.frame(Message = "No sub-populations defined yet."))
    }
    
    # Calculate aggregated totals
    total_population <- 0
    total_cancer_cases <- 0
    total_lyl <- 0
    total_deaths <- 0
    models_used <- c()
    has_data <- FALSE
    
    for (i in 1:nrow(subpop_defs)) {
      subpop_info <- subpop_defs[i, ]
      subpop_id <- subpop_info$id
      subpop_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
      
      if (!is.null(subpop_results) && !is.null(subpop_results[["baseline"]])) {
        has_data <- TRUE
        baseline <- subpop_results[["baseline"]]
        
        total_population <- total_population + subpop_info$population
        total_cancer_cases <- total_cancer_cases + baseline$cca
        total_lyl <- total_lyl + baseline$lyl
        total_deaths <- total_deaths + baseline$cd
        models_used <- c(models_used, baseline$model_type)
      }
    }
    
    if (!has_data) {
      return(data.frame(Message = "No baseline results available for aggregation."))
    }
    
    # Create aggregated summary
    aggregated_data <- data.frame(
      Metric = c(
        "Total Population",
        "Total Cancer Cases",
        "Total Life Years Lost", 
        "Total Cancer Deaths",
        "Cancer Rate per 100,000",
        "Life Years Lost Rate per 100,000",
        "Death Rate per 100,000",
        "Models Used"
      ),
      Value = c(
        format(total_population, big.mark = ","),
        format(round(total_cancer_cases), big.mark = ","),
        format(round(total_lyl), big.mark = ","),
        format(round(total_deaths), big.mark = ","),
        format(round((total_cancer_cases / total_population) * 100000, 1), big.mark = ","),
        format(round((total_lyl / total_population) * 100000, 1), big.mark = ","),
        format(round((total_deaths / total_population) * 100000, 1), big.mark = ","),
        paste(unique(models_used), collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(aggregated_data,
              options = list(
                dom = 't',
                pageLength = -1
              ),
              rownames = FALSE,
              caption = "Aggregated Multi-Population Analysis Results") %>%
      formatStyle(columns = c(1:2),
                  backgroundColor = 'rgba(0,123,255,0.05)',
                  border = '1px solid #ddd')
  })
  
  # Summary statistics table
  output$summary_stats_table <- renderDT({
    subpop_defs <- subpop_definitions()
    if (nrow(subpop_defs) == 0) {
      return(data.frame(Message = "No sub-populations defined yet."))
    }
    
    # Calculate summary statistics across sub-populations
    cancer_rates <- c()
    lyl_rates <- c()
    death_rates <- c()
    subpop_names <- c()
    
    for (i in 1:nrow(subpop_defs)) {
      subpop_info <- subpop_defs[i, ]
      subpop_id <- subpop_info$id
      subpop_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
      
      if (!is.null(subpop_results) && !is.null(subpop_results[["baseline"]])) {
        baseline <- subpop_results[["baseline"]]
        
        cancer_rates <- c(cancer_rates, (baseline$cca / subpop_info$population) * 100000)
        lyl_rates <- c(lyl_rates, (baseline$lyl / subpop_info$population) * 100000)
        death_rates <- c(death_rates, (baseline$cd / subpop_info$population) * 100000)
        subpop_names <- c(subpop_names, subpop_info$name)
      }
    }
    
    if (length(cancer_rates) == 0) {
      return(data.frame(Message = "No baseline results available for summary statistics."))
    }
    
    # Calculate statistics
    summary_data <- data.frame(
      Statistic = c(
        "Number of Sub-Populations",
        "Mean Cancer Rate per 100k",
        "Median Cancer Rate per 100k",
        "Min Cancer Rate per 100k",
        "Max Cancer Rate per 100k",
        "Mean Life Years Lost Rate per 100k",
        "Median Life Years Lost Rate per 100k", 
        "Mean Death Rate per 100k",
        "Median Death Rate per 100k",
        "Sub-Population with Highest Cancer Rate",
        "Sub-Population with Lowest Cancer Rate"
      ),
      Value = c(
        length(subpop_names),
        round(mean(cancer_rates), 1),
        round(median(cancer_rates), 1),
        round(min(cancer_rates), 1),
        round(max(cancer_rates), 1),
        round(mean(lyl_rates), 1),
        round(median(lyl_rates), 1),
        round(mean(death_rates), 1),
        round(median(death_rates), 1),
        subpop_names[which.max(cancer_rates)],
        subpop_names[which.min(cancer_rates)]
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(summary_data,
              options = list(
                dom = 't',
                pageLength = -1
              ),
              rownames = FALSE,
              caption = "Summary Statistics Across Sub-Populations") %>%
      formatStyle(columns = c(1:2),
                  backgroundColor = 'rgba(0,123,255,0.05)',
                  border = '1px solid #ddd')
  })
  
  # Multi-population comprehensive download handler
  output$download_multi_analysis <- downloadHandler(
    filename = function() paste0("multi_population_comprehensive_analysis_", Sys.Date(), ".csv"),
    content = function(file) {
      subpop_defs <- subpop_definitions()
      if (nrow(subpop_defs) == 0) {
        write.csv(data.frame(Message = "No sub-populations defined."), file, row.names = FALSE)
        return()
      }
      
      # Comprehensive export including all sub-population data
      comprehensive_data <- data.frame(
        Sub_Population = character(),
        Scenario_Name = character(),
        Model_Type = character(),
        Population_Size = numeric(),
        Cancer_Cases = numeric(),
        Life_Years_Lost = numeric(),
        Cancer_Deaths = numeric(),
        Cancer_Rate_Per_100k = numeric(),
        LYL_Rate_Per_100k = numeric(),
        Death_Rate_Per_100k = numeric(),
        # Demographics
        Male_Percent = numeric(),
        Female_Percent = numeric(),
        White_Percent = numeric(),
        Black_Percent = numeric(),
        Other_Race_Percent = numeric(),
        Age_45_49_Percent = numeric(),
        Age_50_54_Percent = numeric(),
        Age_55_59_Percent = numeric(),
        Age_60_64_Percent = numeric(),
        Age_65_69_Percent = numeric(),
        Age_70_74_Percent = numeric(),
        # Screening Parameters
        FIT_Before = numeric(),
        FIT_During = numeric(),
        FIT_After = numeric(),
        Colonoscopy_Before = numeric(),
        Colonoscopy_During = numeric(),
        Colonoscopy_After = numeric(),
        Diagnostic_Before = numeric(),
        Diagnostic_During = numeric(),
        Diagnostic_After = numeric(),
        # Analysis metadata
        Analysis_Date = character(),
        Analysis_Type = character(),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:nrow(subpop_defs)) {
        subpop_info <- subpop_defs[i, ]
        subpop_id <- subpop_info$id
        subpop_results <- subpop_states[[paste0(subpop_id, "_scenario_results")]]
        
        if (!is.null(subpop_results) && length(subpop_results) > 0) {
          # Add baseline
          if (!is.null(subpop_results[["baseline"]])) {
            baseline <- subpop_results[["baseline"]]
            
            comprehensive_data <- rbind(comprehensive_data, data.frame(
              Sub_Population = subpop_info$name,
              Scenario_Name = baseline$scenario_name,
              Model_Type = baseline$model_type,
              Population_Size = subpop_info$population,
              Cancer_Cases = baseline$cca,
              Life_Years_Lost = baseline$lyl,
              Cancer_Deaths = baseline$cd,
              Cancer_Rate_Per_100k = round((baseline$cca / subpop_info$population) * 100000, 1),
              LYL_Rate_Per_100k = round((baseline$lyl / subpop_info$population) * 100000, 1),
              Death_Rate_Per_100k = round((baseline$cd / subpop_info$population) * 100000, 1),
              # Demographics
              Male_Percent = baseline$parameters$male_pct,
              Female_Percent = baseline$parameters$female_pct,
              White_Percent = baseline$parameters$white_pct,
              Black_Percent = baseline$parameters$black_pct,
              Other_Race_Percent = baseline$parameters$other_pct,
              Age_45_49_Percent = baseline$parameters$age_45_49,
              Age_50_54_Percent = baseline$parameters$age_50_54,
              Age_55_59_Percent = baseline$parameters$age_55_59,
              Age_60_64_Percent = baseline$parameters$age_60_64,
              Age_65_69_Percent = baseline$parameters$age_65_69,
              Age_70_74_Percent = baseline$parameters$age_70_74,
              # Screening Parameters
              FIT_Before = baseline$parameters$fit_before,
              FIT_During = baseline$parameters$fit_during,
              FIT_After = baseline$parameters$fit_after,
              Colonoscopy_Before = baseline$parameters$colon_before,
              Colonoscopy_During = baseline$parameters$colon_during,
              Colonoscopy_After = baseline$parameters$colon_after,
              Diagnostic_Before = baseline$parameters$diag_before,
              Diagnostic_During = baseline$parameters$diag_during,
              Diagnostic_After = baseline$parameters$diag_after,
              # Analysis metadata
              Analysis_Date = as.character(Sys.Date()),
              Analysis_Type = "Baseline",
              stringsAsFactors = FALSE
            ))
          }
          
          # Add scenarios
          scenario_keys <- names(subpop_results)[grepl("^scenario_", names(subpop_results))]
          for (scenario_key in scenario_keys) {
            scenario <- subpop_results[[scenario_key]]
            
            comprehensive_data <- rbind(comprehensive_data, data.frame(
              Sub_Population = subpop_info$name,
              Scenario_Name = scenario$scenario_name,
              Model_Type = scenario$model_type,
              Population_Size = subpop_info$population,
              Cancer_Cases = scenario$cca,
              Life_Years_Lost = scenario$lyl,
              Cancer_Deaths = scenario$cd,
              Cancer_Rate_Per_100k = round((scenario$cca / subpop_info$population) * 100000, 1),
              LYL_Rate_Per_100k = round((scenario$lyl / subpop_info$population) * 100000, 1),
              Death_Rate_Per_100k = round((scenario$cd / subpop_info$population) * 100000, 1),
              # Demographics
              Male_Percent = scenario$parameters$male_pct,
              Female_Percent = scenario$parameters$female_pct,
              White_Percent = scenario$parameters$white_pct,
              Black_Percent = scenario$parameters$black_pct,
              Other_Race_Percent = scenario$parameters$other_pct,
              Age_45_49_Percent = scenario$parameters$age_45_49,
              Age_50_54_Percent = scenario$parameters$age_50_54,
              Age_55_59_Percent = scenario$parameters$age_55_59,
              Age_60_64_Percent = scenario$parameters$age_60_64,
              Age_65_69_Percent = scenario$parameters$age_65_69,
              Age_70_74_Percent = scenario$parameters$age_70_74,
              # Screening Parameters
              FIT_Before = scenario$parameters$fit_before,
              FIT_During = scenario$parameters$fit_during,
              FIT_After = scenario$parameters$fit_after,
              Colonoscopy_Before = scenario$parameters$colon_before,
              Colonoscopy_During = scenario$parameters$colon_during,
              Colonoscopy_After = scenario$parameters$colon_after,
              Diagnostic_Before = scenario$parameters$diag_before,
              Diagnostic_During = scenario$parameters$diag_during,
              Diagnostic_After = scenario$parameters$diag_after,
              # Analysis metadata
              Analysis_Date = as.character(Sys.Date()),
              Analysis_Type = "Scenario_Comparison",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      if (nrow(comprehensive_data) == 0) {
        comprehensive_data <- data.frame(Message = "No analysis results available for download.")
      }
      
      write.csv(comprehensive_data, file, row.names = FALSE, na = "")
    }
  )
}

# Run the application
shinyApp(ui, server)
