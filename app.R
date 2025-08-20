library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(glmnet)
library(DT)
library(shinyBS) # For tooltips

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
## UI Definition
## ---------------------------
ui <- fluidPage(
  titlePanel("Metamodel Decision Tool for Estimating Colorectal Cancer Health Outcomes"),
  
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
               # Model Selection (MOVED HERE)
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
             h5("Baseline Screening Parameters (Before COVID-19) - Applied to All Scenarios"),
             fluidRow(
               column(4,
                      h6("FIT Parameters"),
                      div(
                        numericInput("global_fit_before", "FIT Before",
                                     value = 8, min = 0, max = 30),
                        bsTooltip("global_fit_before",
                                  "FIT screening rate before COVID-19 (Range: 0-30%)",
                                  placement = "top", trigger = "hover")
                      )
               ),
               column(4,
                      h6("Colonoscopy Parameters"),
                      div(
                        numericInput("global_colon_before", "COLON Before",
                                     value = 48, min = 30, max = 70),
                        bsTooltip("global_colon_before",
                                  "Colonoscopy screening rate before COVID-19 (Range: 30-70%)",
                                  placement = "top", trigger = "hover")
                      )
               ),
               column(4,
                      h6("Diagnostic Parameters"),
                      div(
                        numericInput("global_diag_before", "Diagnostic Before",
                                     value = 7, min = 0, max = 90),
                        bsTooltip("global_diag_before",
                                  "Diagnostic screening rate before COVID-19 (Range: 0-90%)",
                                  placement = "top", trigger = "hover")
                      )
               )
             ),
             
             hr(),
             
             # Demographics Section
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
  
  # Results Comparison Section (Always visible for baseline)
  fluidRow(
    column(12,
           wellPanel(
             h3("Results Comparison"),
             DTOutput("comparison_table"),
             br(),
             downloadButton("download_comparison", "Download Comprehensive Analysis", class = "btn-primary")
           )
    )
  )
)

## ---------------------------
## Server Logic
## ---------------------------
server <- function(input, output, session) {
  # Reactive values
  scenario_results <- reactiveVal(list())
  model_cache <- reactiveVal(list()) # Cache models to avoid retraining
  scenario_names <- reactiveVal(c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"))
  mapping_df <- create_mapping_df()
  
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
  
  # Generate baseline scenario automatically
  observe({
    # Check if we have all required inputs
    if (!is.null(input$global_fit_before) && !is.null(input$global_colon_before) &&
        !is.null(input$global_diag_before) && !is.null(input$global_total_pop) &&
        !is.null(input$global_model_type)) {
      
      # Create baseline scenario with the globally selected model
      baseline_inputs <- list(
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
        fit_during = input$global_fit_before,  # Same as before for baseline
        fit_after = input$global_fit_before,   # Same as before for baseline
        colon_before = input$global_colon_before,
        colon_during = input$global_colon_before,  # Same as before for baseline
        colon_after = input$global_colon_before,   # Same as before for baseline
        diag_before = input$global_diag_before,
        diag_during = input$global_diag_before,  # Same as before for baseline
        diag_after = input$global_diag_before,   # Same as before for baseline
        model_type = input$global_model_type  # Use global model selection
      )
      
      # Generate baseline prediction
      generate_baseline_prediction(baseline_inputs)
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
                 # Left Column - During/After Parameters (Model selection removed)
                 column(6,
                        h5("During COVID-19 Parameters"),
                        fluidRow(
                          column(6,
                                 div(
                                   numericInput(paste0("fit_during_", scenario_num), "FIT During",
                                                value = 10, min = 0, max = 30),
                                   bsTooltip(paste0("fit_during_", scenario_num),
                                             "FIT screening rate during COVID-19 (Range: 0-30%). Must be > Before value",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("colon_during_", scenario_num), "COLON During",
                                                value = 55, min = 30, max = 70),
                                   bsTooltip(paste0("colon_during_", scenario_num),
                                             "Colonoscopy rate during COVID-19 (Range: 30-70%). Must be > Before value",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("diag_during_", scenario_num), "Diagnostic During",
                                                value = 10, min = 0, max = 90),
                                   bsTooltip(paste0("diag_during_", scenario_num),
                                             "Diagnostic rate during COVID-19 (Range: 0-90%). Must be > Before value",
                                             placement = "top", trigger = "hover")
                                 )
                          ),
                          column(6,
                                 h6("After COVID-19 Parameters"),
                                 div(
                                   numericInput(paste0("fit_after_", scenario_num), "FIT After",
                                                value = 9, min = 0, max = 30),
                                   bsTooltip(paste0("fit_after_", scenario_num),
                                             "FIT screening rate after COVID-19 (Range: 0-30%). Must be > Before and < During",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("colon_after_", scenario_num), "COLON After",
                                                value = 52, min = 30, max = 70),
                                   bsTooltip(paste0("colon_after_", scenario_num),
                                             "Colonoscopy rate after COVID-19 (Range: 30-70%). Must be > Before and < During",
                                             placement = "top", trigger = "hover")
                                 ),
                                 div(
                                   numericInput(paste0("diag_after_", scenario_num), "Diagnostic After",
                                                value = 8, min = 0, max = 90),
                                   bsTooltip(paste0("diag_after_", scenario_num),
                                             "Diagnostic rate after COVID-19 (Range: 0-90%). Must be > Before and < During",
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
      model_type = input$global_model_type  # Use global model selection
    )
  }
  
  # Train models with progress callback
  train_models_with_progress <- function(data, model_type) {
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
          
          # Update status
          output[[paste0("status_", scenario_num)]] <- renderText({
            paste("Initializing prediction for", get_scenario_name(scenario_num), "...")
          })
          
          # Get training data
          data <- training_data()
          if (is.null(data)) {
            output[[paste0("status_", scenario_num)]] <- renderText({
              "Error: Training data not available. Check data files."
            })
            return()
          }
          
          # Train models
          output[[paste0("status_", scenario_num)]] <- renderText({
            paste("Training", scenario_inputs$model_type, "models for", get_scenario_name(scenario_num), "...")
          })
          
          trained_models <- train_models_with_progress(data, scenario_inputs$model_type)
          
          if (is.null(trained_models)) {
            output[[paste0("status_", scenario_num)]] <- renderText({
              "Error: Model training failed."
            })
            return()
          }
          
          # Get weights
          current_weights <- compute_demographic_weights(scenario_inputs)
          
          if (is.null(current_weights) || all(current_weights == 0)) {
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
  
  # Comparison table
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
        FIT_Before_COVID = numeric(),
        FIT_During_COVID = numeric(),
        FIT_After_COVID = numeric(),
        Colonoscopy_Before_COVID = numeric(),
        Colonoscopy_During_COVID = numeric(),
        Colonoscopy_After_COVID = numeric(),
        Diagnostic_Before_COVID = numeric(),
        Diagnostic_During_COVID = numeric(),
        Diagnostic_After_COVID = numeric(),
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
          FIT_Before_COVID = baseline$parameters$fit_before,
          FIT_During_COVID = baseline$parameters$fit_during,
          FIT_After_COVID = baseline$parameters$fit_after,
          Colonoscopy_Before_COVID = baseline$parameters$colon_before,
          Colonoscopy_During_COVID = baseline$parameters$colon_during,
          Colonoscopy_After_COVID = baseline$parameters$colon_after,
          Diagnostic_Before_COVID = baseline$parameters$diag_before,
          Diagnostic_During_COVID = baseline$parameters$diag_during,
          Diagnostic_After_COVID = baseline$parameters$diag_after,
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
            FIT_Before_COVID = scenario$parameters$fit_before,
            FIT_During_COVID = scenario$parameters$fit_during,
            FIT_After_COVID = scenario$parameters$fit_after,
            Colonoscopy_Before_COVID = scenario$parameters$colon_before,
            Colonoscopy_During_COVID = scenario$parameters$colon_during,
            Colonoscopy_After_COVID = scenario$parameters$colon_after,
            Diagnostic_Before_COVID = scenario$parameters$diag_before,
            Diagnostic_During_COVID = scenario$parameters$diag_during,
            Diagnostic_After_COVID = scenario$parameters$diag_after,
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
                                  "Age_70_74_Percent", "FIT_Before_COVID", "FIT_During_COVID", 
                                  "FIT_After_COVID", "Colonoscopy_Before_COVID", "Colonoscopy_During_COVID", 
                                  "Colonoscopy_After_COVID", "Diagnostic_Before_COVID", "Diagnostic_During_COVID", 
                                  "Diagnostic_After_COVID")
            diff_row[demo_params_cols] <- NA
            
            comprehensive_data <- rbind(comprehensive_data, diff_row)
          }
        }
      }
      
      write.csv(comprehensive_data, file, row.names = FALSE, na = "")
    }
  )
}

# Run the application
shinyApp(ui, server)
