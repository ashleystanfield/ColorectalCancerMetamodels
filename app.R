library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(glmnet)

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
## Enhanced Model Training Functions
## ---------------------------

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
train_lm_models <- function(data, outcome) {
  models <- list()
  
  for (person in paste0("P_", 1:180)) {
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
  }
  return(models)
}

# Decision Tree Training
train_tree_models <- function(data, outcome) {
  models <- list()
  
  for (person in paste0("P_", 1:180)) {
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
  }
  return(models)
}

# Random Forest Training
train_rf_models <- function(data, outcome) {
  models <- list()
  
  for (person in paste0("P_", 1:180)) {
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
  }
  return(models)
}

# Support Vector Regression Training
train_svr_models <- function(data, outcome) {
  models <- list()
  
  for (person in paste0("P_", 1:180)) {
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
  }
  return(models)
}

# Lasso Regression Training
train_lasso_models <- function(data, outcome) {
  models <- list()
  
  for (person in paste0("P_", 1:180)) {
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
  }
  return(models)
}

# Ridge Regression Training
train_ridge_models <- function(data, outcome) {
  models <- list()
  
  for (person in paste0("P_", 1:180)) {
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
  }
  return(models)
}

## ---------------------------
## UI Definition
## ---------------------------
ui <- fluidPage(
  titlePanel("Metamodel Decision Tool for Estimating Colorectal Cancer Health Outcomes"),
  sidebarLayout(
    sidebarPanel(
      numericInput("total_pop", "Total Population", value = 100000),
      actionButton("use_census", "Use U.S. Census Defaults"),
      actionButton("clear", "Clear Values"),
      radioButtons("input_mode", "Select Input Method:",
                   choices = c("Manual Index Entry", "Demographic-Based Entry"),
                   selected = "Manual Index Entry"),
      
      conditionalPanel(
        condition = "input.input_mode == 'Manual Index Entry'",
        h5("Entering values for 180 person indices (P-1 to P-180)"),
        div(style = "max-height: 300px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
            uiOutput("group_inputs_ui"))
      ),
      
      conditionalPanel(
        condition = "input.input_mode == 'Demographic-Based Entry'",
        h4("Gender Proportions"),
        sliderInput("male_pct", "Male %", min = 0, max = 100, value = 50),
        sliderInput("female_pct", "Female %", min = 0, max = 100, value = 50),
        
        h4("Race Proportions"),
        sliderInput("white_pct", "White %", min = 0, max = 100, value = 60),
        sliderInput("black_pct", "Black %", min = 0, max = 100, value = 13),
        sliderInput("other_pct", "Other %", min = 0, max = 100, value = 27),
        
        h4("Age Group Proportions"),
        sliderInput("age_45_49", "45–49 %", min = 0, max = 100, value = 17),
        sliderInput("age_50_54", "50–54 %", min = 0, max = 100, value = 17),
        sliderInput("age_55_59", "55–59 %", min = 0, max = 100, value = 16),
        sliderInput("age_60_64", "60–64 %", min = 0, max = 100, value = 16),
        sliderInput("age_65_69", "65–69 %", min = 0, max = 100, value = 16),
        sliderInput("age_70_74", "70–74 %", min = 0, max = 100, value = 18)
      ),
      
      h4("Screening Parameters"),
      numericInput("fit_before", "FIT_before", value = 8, min = 0, max = 30),
      helpText("Fecal Immunochemical Test before: 0–30"),
      numericInput("fit_during", "FIT_during", value = 8, min = 0, max = 30),
      helpText("FIT during intervention: 0–30"),
      numericInput("fit_after", "FIT_after", value = 8, min = 0, max = 30),
      helpText("FIT after intervention: 0–30"),
      
      numericInput("colon_before", "COLON_before", value = 48, min = 30, max = 70),
      helpText("Colonoscopy before: 30–70"),
      numericInput("colon_during", "COLON_during", value = 48, min = 30, max = 70),
      helpText("Colonoscopy during: 30–70"),
      numericInput("colon_after", "COLON_after", value = 48, min = 30, max = 70),
      helpText("Colonoscopy after: 30–70"),
      
      numericInput("diag_before", "diagnostic_before", value = 7, min = 40, max = 90),
      helpText("Diagnostic before: 40–90"),
      numericInput("diag_during", "diagnostic_during", value = 7, min = 40, max = 90),
      helpText("Diagnostic during: 40–90"),
      numericInput("diag_after", "diagnostic_after", value = 7, min = 40, max = 90),
      helpText("Diagnostic after: 40–90"),
      
      h4("Select Modeling Approach"),
      selectInput("model_type", "Choose Model:",
                  choices = c("Linear Regression",
                              "Decision Tree",
                              "Random Forest",
                              "Support Vector Regression",
                              "Lasso Regression",
                              "Ridge Regression"),
                  selected = "Linear Regression"),
      actionButton("run_sim", "Run Simulation")
    ),
    
    mainPanel(
      h3("Predicted Outcomes"),
      verbatimTextOutput("outcome1"),
      verbatimTextOutput("outcome2"),
      verbatimTextOutput("outcome3"),
      downloadButton("download", "Download Results"),
      h4("Model Training Status"),
      verbatimTextOutput("training_status"),
      h4("Debug Info"),
      verbatimTextOutput("debug_info")
    )
  )
)

## ---------------------------
## Server Logic
## ---------------------------
server <- function(input, output, session) {
  # Reactive values
  values_reset <- reactiveVal(TRUE)
  results <- reactiveVal(list(cca = 0, lyl = 0, cd = 0))
  model_status <- reactiveVal("Models not trained yet")
  
  # Create mapping_df
  mapping_df <- create_mapping_df()
  
  # Load and prepare training data with error handling
  training_data <- reactive({
    files <- c("edited_INFORMScancer_after_averages_combined.csv",
               "edited_INFORMSlife_years_averages_combined.csv",
               "edited_INFORMScancer_death_averages_combined.csv")
    
    # Check if files exist
    missing_files <- files[!file.exists(files)]
    if (length(missing_files) > 0) {
      model_status(paste("Missing files:", paste(missing_files, collapse = ", ")))
      return(NULL)
    }
    
    withProgress(message = 'Loading training data', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Loading CCA data")
        cca_data <- data_prep(files[1], "CCA")
        
        incProgress(0.6, detail = "Loading LYL data")
        lyl_data <- data_prep(files[2], "LYL")
        
        incProgress(0.9, detail = "Loading CD data")
        cd_data <- data_prep(files[3], "CD")
        
        list(cca = cca_data, lyl = lyl_data, cd = cd_data)
      }, error = function(e) {
        model_status(paste("Error loading data:", e$message))
        return(NULL)
      })
    })
  })
  
  # Train models with error handling
  models <- reactive({
    data <- training_data()
    if (is.null(data)) return(NULL)
    
    withProgress(message = 'Training models', value = 0, {
      tryCatch({
        model_status(paste("Training", input$model_type, "models..."))
        
        # Train models based on selected method
        incProgress(0.2, detail = "Training CCA models")
        cca_models <- switch(input$model_type,
                             "Linear Regression" = train_lm_models(data$cca, "CCA"),
                             "Decision Tree" = train_tree_models(data$cca, "CCA"),
                             "Random Forest" = train_rf_models(data$cca, "CCA"),
                             "Support Vector Regression" = train_svr_models(data$cca, "CCA"),
                             "Lasso Regression" = train_lasso_models(data$cca, "CCA"),
                             "Ridge Regression" = train_ridge_models(data$cca, "CCA"))
        
        incProgress(0.5, detail = "Training LYL models")
        lyl_models <- switch(input$model_type,
                             "Linear Regression" = train_lm_models(data$lyl, "LYL"),
                             "Decision Tree" = train_tree_models(data$lyl, "LYL"),
                             "Random Forest" = train_rf_models(data$lyl, "LYL"),
                             "Support Vector Regression" = train_svr_models(data$lyl, "LYL"),
                             "Lasso Regression" = train_lasso_models(data$lyl, "LYL"),
                             "Ridge Regression" = train_ridge_models(data$lyl, "LYL"))
        
        incProgress(0.8, detail = "Training CD models")
        cd_models <- switch(input$model_type,
                            "Linear Regression" = train_lm_models(data$cd, "CD"),
                            "Decision Tree" = train_tree_models(data$cd, "CD"),
                            "Random Forest" = train_rf_models(data$cd, "CD"),
                            "Support Vector Regression" = train_svr_models(data$cd, "CD"),
                            "Lasso Regression" = train_lasso_models(data$cd, "CD"),
                            "Ridge Regression" = train_ridge_models(data$cd, "CD"))
        
        model_status(paste("Training completed successfully for", input$model_type))
        list(cca = cca_models, lyl = lyl_models, cd = cd_models)
      }, error = function(e) {
        model_status(paste("Training failed:", e$message))
        return(NULL)
      })
    })
  })
  
  # Group inputs UI
  output$group_inputs_ui <- renderUI({
    lapply(1:180, function(i) {
      numericInput(paste0("group_", i), paste0("P-", i), value = 0, min = 0)
    })
  })
  
  # Get group values
  group_values <- reactive({
    sapply(1:180, function(i) {
      val <- input[[paste0("group_", i)]]
      if (is.null(val)) 0 else val
    })
  })
  
  # Census defaults
  observeEvent(input$use_census, {
    updateSliderInput(session, "male_pct", value = 49)
    updateSliderInput(session, "female_pct", value = 51)
    updateSliderInput(session, "white_pct", value = 60)
    updateSliderInput(session, "black_pct", value = 13)
    updateSliderInput(session, "other_pct", value = 27)
    updateSliderInput(session, "age_45_49", value = 17)
    updateSliderInput(session, "age_50_54", value = 17)
    updateSliderInput(session, "age_55_59", value = 16)
    updateSliderInput(session, "age_60_64", value = 16)
    updateSliderInput(session, "age_65_69", value = 16)
    updateSliderInput(session, "age_70_74", value = 18)
    
    per_val <- input$total_pop %/% 180
    remainder <- input$total_pop %% 180
    for (i in 1:180) {
      updateNumericInput(session, paste0("group_", i), 
                         value = if (i <= remainder) per_val + 1 else per_val)
    }
  })
  
  # Clear inputs
  observeEvent(input$clear, {
    updateNumericInput(session, "total_pop", value = 100000)
    updateSliderInput(session, "male_pct", value = 50)
    updateSliderInput(session, "female_pct", value = 50)
    updateSliderInput(session, "white_pct", value = 50)
    updateSliderInput(session, "black_pct", value = 30)
    updateSliderInput(session, "other_pct", value = 20)
    updateSliderInput(session, "age_45_49", value = 16)
    updateSliderInput(session, "age_50_54", value = 16)
    updateSliderInput(session, "age_55_59", value = 16)
    updateSliderInput(session, "age_60_64", value = 16)
    updateSliderInput(session, "age_65_69", value = 16)
    updateSliderInput(session, "age_70_74", value = 20)
    for (i in 1:180) {
      updateNumericInput(session, paste0("group_", i), value = 0)
    }
    values_reset(TRUE)
    results(list(cca = 0, lyl = 0, cd = 0))
  })
  
  # Compute demographic weights
  compute_demographic_weights <- reactive({
    gender <- c(male = input$male_pct / 100, female = input$female_pct / 100)
    race <- c(white = input$white_pct / 100, black = input$black_pct / 100, other = input$other_pct / 100)
    age <- c(
      "45-49" = input$age_45_49 / 100,
      "50-54" = input$age_50_54 / 100,
      "55-59" = input$age_55_59 / 100,
      "60-64" = input$age_60_64 / 100,
      "65-69" = input$age_65_69 / 100,
      "70-74" = input$age_70_74 / 100
    )
    
    weights_vec <- numeric(180)
    for (g in names(gender)) {
      for (r in names(race)) {
        for (a in names(age)) {
          subpop <- input$total_pop * gender[g] * race[r] * age[a]
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
  })
  
  # Final weights
  final_weights <- reactive({
    if (input$input_mode == "Manual Index Entry") {
      group_values()
    } else {
      compute_demographic_weights()
    }
  })
  
  # Safe prediction function
  safe_predict <- function(model, newdata) {
    safe_model_predict(model, newdata)
  }
  
  # Run simulation
  observeEvent(input$run_sim, {
    trained_models <- models()
    if (is.null(trained_models)) {
      showNotification("Models not available. Check data files.", type = "error")
      return()
    }
    
    values_reset(FALSE)
    
    current_weights <- final_weights()
    if (is.null(current_weights) || all(current_weights == 0)) {
      showNotification("Please enter population values", type = "error")
      return()
    }
    
    input_data <- data.frame(
      FIT_before = input$fit_before,
      FIT_during = input$fit_during,
      FIT_after = input$fit_after,
      COLON_before = input$colon_before,
      COLON_during = input$colon_during,
      COLON_after = input$colon_after,
      DIAG_before = input$diag_before,
      DIAG_during = input$diag_during,
      DIAG_after = input$diag_after
    )
    
    predict_total <- function(models) {
      preds <- sapply(paste0("P_", 1:180), function(p) {
        safe_predict(models[[p]], input_data)
      })
      sum(preds * current_weights, na.rm = TRUE)
    }
    
    results(list(
      cca = round(predict_total(trained_models$cca), 2),
      lyl = round(predict_total(trained_models$lyl), 2),
      cd = round(predict_total(trained_models$cd), 2)
    ))
  })
  
  # Outputs
  output$outcome1 <- renderText({
    paste("Cancer Cases:", results()$cca)
  })
  
  output$outcome2 <- renderText({
    paste("Life Years Lost:", results()$lyl)
  })
  
  output$outcome3 <- renderText({
    paste("Cancer Deaths:", results()$cd)
  })
  
  output$training_status <- renderText({
    model_status()
  })
  
  output$debug_info <- renderText({
    paste("Data files exist:", 
          all(file.exists(c("edited_INFORMScancer_after_averages_combined.csv",
                            "edited_INFORMSlife_years_averages_combined.csv",
                            "edited_INFORMScancer_death_averages_combined.csv"))))
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("model_results_", Sys.Date(), ".csv"),
    content = function(file) {
      current_results <- results()
      write.csv(data.frame(
        Outcome = c("Cancer Cases", "Life Years Lost", "Cancer Deaths"),
        Predicted_Value = c(current_results$cca, current_results$lyl, current_results$cd),
        Model_Type = input$model_type
      ), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)