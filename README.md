# Metamodel Decision Tool for Colorectal Cancer Health Outcomes

A sophisticated Shiny application for analyzing and comparing colorectal cancer screening scenarios using machine learning metamodels. This tool enables healthcare researchers and policymakers to evaluate the impact of different screening strategies on cancer cases, life years lost, and cancer deaths across diverse populations.

## 🎯 Overview

The Metamodel Decision Tool provides:
- **Single and Multi-Population Analysis**: Analyze outcomes for a single population or compare multiple sub-populations simultaneously
- **Multiple ML Models**: Choose from 6 different machine learning approaches (Linear Regression, Decision Tree, Random Forest, SVR, Lasso, Ridge)
- **Scenario Comparison**: Compare up to 5 different screening scenarios side-by-side
- **Demographic Weighting**: Account for population demographics (gender, race, age groups)
- **Comprehensive Outputs**: Export detailed analyses with all parameters and results

## ✨ Key Features

### Analysis Modes
- **Single Population Analysis**: Focused analysis for a single target population
- **Multi-Population Comparative Analysis**: Compare outcomes across multiple sub-populations with different characteristics

### Machine Learning Models
- Linear Regression (fastest, ~30 seconds - 1 minute)
- Decision Tree (~1-2 minutes)
- Random Forest (~2-3 minutes)
- Support Vector Regression (~2-4 minutes)
- Lasso Regression (~1-2 minutes)
- Ridge Regression (~1-2 minutes)

### Screening Parameters
Configure three types of screening interventions:
- **FIT (Fecal Immunochemical Test)**: Range 0-30%
- **Colonoscopy**: Range 30-70%
- **Diagnostic Screening**: Range 0-90%

Each with Before/During/After time periods.

### Demographics
Customizable population demographics:
- **Gender**: Male/Female distribution
- **Race**: White/Black/Other distribution
- **Age Groups**: 45-49, 50-54, 55-59, 60-64, 65-69, 70-74

### Health Outcomes
The tool predicts three key health outcomes:
- **Cancer Cases (CCA)**: Total number of colorectal cancer cases
- **Life Years Lost (LYL)**: Total life years lost due to colorectal cancer
- **Cancer Deaths (CD)**: Total number of cancer-related deaths

## 📋 Prerequisites

### System Requirements
- R (version 4.0.0 or higher)
- RStudio (recommended)
- Minimum 8GB RAM (16GB recommended for larger populations)
- 2GB free disk space

### Required R Packages
```r
install.packages(c(
  "shiny",
  "readr",
  "dplyr",
  "tidyr",
  "purrr",
  "caret",
  "glmnet",
  "DT",
  "shinyBS",
  "digest",
  "randomForest",
  "e1071",
  "rpart",
  "kernlab"
))
```

### Required Data Files
The application requires three CSV training data files:
1. `edited_INFORMScancer_after_averages_combined.csv` - Cancer cases training data
2. `edited_INFORMSlife_years_averages_combined.csv` - Life years lost training data
3. `edited_INFORMScancer_death_averages_combined.csv` - Cancer deaths training data

### Pre-computed Model Files (Optional but Recommended)
For faster performance, pre-computed model files should be placed in the application directory:
- `models_linear_regression.rds`
- `models_decision_tree.rds`
- `models_random_forest.rds`
- `models_support_vector_regression.rds`
- `models_lasso_regression.rds`
- `models_ridge_regression.rds`

## 🚀 Installation

### Method 1: Clone Repository
```bash
git clone https://github.com/yourusername/colorectal-cancer-metamodel.git
cd colorectal-cancer-metamodel
```

### Method 2: Download ZIP
1. Download the repository as a ZIP file
2. Extract to your desired location
3. Navigate to the directory

### Setup Steps
1. Ensure all required R packages are installed
2. Place the three required CSV data files in the application directory
3. (Optional) Place pre-computed model files in the application directory for faster performance
4. Open `app.R` in RStudio

## 🎮 Quick Start

### Running the Application

#### In RStudio:
```r
# Set working directory to application folder
setwd("path/to/colorectal-cancer-metamodel")

# Run the application
shiny::runApp()
```

#### From Command Line:
```r
R -e "shiny::runApp('path/to/app.R')"
```

### Basic Workflow

#### Single Population Analysis:
1. **Configure Global Settings**
   - Set total population size
   - Select machine learning model type
   - Set baseline screening parameters (FIT, COLON, DIAG before values)
   - Configure demographics (optional but recommended)

2. **Set Up Scenarios**
   - Choose number of scenarios (1-5)
   - Name your scenarios
   - Configure During/After parameters for each scenario

3. **Run Analysis**
   - Click "Run Prediction" for each scenario
   - Wait for model loading and prediction (time varies by model)
   - Review results in the comparison table

4. **Export Results**
   - Click "Download Comprehensive Analysis" to export all results

#### Multi-Population Analysis:
1. **Define Sub-Populations**
   - Set total population
   - Choose number of sub-populations (2-10)
   - Define each sub-population by percentage or absolute numbers
   - Validate sub-populations

2. **Configure Each Sub-Population**
   - Expand each sub-population panel
   - Set specific parameters and demographics
   - Configure scenarios for each sub-population
   - Run predictions

3. **Review Comprehensive Results**
   - Individual Results: View all sub-population results
   - Cross-Population Comparison: Compare baseline rates
   - Aggregated Analysis: See overall totals
   - Summary Statistics: Review statistical summaries

4. **Export Multi-Population Analysis**
   - Download complete multi-population analysis with all details

## 📊 Example Use Case

### Scenario: Evaluating Enhanced Screening in Urban vs Rural Populations

**Objective**: Compare the impact of enhanced FIT screening across urban and rural populations.

**Setup**:
- Total Population: 500,000
- Sub-Population 1 (Urban): 350,000 (70%)
- Sub-Population 2 (Rural): 150,000 (30%)

**Baseline (Both Populations)**:
- FIT: 8%, Colonoscopy: 48%, Diagnostic: 7%

**Enhanced Screening Scenario**:
- FIT During: 15%, FIT After: 12%
- Colonoscopy During: 55%, Colonoscopy After: 52%
- Diagnostic During: 12%, Diagnostic After: 10%

**Expected Outputs**:
- Baseline cancer cases, life years lost, and deaths for each population
- Impact of enhanced screening on each population
- Per-capita rates for cross-population comparison
- Aggregated totals across both populations

## 📁 Project Structure

```
colorectal-cancer-metamodel/
├── app.R                                          # Main Shiny application
├── README.md                                      # This file
├── technical_documentation.md                     # Detailed technical documentation
├── user_guide.md                                  # Comprehensive user guide
├── edited_INFORMScancer_after_averages_combined.csv
├── edited_INFORMSlife_years_averages_combined.csv
├── edited_INFORMScancer_death_averages_combined.csv
├── models_linear_regression.rds                   # (Optional)
├── models_decision_tree.rds                       # (Optional)
├── models_random_forest.rds                       # (Optional)
├── models_support_vector_regression.rds           # (Optional)
├── models_lasso_regression.rds                    # (Optional)
└── models_ridge_regression.rds                    # (Optional)
```

## 🔧 Troubleshooting

### Common Issues

**Issue**: "File not found" error
- **Solution**: Ensure all three CSV data files are in the application directory

**Issue**: Models take too long to load
- **Solution**: Use pre-computed model files (.rds) for faster loading

**Issue**: Application crashes with large populations
- **Solution**: Increase available RAM or reduce population size

**Issue**: Validation errors for demographics
- **Solution**: Ensure all demographic percentages sum to exactly 100%

**Issue**: Parameter validation errors
- **Solution**: Verify Before < After < During relationships for all screening parameters


## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 📧 Contact

For questions, issues, or inquiries:
- Email: srikarsai.puppala@gmail.com

## 🙏 Acknowledgments

- Based on INFORMS cancer screening research
- Built with R Shiny framework
- Machine learning models powered by caret, randomForest, e1071, and glmnet packages

## 📚 Citation

If you use this tool in your research, please cite:

```
Sai Srikar Puppala, North Carolina State University. (2025). Metamodel Decision Tool for Colorectal Cancer Health Outcomes. 
GitHub repository: https://github.com/ashleystanfield/ColorectalCancerMetamodels
```

## 🔄 Version History

### Version 1.0.0 (Current)
- Single and multi-population analysis modes
- Six machine learning model options
- Comprehensive demographic weighting
- Enhanced validation and error handling
- Detailed export capabilities
- Pre-computed model support

## 🚦 Status

![Status](https://img.shields.io/badge/status-active-success.svg)
![R Version](https://img.shields.io/badge/R-4.0%2B-blue.svg)
![License](https://img.shields.io/badge/license-MIT-blue.svg)

---

**Note**: This tool is designed for research and policy analysis purposes. Clinical decisions should involve consultation with healthcare professionals and consideration of individual patient circumstances.
