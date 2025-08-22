# Metamodel Decision Tool for Estimating Colorectal Cancer Health Outcomes

A comprehensive R Shiny application for analyzing the impact of different screening scenarios on colorectal cancer health outcomes using machine learning models.

## 📋 Table of Contents
- [Overview](#overview)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Data Requirements](#data-requirements)
- [Model Types](#model-types)
- [Output Analysis](#output-analysis)
- [File Structure](#file-structure)
- [Contributing](#contributing)
- [License](#license)

## 🎯 Overview

This tool enables healthcare researchers and policymakers to model the impact of colorectal cancer screening interventions by comparing different scenarios. The application uses metamodeling techniques to predict three key health outcomes:
- **Cancer Cases Avoided (CCA)**
- **Life Years Lost (LYL)**  
- **Cancer Deaths (CD)**

The tool is particularly designed to analyze the effects of COVID-19 disruptions on screening programs and evaluate recovery strategies.

## ✨ Features

### 🔧 Core Functionality
- **Automated Baseline Generation**: Automatically creates a "Usual Care" baseline scenario
- **Multi-Scenario Comparison**: Compare up to 5 intervention scenarios simultaneously
- **6 Machine Learning Models**: Choose from Linear Regression, Decision Tree, Random Forest, SVR, Lasso, and Ridge Regression
- **Demographic Modeling**: Comprehensive population demographics (gender, race, age groups)
- **Parameter Validation**: Real-time validation of screening parameter relationships

### 📊 Analysis Features
- **Interactive Results Table**: Real-time comparison of all scenarios
- **Comprehensive Downloads**: Full parameter documentation for reproducibility
- **Progress Tracking**: Visual progress indicators for model training
- **Model Caching**: Intelligent caching to avoid retraining identical models

### 🎨 User Experience
- **Intuitive Workflow**: Global configuration → Scenario setup → Analysis
- **Smart Defaults**: U.S. Census demographic defaults with one-click application
- **Tooltips & Validation**: Extensive help text and parameter validation
- **Responsive Design**: Works across different screen sizes

## 🚀 Installation

### Prerequisites
```r
# Required R packages
install.packages(c(
  "shiny",
  "readr", 
  "dplyr",
  "tidyr",
  "purrr",
  "caret",
  "glmnet",
  "DT",
  "shinyBS"
))
```

### Setup
1. **Clone the repository**
   ```bash
   git clone https://github.com/yourusername/colorectal-cancer-screening-tool.git
   cd colorectal-cancer-screening-tool
   ```

2. **Prepare your data files** (see [Data Requirements](#data-requirements))

3. **Run the application**
   ```r
   # In R console
   shiny::runApp("app.R")
   ```

## 📖 Usage

### Step 1: Global Configuration
1. **Set Population Size**: Enter total population for analysis (minimum 1,000)
2. **Choose Model Type**: Select machine learning model for all scenarios
3. **Configure Baseline Parameters**: Set pre-COVID screening rates
4. **Set Demographics**: Configure population demographics or use Census defaults

### Step 2: Scenario Setup
1. **Choose Number of Scenarios**: Select 1-5 comparison scenarios
2. **Name Your Scenarios**: Give meaningful names (e.g., "Enhanced Outreach", "Budget Option")
3. **Configure Each Scenario**: Set during-COVID and post-COVID screening parameters

### Step 3: Analysis
1. **Automatic Baseline**: Baseline scenario generates automatically
2. **Run Scenarios**: Click "Run Prediction" for each comparison scenario
3. **Review Results**: Compare outcomes in the results table
4. **Download Analysis**: Get comprehensive parameter documentation

### Example Workflow
```
Global Setup:
├── Population: 100,000
├── Model: Random Forest
├── FIT Before: 8%
├── Colonoscopy Before: 48%
└── Demographics: Census defaults

Scenario 1 - "Enhanced Recovery":
├── FIT During: 12% → After: 10%
├── Colonoscopy During: 60% → After: 55%
└── Results: Compare vs baseline
```

## 📁 Data Requirements

### Required Files
Place these CSV files in your app directory:

1. **`edited_INFORMScancer_after_averages_combined.csv`**
   - Cancer cases avoided data
   - 180 person-types (P_1 to P_180)
   - 9 screening parameter columns

2. **`edited_INFORMSlife_years_averages_combined.csv`**
   - Life years lost data
   - Same structure as cancer cases file

3. **`edited_INFORMScancer_death_averages_combined.csv`**
   - Cancer deaths data
   - Same structure as cancer cases file

### File Format
```
Column Structure:
- FIT_before, FIT_during, FIT_after
- COLON_before, COLON_during, COLON_after  
- DIAG_before, DIAG_during, DIAG_after
- P_1, P_2, ..., P_180 (person-type outcomes)
```

### Person-Type Mapping
The 180 person-types represent all combinations of:
- **Gender**: Male, Female (2 categories)
- **Race**: White, Black, Other (3 categories)  
- **Age Groups**: 45-49, 50-54, 55-59, 60-64, 65-69, 70-74 (6 categories)
- **Total**: 2 × 3 × 6 = 36 combinations × 5 replicates = 180 person-types

## 🤖 Model Types

| Model | Best For | Training Time | Interpretability |
|-------|----------|---------------|------------------|
| **Linear Regression** | Quick analysis, baseline | Fast (15s) | High |
| **Decision Tree** | Rule-based insights | Medium (45s) | High |
| **Random Forest** | Robust predictions | Long (90s) | Medium |
| **Support Vector Regression** | Complex patterns | Longest (120s) | Low |
| **Lasso Regression** | Feature selection | Medium (60s) | Medium |
| **Ridge Regression** | Regularized linear | Medium (50s) | Medium |

*Training times are estimates for 100K population*

## 📈 Output Analysis

### Results Table
- **Real-time Comparison**: All scenarios vs baseline
- **Formatted Numbers**: Comma-separated for readability
- **Color Coding**: Baseline highlighted in yellow

### Comprehensive Download
The CSV download includes:

#### Results Section
- Cancer Cases, Life Years Lost, Cancer Deaths
- Model type and population size

#### Complete Parameters  
- All demographic distributions
- All screening parameters (before/during/after)
- Analysis metadata and timestamps

#### Summary Statistics
- Automatic calculation of differences from baseline
- Separate section for easy analysis

### Example Output Structure
```
Scenario_Name,Model_Type,Cancer_Cases,Life_Years_Lost,Cancer_Deaths,
Male_Percent,Female_Percent,White_Percent,Black_Percent,Other_Race_Percent,
Age_45_49_Percent,...,FIT_Before_COVID,FIT_During_COVID,FIT_After_COVID,...

Usual Care (Baseline),Random Forest,2540,15420,850,49,51,60,13,27,17,17,16,16,16,18,8,8,8,48,48,48,7,7,7
Enhanced Recovery,Random Forest,2380,14200,780,49,51,60,13,27,17,17,16,16,16,18,8,12,10,48,60,55,7,10,8
DIFFERENCE: Enhanced Recovery vs Baseline,Random Forest,-160,-1220,-70,...
```

## 📂 File Structure

```
colorectal-cancer-screening-tool/
├── app.R                                           # Main Shiny application
├── README.md                                       # This file
├── data/
│   ├── edited_INFORMScancer_after_averages_combined.csv
│   ├── edited_INFORMSlife_years_averages_combined.csv
│   └── edited_INFORMScancer_death_averages_combined.csv
├── docs/
│   ├── user_guide.md                              # Detailed user guide
│   └── technical_documentation.md                 # Technical specifications
└── examples/
    ├── sample_analysis.csv                        # Example output
    └── demo_scenarios.md                          # Example scenarios
```

## 🔧 Advanced Features

### Parameter Validation Rules
- **FIT**: Before < During, After < During, Before < After
- **Colonoscopy**: Before < During, After < During, Before < After  
- **Diagnostic**: Before < During, After < During, Before < After

### Model Caching
- Automatically caches trained models
- Avoids retraining identical configurations
- Significant time savings for repeated analyses

### Error Handling
- Graceful handling of missing data files
- Fallback dummy models for insufficient data
- Clear error messages and notifications

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Setup
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Reporting Issues
Please use the [GitHub Issues](https://github.com/yourusername/colorectal-cancer-screening-tool/issues) page to report bugs or request features.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 📚 Citation

If you use this tool in your research, please cite:

```bibtex
@software{colorectal_screening_tool,
  title={Metamodel Decision Tool for Estimating Colorectal Cancer Health Outcomes},
  author={Sai Srikar Puppala},
  year={2025},
  url={https://github.com/puppalasaisrikar/colorectal-cancer-screening-tool}
}
```

## 🙏 Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)
- Machine learning models powered by [caret](https://topepo.github.io/caret/)
- UI components from [shinyBS](https://github.com/ebailey78/shinyBS)

## 📞 Support

- **Documentation**: Check the [User Guide](user_guide.md)
- **Email**: srikarsai.puppala@gmail.com

---

**Last Updated**: August 2025  
