# User Guide: Metamodel Decision Tool for Colorectal Cancer Health Outcomes

## Table of Contents
1. [Getting Started](#getting-started)
2. [Understanding the Interface](#understanding-the-interface)
3. [Step-by-Step Tutorial](#step-by-step-tutorial)
4. [Advanced Features](#advanced-features)
5. [Interpreting Results](#interpreting-results)
6. [Common Use Cases](#common-use-cases)
7. [Troubleshooting](#troubleshooting)

## Getting Started

### System Requirements
- R version 4.0.0 or higher
- Minimum 4GB RAM (8GB recommended for Random Forest models)
- Modern web browser (Chrome, Firefox, Safari, Edge)

### First Time Setup
1. Ensure all required packages are installed (see main README)
2. Place the three required CSV data files in your app directory
3. Launch the application using `shiny::runApp("app.R")`
4. The application will open in your default web browser

### Data File Requirements
Your app directory must contain these exact files:
- `edited_INFORMScancer_after_averages_combined.csv`
- `edited_INFORMSlife_years_averages_combined.csv`
- `edited_INFORMScancer_death_averages_combined.csv`

## Understanding the Interface

### Main Sections

#### 1. Global Configuration Panel
- **Purpose**: Sets baseline parameters applied to all scenarios
- **Key Inputs**: Population size, model type, demographics, baseline screening rates
- **Location**: Top section, always visible

#### 2. Scenario Configuration Panel
- **Purpose**: Define number and names of comparison scenarios
- **Key Inputs**: Number of scenarios (1-5), custom scenario names
- **Location**: Middle section, expands based on scenario count

#### 3. Dynamic Scenario Panels
- **Purpose**: Configure individual scenario parameters
- **Key Inputs**: During-COVID and After-COVID screening rates
- **Location**: Generated dynamically below configuration

#### 4. Results Comparison Section
- **Purpose**: View and download analysis results
- **Outputs**: Interactive table, comprehensive CSV download
- **Location**: Bottom section, always visible

### Understanding Parameters

#### Screening Types
- **FIT (Fecal Immunochemical Test)**: Non-invasive stool-based screening
- **COLON (Colonoscopy)**: Comprehensive diagnostic and screening procedure
- **DIAG (Diagnostic)**: Follow-up diagnostic interventions

#### Time Periods
- **Before**: Pre-COVID-19 baseline rates
- **During**: Peak pandemic disruption rates
- **After**: Post-pandemic recovery rates

#### Validation Rules
All parameters must follow: Before < During AND After < During AND Before < After

## Step-by-Step Tutorial

### Tutorial 1: Basic Analysis (Single Scenario)

#### Step 1: Global Configuration
1. Set **Total Population**: 100,000
2. Choose **Model Type**: Start with "Linear Regression" (fastest)
3. Click **"Use U.S. Census Defaults"** for demographics
4. Set baseline screening rates:
   - FIT Before: 8
   - COLON Before: 48
   - DIAG Before: 7

#### Step 2: Scenario Setup
1. Keep **Number of Scenarios**: 1
2. In scenario names, enter: "Enhanced Recovery"
3. Click **"Apply Scenario Names"**

#### Step 3: Configure Scenario
1. In the "Enhanced Recovery" panel:
   - FIT During: 12, After: 10
   - COLON During: 60, After: 55
   - DIAG During: 10, After: 8
2. Verify green checkmark appears (parameters valid)

#### Step 4: Run Analysis
1. Click **"Run Prediction for Enhanced Recovery"**
2. Wait for progress completion (15-30 seconds)
3. Review results in the comparison table

#### Step 5: Download Results
1. Click **"Download Comprehensive Analysis"**
2. Open CSV file to see complete parameter documentation

### Tutorial 2: Multi-Scenario Comparison

#### Step 1: Setup Multiple Scenarios
1. Set **Number of Scenarios**: 3
2. Name scenarios:
   - Scenario 1: "Conservative Recovery"
   - Scenario 2: "Moderate Recovery" 
   - Scenario 3: "Aggressive Recovery"
3. Apply scenario names

#### Step 2: Configure Each Scenario
**Conservative Recovery:**
- FIT: During 10, After 9
- COLON: During 52, After 50
- DIAG: During 8, After 7.5

**Moderate Recovery:**
- FIT: During 12, After 10
- COLON: During 55, After 52
- DIAG: During 10, After 8

**Aggressive Recovery:**
- FIT: During 15, After 12
- COLON: During 60, After 55
- DIAG: During 12, After 10

#### Step 3: Run All Scenarios
1. Run each scenario individually
2. Compare results in the table
3. Download comprehensive analysis

## Advanced Features

### Model Selection Guide

| Model | Best For | Training Time | When to Use |
|-------|----------|---------------|-------------|
| Linear Regression | Quick analysis, baseline | 15s | Initial exploration, presentations |
| Decision Tree | Interpretable results | 45s | Understanding decision rules |
| Random Forest | Most accurate predictions | 90s | Final analysis, publications |
| Support Vector Regression | Complex patterns | 120s | Non-linear relationships |
| Lasso Regression | Feature selection | 60s | Identifying key parameters |
| Ridge Regression | Regularized predictions | 50s | Avoiding overfitting |

### Demographic Customization

#### Understanding Population Weights
- Tool models 180 distinct demographic profiles
- Each represents: Gender × Race × Age Group combinations
- Your demographic inputs determine weighting across these profiles

#### Custom Demographics Example
For a rural population:
- Adjust race distribution (higher White percentage)
- Modify age distribution (potentially older population)
- Keep gender distribution close to 50/50

### Parameter Validation

#### Common Validation Errors
1. **"FIT Before must be less than FIT During"**
   - Solution: Ensure During rates exceed Before rates
   - Rationale: During-COVID represents increased screening

2. **"Parameter validation failed"**
   - Check all nine parameters follow the ordering rules
   - Use the validation messages to identify specific issues

### Model Caching
- The tool automatically caches trained models
- Identical configurations won't retrain models
- Saves significant time for repeated analyses
- Cache is reset when you restart the application

## Interpreting Results

### Understanding the Outputs

#### Cancer Cases Avoided (CCA)
- **Definition**: Number of cancer cases prevented by screening
- **Higher values**: Better screening effectiveness
- **Typical range**: Hundreds to thousands for large populations

#### Life Years Lost (LYL)
- **Definition**: Total years of life lost due to cancer deaths
- **Lower values**: Better screening effectiveness
- **Typical range**: Thousands to tens of thousands

#### Cancer Deaths (CD)
- **Definition**: Number of deaths prevented by screening
- **Higher values**: Better screening effectiveness
- **Typical range**: Tens to hundreds for large populations

### Comparative Analysis

#### Baseline Comparison
- Baseline represents "Usual Care" (no change from pre-COVID)
- Compare all scenarios against this baseline
- Look for consistent improvements across all three metrics

#### Scenario Ranking
1. **Most Effective**: Highest CCA and CD, Lowest LYL
2. **Cost-Effective**: Moderate improvements with feasible parameter values
3. **Realistic**: Achievable screening rate improvements

### Statistical Considerations

#### Model Uncertainty
- Results are point estimates, not confidence intervals
- Different models may give different predictions
- Use Random Forest for most robust estimates

#### Population Scaling
- Results scale proportionally with population size
- Per-capita rates remain constant
- Useful for extrapolating to different geographic areas

## Common Use Cases

### Use Case 1: Post-COVID Recovery Planning
**Scenario**: State health department planning catch-up screening

**Setup:**
- Population: State's eligible screening population
- Model: Random Forest (most accurate)
- Scenarios: Different recovery timeline speeds

**Analysis:**
- Compare gradual vs rapid recovery scenarios
- Identify optimal resource allocation
- Estimate health impact of delays

### Use Case 2: Budget Impact Analysis
**Scenario**: Healthcare system evaluating screening expansion

**Setup:**
- Population: System's catchment area
- Model: Linear Regression (interpretable)
- Scenarios: Different expansion levels

**Analysis:**
- Cost per outcome improvement
- Diminishing returns identification
- Budget justification data

### Use Case 3: Health Equity Research
**Scenario**: Academic research on screening disparities

**Setup:**
- Population: Diverse urban area
- Model: Multiple models for robustness
- Demographics: Varied race/age distributions

**Analysis:**
- Differential impact across populations
- Equity-focused intervention design
- Publication-ready results

### Use Case 4: Grant Proposal Development
**Scenario**: Researchers seeking intervention funding

**Setup:**
- Population: Proposed study population
- Model: Conservative (Linear Regression)
- Scenarios: Proposed intervention vs control

**Analysis:**
- Expected health benefits
- Sample size justification
- Preliminary impact estimates

## Troubleshooting

### Common Issues and Solutions

#### Application Won't Start
**Error**: "Cannot find function 'runApp'"
- **Solution**: Install shiny package: `install.packages("shiny")`

**Error**: "File not found: [filename].csv"
- **Solution**: Ensure all three CSV files are in the app directory with exact filenames

#### Slow Performance
**Issue**: Models taking very long to train
- **Solution**: Start with Linear Regression, then try more complex models
- **Check**: Available RAM (close other applications)

#### Validation Errors
**Issue**: Parameters fail validation despite appearing correct
- **Solution**: Check for decimal precision (use 8.0 instead of 8)
- **Verify**: All nine parameters follow ordering rules

#### Results Seem Unrealistic
**Issue**: Extremely high or low outcome numbers
- **Check**: Population size (should be realistic)
- **Verify**: Screening rates are percentages (8 = 8%, not 0.08)
- **Consider**: Different model types may give different results

#### Browser Issues
**Issue**: Interface doesn't display correctly
- **Solution**: Try different browser (Chrome recommended)
- **Check**: Browser zoom level (100% recommended)
- **Clear**: Browser cache and cookies

### Getting Help

#### Self-Help Resources
1. Check parameter validation messages
2. Review this user guide
3. Examine the technical documentation
4. Try simpler scenarios first

#### Contact Support
- **GitHub Issues**: [Repository Issues Page]
- **Email**: srikarsai.puppala@gmail.com
- **Include**: Error messages, screenshots, and steps to reproduce

### Performance Tips

#### For Large Populations (>500K)
- Start with Linear Regression
- Use Random Forest only for final analysis
- Consider running analyses during off-peak hours

#### For Multiple Scenarios
- Configure all scenarios before running any
- Run scenarios in order of importance
- Save results frequently using download feature

#### For Presentation Purposes
- Use Linear Regression for speed
- Focus on relative differences between scenarios
- Round results appropriately for audience

---

## Next Steps

After completing this user guide:
1. Review the [Technical Documentation](technical_documentation.md) for model details
2. Explore the [Example Scenarios](../examples/demo_scenarios.md)
3. Check the main [README](../README.md) for updates

For advanced users interested in modifying the tool, see the technical documentation for code structure and customization options.
