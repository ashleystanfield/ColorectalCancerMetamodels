# User Guide: Metamodel Decision Tool for Colorectal Cancer Health Outcomes

## Table of Contents
1. [Getting Started](#getting-started)
2. [Understanding the Interface](#understanding-the-interface)
3. [Single Population Analysis](#single-population-analysis)
4. [Multi-Population Analysis](#multi-population-analysis)
5. [Interpreting Results](#interpreting-results)
6. [Best Practices](#best-practices)
7. [Common Workflows](#common-workflows)
8. [Troubleshooting](#troubleshooting)
9. [FAQ](#faq)
10. [Glossary](#glossary)

---

## Getting Started

### What is the Metamodel Decision Tool?

The Metamodel Decision Tool is a web-based application that helps healthcare researchers, policymakers, and analysts evaluate the impact of different colorectal cancer screening strategies on population health outcomes. Using machine learning models trained on comprehensive cancer screening data, the tool predicts:

- **Cancer Cases (CCA)**: The number of colorectal cancer cases expected in a population
- **Life Years Lost (LYL)**: The total years of life lost due to colorectal cancer
- **Cancer Deaths (CD)**: The number of deaths from colorectal cancer

### Who Should Use This Tool?

This tool is designed for:
- **Public Health Officials**: Evaluating screening program effectiveness
- **Healthcare Policymakers**: Comparing different screening strategies
- **Researchers**: Analyzing population-level cancer screening impacts
- **Healthcare Administrators**: Planning screening program implementations
- **Students and Educators**: Learning about cancer screening modeling

### Prerequisites

Before using the tool, you should:
- Understand basic colorectal cancer screening concepts (FIT, colonoscopy, diagnostic screening)
- Have access to the application (via web browser)
- Know your target population size and demographic composition
- Have defined screening scenarios you want to evaluate

### First Time Setup

1. **Open the Application**
   - Navigate to the application URL in your web browser
   - Recommended browsers: Chrome, Firefox, Safari, Edge
   - Ensure JavaScript is enabled

2. **Familiarize Yourself with the Interface**
   - The application has two main modes: Single Population and Multi-Population
   - Start with Single Population mode to learn the basics

3. **Verify Data Files**
   - The application should load automatically
   - If you see "File not found" errors, contact your system administrator

---

## Understanding the Interface

### Main Components

#### 1. Analysis Mode Selection
At the top of the application, you'll see:
- **Single Population Analysis**: For analyzing one population at a time
- **Multi-Population Comparative Analysis**: For comparing multiple sub-populations

#### 2. Global Configuration Panel
This panel controls settings that apply to all scenarios:
- **Total Population**: The size of the population you're analyzing
- **Model Type**: The machine learning algorithm to use
- **Baseline Screening Parameters**: The "before" rates for FIT, Colonoscopy, and Diagnostic screening
- **Demographics**: Population composition by gender, race, and age

#### 3. Scenario Configuration
- **Number of Scenarios**: Choose how many scenarios to compare (1-5)
- **Scenario Names**: Give meaningful names to your scenarios
- **Scenario Panels**: Configure each scenario's screening parameters

#### 4. Results Display
- **Comparison Table**: Side-by-side comparison of all scenarios
- **Download Button**: Export comprehensive analysis to CSV

### Key Concepts

#### Screening Parameters

**FIT (Fecal Immunochemical Test)**
- Non-invasive stool test for blood
- Range: 0-30% (percentage of population screened)
- Lower cost, lower sensitivity than colonoscopy

**Colonoscopy**
- Direct visualization of colon
- Range: 30-70% (percentage of population screened)
- Gold standard but higher cost and invasiveness

**Diagnostic Screening**
- Follow-up testing after positive results
- Range: 0-90% (percentage of population screened)
- Critical for confirming cancer presence

#### Time Periods

Each screening parameter has three time periods:

1. **Before**: Baseline screening rate (starting point)
2. **During**: Peak screening rate (during intervention)
3. **After**: Sustained screening rate (post-intervention)

**Important Rule**: Before < After < During

This ensures logical progression where:
- Screening increases from baseline during intervention
- Some improvement is maintained after intervention
- Peak rates occur during the intervention period

### Visual Indicators

**Color Coding**:
- 🟢 Green backgrounds: Valid parameters, successful results
- 🟡 Yellow backgrounds: Baseline (usual care) scenarios
- 🔴 Red backgrounds: Validation errors, invalid parameters
- 🔵 Blue accents: Active controls and headers

**Status Messages**:
- ✓ Check marks: Successful validation or completion
- ⚠️ Warning symbols: Validation errors
- 🔄 Spinner: Processing in progress

---

## Single Population Analysis

### Step-by-Step Guide

#### Step 1: Configure Global Settings

1. **Set Total Population**
   ```
   Example: 100,000
   Minimum: 1,000
   Tip: Use realistic population sizes for your jurisdiction
   ```

2. **Select Model Type**
   ```
   Options:
   - Linear Regression (fastest, ~30 seconds)
   - Decision Tree (~1-2 minutes)
   - Random Forest (~2-3 minutes)
   - Support Vector Regression (~2-4 minutes)
   - Lasso Regression (~1-2 minutes)
   - Ridge Regression (~1-2 minutes)
   
   Recommendation: Start with Linear Regression for quick results
   ```

3. **Set Baseline Screening Parameters**
   ```
   FIT Before: 8% (typical U.S. baseline)
   Colonoscopy Before: 48% (typical U.S. baseline)
   Diagnostic Before: 7% (typical U.S. baseline)
   
   Adjust these based on your population's actual screening rates
   ```

4. **Configure Demographics (Optional but Recommended)**
   
   Click "Show/Hide Demographics" to expand the section.
   
   **Gender Distribution**:
   ```
   Male: 49%
   Female: 51%
   Total must equal 100%
   ```
   
   **Race Distribution**:
   ```
   White: 60%
   Black: 13%
   Other: 27%
   Total must equal 100%
   ```
   
   **Age Group Distribution**:
   ```
   45-49: 17%
   50-54: 17%
   55-59: 16%
   60-64: 16%
   65-69: 16%
   70-74: 18%
   Total must equal 100%
   ```
   
   **Quick Setup**: Click "Use U.S. Census Defaults" to apply typical U.S. demographics

#### Step 2: Configure Scenarios

1. **Choose Number of Scenarios**
   ```
   Select 1-5 scenarios depending on your analysis needs
   
   Examples:
   - 1 scenario: Single intervention evaluation
   - 2 scenarios: Compare two strategies
   - 3+ scenarios: Comprehensive policy comparison
   ```

2. **Name Your Scenarios**
   ```
   Good names are descriptive and meaningful:
   ✓ "Enhanced FIT Outreach"
   ✓ "Colonoscopy Expansion"
   ✓ "Multi-Modal Strategy"
   
   Avoid generic names:
   ✗ "Scenario 1"
   ✗ "Test"
   ```

3. **Configure Each Scenario**
   
   For each scenario, set the "During" and "After" parameters:
   
   **Example: Enhanced FIT Outreach**
   ```
   FIT During: 15%    (increased from 8% baseline)
   FIT After: 12%     (some sustained increase)
   
   Colonoscopy During: 55%    (modest increase from 48%)
   Colonoscopy After: 52%     (sustained improvement)
   
   Diagnostic During: 12%     (increased from 7%)
   Diagnostic After: 10%      (sustained improvement)
   ```
   
   **Remember**: Before < After < During for all parameters

#### Step 3: Run Predictions

1. **Validate Parameters**
   - Check for validation messages below each scenario
   - Green checkmark (✓) = all valid
   - Red warnings (⚠️) = fix errors before running

2. **Run Each Scenario**
   - Click "Run Prediction for [Scenario Name]"
   - Wait for model loading (progress shown in modal)
   - Results appear in green box when complete

3. **Review Individual Results**
   ```
   Example Output:
   Cancer Cases: 12,345
   Life Years Lost: 54,321
   Cancer Deaths: 987
   Model Used: Linear Regression
   Population: 100,000
   ```

#### Step 4: Compare Results

The comparison table shows all scenarios side-by-side:

```
┌─────────────────┬──────────────┬────────────┬──────────────┬──────────────┬──────────────┐
│ Scenario        │ Model Type   │ Population │ Cancer Cases │ Life Years   │ Cancer Deaths│
│                 │              │            │              │ Lost         │              │
├─────────────────┼──────────────┼────────────┼──────────────┼──────────────┼──────────────┤
│ Usual Care      │ Linear Reg.  │ 100,000    │ 12,345       │ 54,321       │ 987          │
│ (Baseline)      │              │            │              │              │              │
├─────────────────┼──────────────┼────────────┼──────────────┼──────────────┼──────────────┤
│ Enhanced FIT    │ Linear Reg.  │ 100,000    │ 11,890       │ 52,100       │ 945          │
├─────────────────┼──────────────┼────────────┼──────────────┼──────────────┼──────────────┤
│ Colonoscopy     │ Linear Reg.  │ 100,000    │ 11,200       │ 49,800       │ 890          │
│ Expansion       │              │            │              │              │              │
└─────────────────┴──────────────┴────────────┴──────────────┴──────────────┴──────────────┘

Impact Analysis:
Enhanced FIT vs Baseline:
- Cancer Cases Prevented: 455 (3.7% reduction)
- Life Years Saved: 2,221 (4.1% reduction)
- Deaths Prevented: 42 (4.3% reduction)
```

#### Step 5: Export Results

Click "Download Comprehensive Analysis" to export:
- All scenario results
- All input parameters (demographics, screening rates)
- Calculated differences from baseline
- Analysis metadata (date, model type)

The exported CSV includes 30+ columns with complete analysis details.

### Advanced Single Population Features

#### Comparing Different Models

You can compare how different machine learning models perform:

1. **Run Analysis with Linear Regression**
2. **Change Global Model Type to Random Forest**
3. **Run Analysis Again**
4. **Compare Results**

This helps understand model sensitivity and prediction robustness.

#### Sensitivity Analysis

Test how results change with different parameters:

1. **Run baseline analysis**
2. **Adjust one parameter (e.g., FIT During from 15% to 20%)**
3. **Run again**
4. **Compare results to see impact**

This identifies which parameters have the most impact on outcomes.

---

## Multi-Population Analysis

Multi-population analysis allows you to compare outcomes across different sub-populations (e.g., urban vs. rural, different regions, different demographics).

### Step-by-Step Guide

#### Step 1: Switch to Multi-Population Mode

1. At the top of the application, select **"Multi-Population Comparative Analysis"**
2. The interface will change to show sub-population setup

#### Step 2: Define Sub-Populations

1. **Set Total Population**
   ```
   Example: 1,000,000 (entire state or region)
   ```

2. **Choose Number of Sub-Populations**
   ```
   Range: 2-10 sub-populations
   Example: 3 sub-populations (Urban, Suburban, Rural)
   ```

3. **Select Input Mode**
   - **Percentages (%)**: Define each sub-population as percentage of total
   - **Absolute Numbers**: Define each sub-population by exact count

4. **Define Each Sub-Population**
   
   **Example - Percentage Mode**:
   ```
   Sub-Population 1:
   Name: Urban Population
   Percentage: 50% (500,000 people)
   
   Sub-Population 2:
   Name: Suburban Population
   Percentage: 30% (300,000 people)
   
   Sub-Population 3:
   Name: Rural Population
   Percentage: 20% (200,000 people)
   
   Total: 100% ✓
   ```
   
   **Example - Absolute Mode**:
   ```
   Sub-Population 1:
   Name: Urban Population
   Count: 500,000
   
   Sub-Population 2:
   Name: Suburban Population
   Count: 300,000
   
   Sub-Population 3:
   Name: Rural Population
   Count: 200,000
   
   Total: 1,000,000 ✓
   ```

5. **Validate Sub-Populations**
   - Click "Validate Sub-Populations"
   - Ensure green success message appears
   - Fix any validation errors if present

#### Step 3: Configure Each Sub-Population

After validation, expandable panels appear for each sub-population.

**For Each Sub-Population**:

1. **Expand the Panel**
   - Click "Expand/Collapse" button
   - Full configuration interface appears

2. **Configure Global Settings**
   - Population size is pre-filled (read-only)
   - Select model type for this sub-population
   - Set baseline screening parameters
   - Configure demographics (can differ by sub-population)

3. **Set Up Scenarios**
   - Choose number of scenarios (1-5)
   - Name scenarios
   - Configure During/After parameters

4. **Run Predictions**
   - Run each scenario individually
   - Review results for this sub-population

**Example - Different Demographics by Sub-Population**:

```
Urban Population:
- Demographics: Higher diversity (White 40%, Black 25%, Other 35%)
- Age: Younger population (more in 45-54 age groups)
- Baseline FIT: Higher (12%)

Rural Population:
- Demographics: Less diverse (White 80%, Black 5%, Other 15%)
- Age: Older population (more in 65-74 age groups)
- Baseline FIT: Lower (5%)
```

#### Step 4: Review Multi-Population Results

The multi-population results section has four tabs:

**1. Individual Results**
```
All results from all sub-populations and all scenarios combined:
┌────────────────┬────────────────┬──────────────┬────────────┬──────────────┐
│ Sub-Population │ Scenario       │ Model Type   │ Population │ Cancer Cases │
├────────────────┼────────────────┼──────────────┼────────────┼──────────────┤
│ Urban          │ Baseline       │ Linear Reg.  │ 500,000    │ 6,200        │
│ Urban          │ Enhanced FIT   │ Linear Reg.  │ 500,000    │ 5,950        │
│ Suburban       │ Baseline       │ Linear Reg.  │ 300,000    │ 3,600        │
│ Suburban       │ Enhanced FIT   │ Linear Reg.  │ 300,000    │ 3,480        │
│ Rural          │ Baseline       │ Linear Reg.  │ 200,000    │ 2,800        │
│ Rural          │ Enhanced FIT   │ Linear Reg.  │ 200,000    │ 2,650        │
└────────────────┴────────────────┴──────────────┴────────────┴──────────────┘
```

**2. Cross-Population Comparison**
```
Baseline rates per 100,000 population for comparison:
┌────────────────┬─────────────────────┬─────────────────────┬────────────────┐
│ Sub-Population │ Cancer Rate per     │ LYL Rate per 100k   │ Death Rate per │
│                │ 100k                │                     │ 100k           │
├────────────────┼─────────────────────┼─────────────────────┼────────────────┤
│ Urban          │ 1,240               │ 10,500              │ 195            │
│ Suburban       │ 1,200               │ 10,200              │ 188            │
│ Rural          │ 1,400               │ 12,100              │ 220            │
└────────────────┴─────────────────────┴─────────────────────┴────────────────┘

Key Insights:
- Rural areas have 13% higher cancer rates than urban
- Rural areas have 15% higher death rates
- This suggests rural populations need targeted interventions
```

**3. Aggregated Analysis**
```
Total across all sub-populations:
Total Population: 1,000,000
Total Cancer Cases: 12,600
Total Life Years Lost: 108,000
Total Cancer Deaths: 2,030
Average Cancer Rate per 100k: 1,260
```

**4. Summary Statistics**
```
Statistical summary across sub-populations:
Number of Sub-Populations: 3
Mean Cancer Rate: 1,280 per 100k
Median Cancer Rate: 1,240 per 100k
Range: 1,200 - 1,400 per 100k
Sub-Population with Highest Rate: Rural Population
Sub-Population with Lowest Rate: Suburban Population
```

#### Step 5: Export Multi-Population Analysis

Click "Download Complete Multi-Population Analysis" to export comprehensive data including:
- All sub-population results
- All scenarios per sub-population
- Demographics and parameters for each
- Calculated rates per 100,000
- Cross-population comparisons
- Summary statistics

---

## Interpreting Results

### Understanding the Outcomes

#### Cancer Cases (CCA)
- **What it means**: Projected number of new colorectal cancer cases
- **Interpretation**: Lower is better
- **Typical range**: 1,000-2,000 per 100,000 population annually
- **Use cases**: Budget planning, resource allocation, program evaluation

#### Life Years Lost (LYL)
- **What it means**: Total years of life lost due to colorectal cancer deaths
- **Interpretation**: Lower is better
- **Why it matters**: Captures both mortality and age at death (younger deaths = more years lost)
- **Typical range**: 8,000-15,000 per 100,000 population
- **Use cases**: Quality of life analysis, societal burden assessment

#### Cancer Deaths (CD)
- **What it means**: Number of deaths from colorectal cancer
- **Interpretation**: Lower is better
- **Typical range**: 150-250 per 100,000 population annually
- **Use cases**: Mortality reduction targets, survival rate estimation

### Comparing Scenarios

#### Absolute Differences
```
Example:
Baseline Cancer Cases: 12,345
Enhanced FIT Cancer Cases: 11,890
Absolute Difference: 455 cases prevented
```

#### Relative Differences (Percent Change)
```
Percent Reduction = (Baseline - Scenario) / Baseline × 100%
                  = (12,345 - 11,890) / 12,345 × 100%
                  = 3.69% reduction
```

#### Cost-Benefit Analysis (Not Included but Can Be Calculated)
```
If you have cost data:
Cases Prevented: 455
Cost of Intervention: $1,000,000
Cost per Case Prevented: $1,000,000 / 455 = $2,198

Deaths Prevented: 42
Cost per Death Prevented: $1,000,000 / 42 = $23,810
```

### Evaluating Model Performance

When comparing different ML models on the same scenario:

```
Linear Regression: 11,890 cases
Random Forest: 11,756 cases
SVR: 11,923 cases

Average: 11,856 cases
Standard Deviation: 86 cases

Interpretation: Models agree within ~1%, predictions are robust
```

### Statistical Significance (Guidance)

The tool doesn't calculate p-values, but general guidelines:

- **Small differences (<2%)**: May not be meaningful
- **Moderate differences (2-5%)**: Likely meaningful if population is large
- **Large differences (>5%)**: Typically meaningful and actionable

### Red Flags in Results

**Be cautious if**:
- Results change dramatically with small parameter adjustments (>10% change from 1% parameter change)
- Different models give vastly different predictions (>20% difference)
- Results seem unrealistic (e.g., 0 deaths, or more deaths than cases)

**If you encounter red flags**:
1. Verify input parameters
2. Check demographic totals = 100%
3. Try different model types
4. Consider data quality issues
5. Consult with domain experts

---

## Best Practices

### Parameter Selection

#### Realistic Screening Rates

Use evidence-based screening rates:

**FIT Rates**:
```
Conservative: 5-10%
Moderate: 10-15%
Aggressive: 15-25%
Maximum modeled: 30%
```

**Colonoscopy Rates**:
```
Conservative: 40-50%
Moderate: 50-60%
Aggressive: 60-70%
Maximum modeled: 70%
```

**Diagnostic Rates**:
```
Conservative: 5-10%
Moderate: 10-20%
Aggressive: 20-40%
Maximum modeled: 90%
```

#### Incremental Changes

When comparing scenarios, use realistic increments:

```
Good Practice:
Baseline FIT: 8%
Scenario 1: 10% (+2 percentage points)
Scenario 2: 12% (+4 percentage points)
Scenario 3: 15% (+7 percentage points)

Avoid Unrealistic Jumps:
Baseline FIT: 8%
Scenario 1: 25% (+17 percentage points - unrealistic)
```

### Demographic Configuration

#### Use Census Data When Available

For U.S. populations, the "Use U.S. Census Defaults" button provides reasonable estimates. For specific jurisdictions:

1. Look up actual census data for your area
2. Adjust sliders to match
3. Ensure totals = 100% for each category

#### Consider Population Characteristics

Different populations may have different risk profiles:

**Higher Risk Populations**:
- Older age groups (65-74)
- Certain racial/ethnic groups (African Americans have higher CRC rates)
- Lower screening baseline rates

**Lower Risk Populations**:
- Younger age groups (45-54)
- Populations with high baseline screening rates

### Scenario Design

#### Start Simple, Add Complexity

```
Analysis 1: Single intervention (FIT only)
Analysis 2: Multi-modal (FIT + Colonoscopy)
Analysis 3: Comprehensive (FIT + Colonoscopy + Diagnostic)
```

#### Test Boundaries

Run analyses at the extremes to understand range of outcomes:

```
Pessimistic Scenario: Minimal improvements
Baseline Scenario: Current state
Optimistic Scenario: Maximum achievable improvements
```

#### Include Status Quo

Always include a baseline/"usual care" scenario for comparison:

```
Scenario 1: Usual Care (Baseline)
Scenario 2: Proposed Intervention
Scenario 3: Enhanced Intervention
```

### Model Selection

#### Choose Model Based on Needs

**Use Linear Regression when**:
- Quick results needed
- Exploratory analysis
- Simple interpretability required
- Limited computational resources

**Use Random Forest when**:
- Highest accuracy needed
- Non-linear relationships expected
- Time is available (~3 minutes)
- Final/publication-quality results needed

**Use Other Models for**:
- Sensitivity analysis (compare multiple models)
- Validation of Linear Regression results
- Research purposes

### Documentation

#### Record Your Analysis

Create a analysis log:

```
Analysis Date: 2025-10-31
Population: State of North Carolina
Population Size: 2,500,000
Objective: Evaluate enhanced FIT screening program

Scenarios:
1. Baseline (Current State)
   - FIT: 8%, Colonoscopy: 48%, Diagnostic: 7%

2. Enhanced FIT Outreach
   - FIT: 15% (During), 12% (After)
   - Rationale: Statewide media campaign and provider education

Results:
- Cases Prevented: 1,137 (3.8% reduction)
- Lives Saved: 105 deaths prevented
- Cost per Case Prevented: $2,250 (estimated)

Recommendation: Implement enhanced FIT outreach program
```

#### Save Exports

Create organized folder structure:

```
project_name/
├── 2025-10-31_baseline_analysis.csv
├── 2025-10-31_sensitivity_analysis.csv
├── 2025-10-31_final_recommendations.csv
└── analysis_notes.md
```

---

## Common Workflows

### Workflow 1: Single Intervention Evaluation

**Objective**: Evaluate the impact of a single proposed screening program

**Steps**:
1. Configure baseline parameters to match current state
2. Create 2 scenarios:
   - Scenario 1: Usual Care (baseline)
   - Scenario 2: Proposed Intervention
3. Set intervention parameters (During/After rates)
4. Run both scenarios
5. Calculate differences
6. Export results for stakeholder presentation

**Time Required**: 5-10 minutes

### Workflow 2: Multiple Intervention Comparison

**Objective**: Compare several alternative screening strategies

**Steps**:
1. Configure baseline parameters
2. Create 4-5 scenarios:
   - Scenario 1: Usual Care
   - Scenario 2: Enhanced FIT Only
   - Scenario 3: Enhanced Colonoscopy Only
   - Scenario 4: Multi-Modal Enhancement
   - Scenario 5: Comprehensive Maximum Effort
3. Run all scenarios
4. Compare results in table
5. Identify most cost-effective approach
6. Export for decision-making

**Time Required**: 15-20 minutes

### Workflow 3: Geographic Comparison

**Objective**: Compare screening needs and impacts across different regions

**Steps**:
1. Switch to Multi-Population mode
2. Define sub-populations by region:
   - Urban areas
   - Suburban areas
   - Rural areas
3. Configure different demographics for each region
4. Set appropriate baseline rates for each
5. Apply same intervention to all regions
6. Compare per-capita rates across regions
7. Identify high-need areas
8. Export comprehensive analysis

**Time Required**: 30-45 minutes

### Workflow 4: Sensitivity Analysis

**Objective**: Understand how robust your results are to parameter changes

**Steps**:
1. Run baseline analysis
2. Record results
3. Change one parameter slightly (e.g., FIT During from 15% to 17%)
4. Run again
5. Calculate change in outcomes
6. Repeat for other key parameters
7. Identify which parameters have largest impact
8. Focus improvement efforts on high-impact parameters

**Time Required**: 20-30 minutes

### Workflow 5: Model Validation

**Objective**: Verify prediction stability across different ML models

**Steps**:
1. Configure scenario with specific parameters
2. Run with Linear Regression
3. Record results
4. Change model to Random Forest
5. Run same scenario
6. Change model to Decision Tree
7. Run same scenario
8. Compare results across models
9. Calculate mean and standard deviation
10. If results agree (SD < 5%), predictions are robust

**Time Required**: 10-15 minutes (depending on model training times)

---

## Troubleshooting

### Error Messages

#### "File not found: [filename].csv"

**Cause**: Required training data files are missing

**Solutions**:
1. Contact your system administrator
2. Verify all three CSV files are in application directory
3. Check file permissions
4. Restart application

#### "Parameter validation failed: FIT Before must be less than FIT During"

**Cause**: Screening parameters don't follow Before < After < During rule

**Solutions**:
1. Review all FIT parameters
2. Ensure FIT Before < FIT After < FIT During
3. Adjust values accordingly
4. Re-validate before running

Example:
```
❌ Wrong:
FIT Before: 10%
FIT After: 12%
FIT During: 11%  ← During must be highest

✅ Correct:
FIT Before: 10%
FIT After: 11%
FIT During: 12%
```

#### "Gender/Race/Age percentages must sum to 100%"

**Cause**: Demographics don't total exactly 100%

**Solutions**:
1. Check each demographic category
2. Adjust sliders until total = 100%
3. Use decimal precision if needed (49.5% + 50.5% = 100%)

Visual aid:
```
Male: 49% }
Female: 51% } = 100% ✓

White: 60% }
Black: 13% } = 100% ✓
Other: 27% }
```

#### "Total sub-population counts must equal total population"

**Cause**: In absolute mode, sub-population counts don't sum to total

**Solutions**:
1. Check your math
2. Adjust sub-population values
3. Verify total adds up correctly

Example:
```
Total Population: 1,000,000

Sub-Population 1: 400,000
Sub-Population 2: 350,000
Sub-Population 3: 250,000
Total: 1,000,000 ✓
```

#### "Model training failed"

**Cause**: Insufficient data or computational error

**Solutions**:
1. Try different model type (switch to Linear Regression)
2. Check population size (not too small)
3. Verify training data is loaded
4. Restart application
5. Contact support if persists

### Performance Issues

#### "Application is very slow"

**Causes & Solutions**:

1. **Large population size**
   - Solution: Use pre-computed models
   - Solution: Choose faster model (Linear Regression)

2. **Complex model selected**
   - Solution: Switch from Random Forest/SVR to Linear Regression
   - Solution: Be patient (RF takes 2-3 minutes)

3. **Multiple users**
   - Contact administrator about server resources

#### "Application freezes or crashes"

**Solutions**:
1. Refresh browser
2. Clear browser cache
3. Try different browser
4. Check internet connection
5. Contact administrator

### Validation Issues

#### "My demographics sum to 100% but I still get an error"

**Cause**: Rounding errors or decimal precision

**Solutions**:
1. Use one decimal place maximum
2. Double-check math: 49 + 51 = 100 ✓
3. Try adjusting by 0.1% increments
4. Use integer percentages when possible

#### "Results seem unrealistic"

**Possible Causes & Actions**:

1. **Unrealistic parameters**
   - Check if screening rates are evidence-based
   - Verify Before < After < During

2. **Demographics not representative**
   - Compare to census data
   - Verify age distribution is realistic

3. **Model artifact**
   - Try different model type
   - Run sensitivity analysis
   - Consult domain expert

### Results Issues

#### "Different models give very different results"

**What this means**: High model uncertainty

**Actions**:
1. Calculate mean across models
2. Report uncertainty range
3. Be cautious with recommendations
4. Consider additional data or validation

#### "Baseline shows 0 deaths or unrealistic values"

**Cause**: Likely data or parameter issue

**Solutions**:
1. Check total population (not too small)
2. Verify demographics sum to 100%
3. Check screening parameters are reasonable
4. Try different model
5. Contact support

### Export Issues

#### "Download button doesn't work"

**Solutions**:
1. Check browser allows downloads
2. Disable popup blockers
3. Try different browser
4. Check disk space

#### "Downloaded file won't open"

**Solutions**:
1. File should be CSV format
2. Open with Excel, Google Sheets, or text editor
3. Check file isn't corrupted
4. Re-download if necessary

---

## FAQ

### General Questions

**Q: How long does an analysis take?**

A: Timing depends on model type:
- Linear Regression: 30 seconds - 1 minute
- Decision Tree: 1-2 minutes
- Random Forest: 2-3 minutes
- SVR: 2-4 minutes
- Lasso/Ridge: 1-2 minutes

With pre-computed models, all types load in 5-10 seconds.

**Q: What population size should I use?**

A: Use the actual population size of your jurisdiction or study area. Minimum recommended: 1,000. Typical ranges:
- County: 50,000 - 500,000
- State: 1,000,000 - 10,000,000
- Region: 100,000 - 5,000,000

**Q: How many scenarios should I compare?**

A: Depends on your needs:
- Policy evaluation: 2-3 scenarios (baseline + 1-2 interventions)
- Comprehensive analysis: 4-5 scenarios
- Exploratory: Start with 2, add more as needed

**Q: Can I save my analysis and come back to it later?**

A: The application doesn't save state automatically. To preserve your work:
1. Export results to CSV
2. Take screenshots of configurations
3. Document parameters in external notes
4. Re-enter parameters when resuming

**Q: Which model should I use?**

A: 
- **First analysis**: Linear Regression (fast, interpretable)
- **Important decisions**: Random Forest (most accurate)
- **Validation**: Run multiple models and compare
- **Publications**: Report results from multiple models

### Technical Questions

**Q: What machine learning algorithms are used?**

A: The tool includes:
- Linear Regression (OLS)
- Decision Trees (CART algorithm)
- Random Forest (ensemble of decision trees)
- Support Vector Regression (RBF kernel)
- Lasso Regression (L1 regularization)
- Ridge Regression (L2 regularization)

Each outcome (CCA, LYL, CD) uses 180 separate models (one per demographic subgroup).

**Q: How accurate are the predictions?**

A: Accuracy depends on:
- Quality of training data
- Representativeness of your population
- Model type chosen
- Realistic parameters

The models are trained on comprehensive cancer screening data but predictions are estimates, not guarantees.

**Q: What's the difference between the models?**

A:
- **Linear Regression**: Assumes linear relationships, fast, interpretable
- **Random Forest**: Handles non-linearity, most accurate, slower
- **SVR**: Good with complex patterns, requires more data
- **Lasso/Ridge**: Linear with regularization, prevents overfitting

**Q: Why are there 180 person-specific models?**

A: The 180 models represent different demographic combinations:
- 2 genders × 3 races × 6 age groups × 5 replications = 180

This allows the tool to properly weight outcomes by population demographics.

**Q: How does demographic weighting work?**

A: The tool:
1. Distributes your total population across 180 demographic groups
2. Each group gets predictions from its specific model
3. Predictions are weighted by group size
4. Weighted predictions are summed for total outcomes

Example: If 10% of your population is Black males aged 65-69, those specific models get 10% of the weight.

### Methodological Questions

**Q: Should I trust the baseline that appears automatically?**

A: The automatic baseline is computed using your specified:
- Demographics
- Baseline screening rates
- Population size

It represents "usual care" (no intervention). You should verify the baseline seems reasonable before interpreting scenario differences.

**Q: What if I don't know exact demographics?**

A: Options:
1. Use U.S. Census defaults (click the button)
2. Use state/county census data
3. Make educated estimates
4. Run sensitivity analysis with different demographic assumptions

**Q: How do I interpret the time periods (Before, During, After)?**

A:
- **Before**: Current state before intervention
- **During**: Peak intervention period (e.g., 5-year intensive campaign)
- **After**: Sustained effects post-intervention (e.g., years 6-10)

The model assumes screening rates follow this trajectory.

**Q: Can I model a permanent change (not temporary intervention)?**

A: Yes! Set:
- Before: Current rate
- During: New permanent rate
- After: Same as During rate

Example:
```
Policy: Permanent FIT screening mandate at 15%

FIT Before: 8% (current)
FIT During: 15% (new policy)
FIT After: 15% (sustained)
```

**Q: What if I want to model a decline (e.g., funding cuts)?**

A: The tool requires Before < After < During, so it's designed for improvements. For declines:
1. Run baseline with current rates
2. Contact developers about reverse scenarios
3. Manually calculate impact using different baseline

### Results Interpretation

**Q: What's a "good" result?**

A: Depends on your goals:
- **Prevention-focused**: Lower cancer cases
- **Mortality-focused**: Lower cancer deaths
- **Quality of life**: Lower life years lost
- **Cost-effectiveness**: Best outcomes per dollar spent (requires external cost data)

Generally, any reduction from baseline is positive, but balance against implementation costs and feasibility.

**Q: Why do different models give different predictions?**

A: Different algorithms make different assumptions:
- Linear models assume linear relationships
- Random Forests capture non-linear patterns
- SVR handles complex interactions

If models disagree significantly (>10%), there's uncertainty. Report ranges and exercise caution.

**Q: How do I calculate "cases prevented"?**

A:
```
Cases Prevented = Baseline Cases - Scenario Cases

Example:
Baseline: 12,345 cases
Enhanced FIT: 11,890 cases
Cases Prevented: 12,345 - 11,890 = 455 cases
```

**Q: What's the difference between absolute and relative change?**

A:
```
Absolute Change = Baseline - Scenario
Relative Change = (Baseline - Scenario) / Baseline × 100%

Example:
Baseline: 1,000 deaths
Scenario: 950 deaths
Absolute: 50 deaths prevented
Relative: 5% reduction
```

Use both for complete picture.

### Practical Application

**Q: Can I use these results for grant applications?**

A: Yes, but:
1. Clearly state these are model-based estimates
2. Document all assumptions
3. Include uncertainty ranges if possible
4. Have domain experts review
5. Cite appropriate literature

**Q: How do I present results to stakeholders?**

A: Focus on:
1. **Clear comparisons**: Show baseline vs. intervention
2. **Absolute numbers**: "455 cases prevented"
3. **Percentages**: "3.7% reduction"
4. **Lives saved**: "42 deaths prevented"
5. **Visualizations**: Create charts from exported CSV

**Q: Can I use this for budget planning?**

A: The tool provides health outcomes but not costs. You can:
1. Export predicted outcomes
2. Apply cost data externally
3. Calculate cost-effectiveness
4. Use for budget justification

Example:
```
Cases Prevented: 455
Estimated Cost: $1,000,000
Cost per Case: $2,198
Cost per Death Prevented: $23,810
```

**Q: How often should I update my analysis?**

A: Update when:
- New census data available
- Screening rates change
- Interventions implemented
- Policy changes occur
- Annual reviews

---

## Glossary

### Screening Terms

**FIT (Fecal Immunochemical Test)**: Non-invasive stool test that detects hidden blood in stool, which can be an early sign of colorectal cancer.

**Colonoscopy**: Medical procedure where a flexible tube with a camera examines the entire colon and rectum to detect polyps or cancer.

**Diagnostic Screening**: Follow-up testing performed after a positive screening result to confirm the presence of cancer.

**Before Rate**: Baseline screening rate in the population before any intervention.

**During Rate**: Peak screening rate achieved during an active intervention period.

**After Rate**: Sustained screening rate maintained after the intervention period ends.

### Health Outcomes

**Cancer Cases (CCA)**: The number of new colorectal cancer diagnoses expected in the population.

**Life Years Lost (LYL)**: The total years of life lost across all cancer deaths, accounting for age at death (younger deaths = more years lost).

**Cancer Deaths (CD)**: The number of deaths attributable to colorectal cancer.

**Per Capita Rate**: Health outcomes expressed per 100,000 population, allowing comparison across different population sizes.

### Demographics

**Gender Distribution**: The breakdown of the population by male and female.

**Race/Ethnicity**: Population composition by racial/ethnic groups (White, Black, Other).

**Age Groups**: Population distribution across age ranges (45-49, 50-54, 55-59, 60-64, 65-69, 70-74).

**Demographic Weighting**: The process of distributing population across demographic subgroups to generate weighted predictions.

### Model Terms

**Machine Learning Model**: An algorithm that learns patterns from training data to make predictions on new data.

**Training Data**: Historical data used to teach the machine learning models relationships between screening parameters and health outcomes.

**Prediction**: The model's estimate of health outcomes based on input parameters.

**Model Validation**: The process of checking whether model predictions are reliable and accurate.

**Cross-Validation**: A technique used during model training to prevent overfitting and ensure generalization.

### Analysis Terms

**Baseline**: The reference scenario representing "usual care" or current state without intervention.

**Scenario**: A defined set of screening parameters representing a proposed intervention or strategy.

**Sensitivity Analysis**: Testing how results change when parameters are varied to understand robustness.

**Comparison Analysis**: Side-by-side evaluation of multiple scenarios to identify the most effective strategy.

**Absolute Difference**: The raw numerical difference between scenarios (e.g., 455 cases prevented).

**Relative Difference**: The percentage change between scenarios (e.g., 3.7% reduction).

### Population Terms

**Total Population**: The complete size of the population being analyzed.

**Sub-Population**: A distinct segment of the total population (e.g., urban residents, rural residents).

**Population Size**: The number of individuals in a population or sub-population.

**Jurisdictional Population**: The population of a specific geographic area (county, state, region).

### Technical Terms

**Parameters**: Input values that define a scenario (screening rates, demographics, population size).

**Validation**: The process of checking whether inputs meet required rules and constraints.

**Export**: Saving analysis results to a file (CSV format) for external use.

**Modal Dialog**: A popup window that appears during processing to show progress and status.

**Pre-computed Models**: Machine learning models trained in advance and saved for faster loading.

---

## Getting Help

### Support Resources

**Documentation**:
- This User Guide (comprehensive instructions)
- Technical Documentation (developer reference)
- README (quick start guide)

**Troubleshooting**:
- Check Troubleshooting section in this guide
- Review FAQ for common questions
- Verify validation messages in application

**Additional Help**:
- Contact your system administrator
- Submit issues on GitHub repository
- Email support: [your-email@example.com]

### Reporting Issues

When reporting problems, include:
1. What you were trying to do
2. What you expected to happen
3. What actually happened
4. Error messages (take screenshots)
5. Your configuration (population size, model type, etc.)
6. Browser and version

### Providing Feedback

We welcome feedback on:
- Usability improvements
- Feature requests
- Documentation clarity
- Bug reports
- Success stories

---

## Conclusion

The Metamodel Decision Tool provides a powerful way to evaluate colorectal cancer screening strategies and their impact on population health outcomes. By following the guidance in this user guide, you can:

- Configure realistic screening scenarios
- Generate evidence-based predictions
- Compare multiple interventions
- Make data-driven decisions
- Support policy development
- Justify resource allocation

Remember:
- Start simple and add complexity gradually
- Validate all parameters before running
- Compare multiple scenarios
- Document your analysis
- Interpret results in context
- Consult experts for important decisions

The predictions are estimates based on models trained on historical data. Use them as one input among many in your decision-making process, alongside clinical expertise, stakeholder input, available resources, and policy considerations.

---

**Last Updated**: October 31, 2025

**User Guide Version**: 1.0.0

**Application Version**: 1.0.0
