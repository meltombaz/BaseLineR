# BaseLineR
An interactive Shiny application for generating publication-ready baseline characteristics tables from clinical or research datasets.

https://mlktombaz.shinyapps.io/BaseLineR/

# Baseline Characteristics Table Builder

This R Shiny app lets you upload patient-level data (TXT, CSV, Excel), select a grouping variable, and build a baseline characteristics table by group. It automatically detects variable types and applies appropriate statistical tests:

- **Categorical variables**: Chi-square test (or Fisher’s exact test if expected counts < 5)
- **Numeric variables**: t-test/ANOVA if normal by Shapiro-Wilk; Wilcoxon/Kruskal-Wallis otherwise

You can download the results as a CSV.

---

## Features

- **File upload** (CSV, TXT, Excel)
- **Automatic variable type detection** Under Advanced Controls, you can select the variable type.
- **Filter data**
- **Statistical testing by group**
- **Summary stats** (`mean ± SD` or `median [IQR]`)
- **Downloadable results**
- **Preview uploaded data**

---

## How to Use

1. **Upload your data file**: Accepts `.csv`, `.txt`, `.xls`, or `.xlsx`. For Excel, you can select the sheet.
2. **Select grouping column**: The variable by which to stratify the baseline table.
3. **Select variables to include**: Choose which columns to summarize.
4. **Adjust settings** (optional):
   - Threshold for treating numeric as categorical (`unique ≤`)
   - Alpha for Shapiro-Wilk normality test
5. **Click “Build Table”**: View the baseline characteristics table, complete with appropriate statistical tests and summaries.
6. **Download**: Export the results as CSV.

---

## Deployment

### Quick Start (Local)

1. Save `app.R` from this repository.
2. Open RStudio, set working directory to the folder containing `app.R`.
3. Run:

```r
shiny::runApp("app.R")
```


```
## Example Data

You can test the app with the following sample CSV:

```csv
ID,Group,Age,Sex,BMI,Smoker
1,A,34,M,25.1,No
2,A,29,F,21.8,No
3,B,42,F,27.3,Yes
4,B,37,M,24.5,No
5,A,50,F,30.2,Yes
6,B,31,M,23.4,No
```
## Contact

Issues and suggestions welcome!
