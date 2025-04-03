# Alzheimer's Disease and Waste Data Analysis

## Overview
This project analyzes the correlation between Alzheimer's disease prevalence and various waste indicators using multiple datasets. It merges waste composition, health expenditure, and Alzheimer's disease prevalence data to explore potential relationships.

## Datasets
The following datasets are used in this analysis:
- **waste_data.csv**: Contains waste composition and population data.
- **alzheimer.csv**: Contains Alzheimer's disease prevalence data.
- **HEPC.csv**: Contains health expenditure per capita data.
- **HEP.csv**: Contains health expenditure as a percentage of GDP.

## Data Processing Steps
1. **Import Libraries**: Required libraries such as `ggplot2`, `randomForest`, `pdp`, `caret`, and `dplyr` are loaded.
2. **Load Data**: CSV files are read into R.
3. **Feature Engineering**: New features related to per capita waste production are created.
4. **Data Merging**: All datasets are merged on `country_name`.
5. **Data Cleaning**: NA values are removed, and outliers are filtered using the IQR method.
6. **Correlation Analysis**: A correlation matrix is generated to identify relationships between waste indicators and Alzheimer's prevalence.
7. **Statistical Testing**: Pearson correlation tests are performed for each variable, and p-values are calculated.
8. **Data Visualization**: Scatter plots with regression lines illustrate relationships between select waste indicators and Alzheimer's rates.

## Key Features
- **Data Merging & Cleaning**: Ensures data integrity and removes outliers.
- **Correlation Analysis**: Computes correlations between Alzheimer's prevalence and waste variables.
- **Statistical Testing**: Generates p-values to assess the significance of correlations.
- **Visualizations**: Creates scatter plots and formatted tables for interpretation.

## Dependencies
This project requires the following R packages:
```r
install.packages(c("readr", "ggplot2", "randomForest", "patchwork", "pdp", "formattable", "dplyr", "caret", "gt"))
```

## Usage
1. Clone the repository or download the script.
2. Ensure all dataset files are in the working directory.
3. Run the R script to execute the analysis and generate visualizations.

## Results
- Correlation values indicate potential relationships between waste composition and Alzheimer's prevalence.
- Scatter plots provide a visual representation of these relationships.
- P-values assess statistical significance, highlighting key variables of interest.

## Future Improvements
- Incorporate machine learning models to predict Alzheimer's rates based on waste indicators.
- Expand dataset coverage for a more comprehensive analysis.
- Explore additional environmental and socioeconomic factors influencing Alzheimer's prevalence.

## Author
Rohit Malavathu
rohitmalavathu@vt.edu
