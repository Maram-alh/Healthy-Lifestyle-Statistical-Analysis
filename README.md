# Statistical Analysis of Healthy Lifestyle Factors

## 📌 Overview
This project presents a comprehensive statistical analysis of a Healthy Lifestyle dataset using R.  
The goal is to examine relationships between demographic characteristics, lifestyle behaviors, and key health indicators such as:

- Blood Pressure
- Heart Rate
- Sleep Duration
- BMI Category
- Stress Level
- Physical Activity

## 📊 Dataset Information
- 374 samples
- 13 attributes
- Mixed numeric and categorical variables

Key variables include:
- Age
- Gender
- Occupation
- Sleep Duration
- Physical Activity Level
- Stress Level
- BMI Category
- Blood Pressure
- Heart Rate
- Daily Steps
- Sleep Disorder

## 🔬 Methods Used
- Data cleaning and preprocessing
- Descriptive statistics
- Outlier detection (IQR method)
- Correlation analysis (Pearson)
- Multiple linear regression
- Conditional probability analysis
- Data visualization using ggplot2

## 📈 Key Findings
- Very strong correlation between systolic and diastolic blood pressure (r ≈ 0.97)
- Weak relationships between lifestyle factors and blood pressure
- Moderate variation in heart rate across occupations
- Doctors showed higher probability of Normal BMI compared to males overall

## 🛠 Technologies
- R
- dplyr
- ggplot2
- reshape2
- e1071

## 📂 Project Structure
- `analysis_code.R` → Full R code
- `Healthy_Lifestyle_Report.pdf` → Full report
- `healthy_lifestyle.csv` → Dataset
- `images/` → Generated visualizations

## 📚 References
- James et al. (2013). *An Introduction to Statistical Learning*. Springer.
- Hastie et al. (2004). *The Elements of Statistical Learning*. Springer.
- Fox & Weisberg (2019). *An R Companion to Applied Regression*.
