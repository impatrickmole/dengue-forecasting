# 📊 Dengue Outbreak Forecasting in Pangasinan using ARIMA (R Project)

Dengue fever remains a critical public health concern in tropical countries like the Philippines, with Pangasinan frequently experiencing outbreaks due to favorable environmental conditions for mosquito breeding. Despite ongoing prevention and control initiatives, predicting dengue surges remains challenging.

This project applies **data analytics and time-series forecasting** using the **ARIMA model in R** to analyze historical dengue cases and forecast potential future outbreaks in Pangasinan. The objective is to support **early warning systems**, guide local health response strategies, and provide data-driven insights for dengue prevention programs.

---

## ✅ Project Objectives
- Conduct **Exploratory Data Analysis (EDA)** of dengue cases in Pangasinan (2019–2024)
- Identify trends, seasonal patterns, and high-risk periods
- Train and validate ARIMA time-series models using R
- Forecast future dengue outbreaks
- Visualize findings through graphs and analytical plots

---

## 🛠️ Tools & Technologies
| Category | Tools |
|--------|-------|
Programming Language | **R**
Libraries Used | `ggplot2`, `dplyr`, `forecast`, `ggpattern`
Dataset | Dengue case data from PESU Pangasinan (2019–2024)
Visualization | Time-series plots, boxplots, and distribution charts

---

## 📁 Project Structure
```
├── data/
│   └── cleaned_pangasinan_dengue_cases_2019_2024.csv
├── scripts/
│   └── dengue_forecasting_arima.R
├── outputs/
│   ├── forecast_plot.png
│   └── boxplot_yearly_distribution.png
└── README.md
```

---

## ▶️ How to Run

### **1️⃣ Install Required Packages**
```r
install.packages(c("ggplot2", "dplyr", "forecast", "ggpattern"))
```

### **2️⃣ Load the dataset**
Make sure the dataset is in the `data` folder.

```r
dengue_data <- read.csv("data/cleaned_pangasinan_dengue_cases_2019_2024.csv")
```

### **3️⃣ Run the forecasting script**
Execute the `dengue_forecasting_arima.R` file in RStudio or R console.

---

## 📈 Sample Output Plots
- Yearly dengue distribution boxplot (2019–2024)
- ARIMA forecast curve for future dengue cases
- Monthly seasonal trend visualization

---

## 📌 Notes
- Dataset used is obtained officially from **Provincial Epidemiology & Surveillance Unit (PESU) Pangasinan**
- ARIMA model selected based on ACF/PACF and AIC values
- Forecast accuracy measured using **MAPE & RMSE**

---

## 📚 Citation
If you use this project or code, please cite:

**Mole, Patrick V. (2026)**  
*Exploratory Data Analysis and Forecasting of Dengue Outbreaks in Pangasinan Using ARIMA Model.*  
Accepted for publication in **IJ-ICT (Scopus-Indexed)** | Expected Release: **April 2026**

---

## 🙏 Acknowledgment
Special thanks to **PESU Pangasinan** for providing the dengue surveillance dataset used in this study.

---

## 📬 Contact
📧 **patrickmole@ucu.edu.ph**  
🏫 Urdaneta City University
