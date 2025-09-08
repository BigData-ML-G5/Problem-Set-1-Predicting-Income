# Problem-Set-1-Predicting-Income

## Summary

This study examines the determinants and prediction of individual hourly wages in Bogotá using the 2018 GEIH survey. We combine insights from labor economics —such as the Mincer wage equation, human capital theory, and the presence of concave age–earnings profiles— with predictive modeling to evaluate both econometric and machine-learning approaches. The Colombian labor market, characterized by high informality and the binding role of the minimum wage, provides a unique context where standard wage determinants interact with institutional rigidities. Our objective is not structural estimation but rather the predictive accuracy of alternative methods for modeling log hourly wages.

### Project methodology description

We construct a benchmark Mincer-type OLS model that controls for occupation, industry, firm size, and formality, and compare its performance with flexible machine-learning algorithms. We first use linear regression to estimate the age-wage profile on our database in order to evaluate the relationship betweeen both variables. Second, we use again linear regression to estimate the relationship between gender and wage, including important variables as controls. Finally, the dataset is split into 70% training and 30% testing samples (with a fixed seed for replication), and predictive performance is assessed using RMSE and MAE. For the two best-performing models, leave-one-out cross-validation (LOOCV) is conducted. We also compute variable importance metrics and investigate model performance in the distribution tails to assess whether extreme errors reflect mis-measurement, under-reporting, or structural heterogeneity.

### Conclusions

Our findings confirm several key hypotheses. The analysis reveals a statistically significant concave age-wage profile consistent with life-cycle theory, with wage level peaking around age 54, implying age is a key aspect to explain wage level, yet high variance on our model estimation strongly suggest other variables affecting wage must be considered. Furthermore, we quantify a persistent gender wage gap that, while partially explained by observable characteristics, remains significant, especially present when performing the same job. Our estimations for age-wage profile discriminated by gender suggest a similar average wage level at young age, but highly differentiated entering mid age, with women peaking around age 47, while male peaking at 60, with men having significantly higher wages. Finally, the predictive evaluation demonstrates that flexible models incorporating complex, non-linear interactions substantially outperform simpler linear specifications in terms of out-of-sample error, highlighting the importance of feature engineering for this task.

---

## Replication instructions

This project was developed in statistical programming language R. For replicability, 1st step is to set the working directory (setwd() command) to where the main folder of the project is in your computer; otherwise graphics and tables exports would throw erros. 2nd step is to run the R script named "DataDownload.r", which downloads the data set. Next, although no order is required to run the project, we advice to run "AgeWageProfile.r", "GenderEarningsGap.r" and "PredictingWages.r", as it is the order in which the final report is done.