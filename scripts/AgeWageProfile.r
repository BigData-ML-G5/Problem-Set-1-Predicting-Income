# -----------------------------------------------------
# PART 2: Age and Wage Profile
# This script is for downloading data from the workshop.
# 1) Formulate and estimate the regression model
# 2) Confidence intervals (bootstrap)
# 3) Plotting data to seek the "peak" age
# -----------------------------------------------------

# -----------------------------------------------------
# 1) Formulate and estimate the regression model
# -----------------------------------------------------

model <- lm(log(salario) ~ edad + poly(edad, 2), data = db)
summary(model)

# -----------------------------------------------------
# 2) Confidence intervals (bootstrap)
# -----------------------------------------------------

# Function that estimates the model coeficients
#TODO: paramétrico o no? hay semilla?
eta_fn<-function(data, index){

  coef(lm(log(salario) ~ edad + poly(edad, 2), data = data, subset = index))
}

# Boostraping with our data, the model's coefficients, and 1k replicates
std_errors <- boot(db, eta_fn, R=1000)
std_errors

# -----------------------------------------------------
# 3) Plotting data to seek the "peak" age
# -----------------------------------------------------


