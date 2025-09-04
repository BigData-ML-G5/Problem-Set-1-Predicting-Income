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
# cambiar nombre a función
eta_fn<-function(data, index){
  coef(lm(log(salario) ~ edad + poly(edad, 2), data = data, subset = index))
}

# Boostraping with our data, the model's coefficients, and 1k replicates
# TODO: no sé si me da el error toca hacer sd(std_errors)
std_errors <- boot(db, eta_fn, R=1000)
std_errors

# -----------------------------------------------------
# 3) Plotting data to seek the "peak" age
# -----------------------------------------------------

# Plot the equation using model's coefficients
#TODO: revisar resultado con datos. Debería quedar una cuadrática en la gráfica
age_seq <- seq(min(db$edad), max(db$edad), length.out = 100)
preds <- predict(model, newdata = data.frame(edad = age_seq))
plot(db$edad, log(db$salario), pch = 16, col = rgb(0, 0, 0, 0.5), xlab = "Age", ylab = "Log(Salary)")
lines(age_seq, preds, col = "blue", lwd = 2)
