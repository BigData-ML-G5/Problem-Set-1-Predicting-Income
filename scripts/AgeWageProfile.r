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

model <- lm(log(ingreso_por_hora) ~ age + age2, data = db)
summary(model)

# -----------------------------------------------------
# 2) Confidence intervals (bootstrap)
# -----------------------------------------------------

# Function that estimates the model coeficients
#TODO: paramétrico o no? hay semilla?
# cambiar nombre a función
eta_fn<-function(data, index){
  coef(lm(log(ingreso_por_hora) ~ age + age2, data = data, subset = index))
}

# Boostraping with our data, the model's coefficients, and 1k replicates
# TODO: no sé si me da el error toca hacer sd(std_errors)
std_errors <- boot(db, eta_fn, R=1000)
std_errors

# Confidence intervals
# Calcular intervalos de confianza usando boot.ci()
ci_t1 <- boot.ci(std_errors, type = "perc", index = 1)  # Para el intercepto
ci_t2 <- boot.ci(std_errors, type = "perc", index = 2)  # Para age
ci_t3 <- boot.ci(std_errors, type = "perc", index = 3)  # Para age2

# Imprimir los intervalos de confianza
cat("95% CI for Intercept:", ci_t1$perc[4:5], "\n")
cat("95% CI for age:", ci_t2$perc[4:5], "\n")
cat("95% CI for age2:", ci_t3$perc[4:5], "\n")

# -----------------------------------------------------
# 3) Plotting data to seek the "peak" age
# -----------------------------------------------------

# Plot the equation using model's coefficients
#TODO: queda gráfica, pero está fea. Valores en 0 tocará quitarlos
age_seq <- seq(min(db$age), max(db$age), length.out = 100)
preds <- predict(model, newdata = data.frame(age = age_seq, age2 = age_seq^2))
plot(db$age, log(db$ingreso_por_hora), pch = 16, col = rgb(0, 0, 0, 0.5), xlab = "Age", ylab = "Log(Hourly wage)")
lines(age_seq, preds, col = "blue", lwd = 4)

#Peak age
peak_age <- -coef(model)[2] / (2 * coef(model)[3])

ggplot(data = db, aes(x = age, y = log(ingreso_por_hora))) +
  geom_point(alpha = 0.3, color = "black", size = 1.5) +
  geom_line(data = data.frame(age = age_seq, age2 = age_seq^2), aes(x = age, y = preds), color = "blue", size = 1.2) +  # Línea ajustada
  labs(
    title = "Age-Wage Profile",
    x = "Age",
    y = "Log(Hourly Wage)"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.title = element_text(face = "bold")  
  ) +
  annotate("text", x = peak_age, y = max(preds), label = paste("Peak Age:", round(peak_age, 1)), vjust = -1, color = "red", size = 5)





  