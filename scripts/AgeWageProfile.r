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

model <- lm(log(ingreso_laboral_horas_actuales) ~ age + age2, data = db)
summary(model)

# -----------------------------------------------------
# 2) Confidence intervals (bootstrap)
# -----------------------------------------------------

# Function that estimates the model coeficients
set.seed(2025)
fn<-function(data, index){
  coef(lm(log(ingreso_laboral_horas_actuales) ~ age + age2, data = data, subset = index))
}

# Boostraping with our data, the model's coefficients, and 1k replicates
# TODO: no sé si me da el error toca hacer sd(std_errors)
std_errors <- boot(db, fn, R=1000)
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
age_seq <- seq(min(db$age), max(db$age), length.out = 100)
preds <- predict(model, newdata = data.frame(age = age_seq, age2 = age_seq^2))

# Data for the fitted curve
curve_data <- data.frame(age = age_seq, age2 = age_seq^2, preds = preds)

# Calculate peak age
peak_age <- -coef(model)["age"] / (2 * coef(model)["age2"])
peak_y <- predict(model, newdata = data.frame(age = peak_age, age2 = peak_age^2))
peak_data <- data.frame(age = peak_age, y = peak_y)

ggplot(data = db, aes(x = age, y = log_ingreso_laboral_horas_actuales)) +
  # puntos de la muestra (sin leyenda)
  geom_point(alpha = 0.3, color = "black", size = 1.5) +
  
  # curva ajustada
  geom_line(
    data = curve_data,
    aes(x = age, y = preds, linetype = "Fitted curve"),  # <--- usamos linetype como key
    color = "blue", size = 1.2
  ) +
  
  # punto de máximo (misma key para color y shape)
  geom_point(
    data = peak_data,
    aes(x = age, y = y, color = "Peak age", shape = "Peak age"),
    size = 5
  ) +
  
  scale_color_manual(values = c("Peak age" = "red")) +
  scale_shape_manual(values = c("Peak age" = 16)) +
  scale_linetype_manual(values = c("Fitted curve" = "solid")) +
  
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  
  annotate("text", x = peak_age, y = peak_y, 
           label = paste0("Peak age: ", round(peak_age, 1)), 
           vjust = -1, color = "red", fontface = "bold", size = 5) +
  
  labs(
    title = "Age-Wage Profile",
    x = "Age",
    y = "Log(Hourly Wage)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )





  