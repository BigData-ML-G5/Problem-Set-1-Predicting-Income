# -----------------------------------------------------
# PART 2: Gender Earnings Gap
# This script is for downloading data from the workshop.
# 1) Estimating the unconditional wage gap
# 2) Conditional earnings gap with worker and job controls
#   2.1) with FWL
#   2.2) with FWL and bootstrap
# 3) Plot predicted age-wage profile with confidence intervals by gender.
#   3.1) Estimate age-wage profile
#   3.2) Estimate peak ages
#   3.3) Estimate confidence intervals
# -----------------------------------------------------

# -----------------------------------------------------
# 1) Estimating the unconditional wage gap
# -----------------------------------------------------

# Model estimation
wage_gap_model <- lm(log(salario) ~ sexo, data = db)
summary(wage_gap_model)

# -----------------------------------------------------
# 2.1) Conditional earnings gap with worker and job controls (FWL)
# -----------------------------------------------------

# Define variables for control and fixed effects
#TODO: cambiar controles y FE
controls <- c("edad", "edad2", "educ", "educ2", "experiencia", "experiencia2")
fixed_effects <- c("ocupacion", "industria", "region")

# FWL approach
# Step 1: Partial out controls from log(salario)
residuals_y <- lm(log(salario) ~ ., data = db[, c("log(salario)", controls)])$residuals

# Step 2: Partial out controls from sexo
residuals_x <- lm(sexo ~ ., data = db[, c("sexo", controls)])$residuals

# Step 3: Regress residuals_y on residuals_x
fwl_model <- lm(residuals_y ~ residuals_x)
summary(fwl_model)


# Fixed effects 
# TODO: ver cómo meter estos FE en el modelo de FWL
controls <- c("edad", "edad2", "educ", "educ2", "experiencia", "experiencia2", "ocupacion", "industria", "region")
formula <- as.formula(paste("log(salario) ~ sexo |", paste(controls, collapse = " + ")))
conditional_wage_gap_model <- felm(formula, data = db)

# -----------------------------------------------------
# 2.2) Conditional earnings gap with worker and job controls (FWL and bootstrap)
# -----------------------------------------------------

# FWL function for bootstrapping
fwl_boot_fn <- function(data, index) {
  # Subset the data using the bootstrap index
  boot_data <- data[index, ]
  
  # Step 1: Partial out controls from log(salario)
  fmla_y <- as.formula(paste("log(salario) ~", paste(controls, collapse = " + ")))
  residuals_y <- lm(fmla_y, data = boot_data)$residuals
  
  # Step 2: Partial out controls from sexo
  fmla_x <- as.formula(paste("sexo ~", paste(controls, collapse = " + ")))
  residuals_x <- lm(fmla_x, data = boot_data)$residuals
  
  # Step 3: Regress residuals_y on residuals_x and return the coefficient for residuals_x
  fwl_model <- lm(residuals_y ~ residuals_x)
  return(coef(fwl_model)["residuals_x"])
}

# Bootstrap
# TODO: paramétrico o no? hay semilla?
std_errors <- boot(db, eta_fn, R=1000)
std_errors

# Confidence intervals
# TODO: por qué type perc? revisar
intervals <- boot.ci(std_errors, type = "perc", index = 2) # index

# -----------------------------------------------------
# 3) Plot predicted age-wage profile with confidence intervals
# -----------------------------------------------------

# -----------------------------------------------------
# 3.1) Estimate age-wage profile
# -----------------------------------------------------

# Fit separate models for men and women
# TODO: revisar el valor de las dummys
model_men <- lm(log(salario) ~ edad + poly(edad, 2), data = db %>% filter(sexo == 0))
model_women <- lm(log(salario) ~ edad + poly(edad, 2), data = db %>% filter(sexo == 1))

# Generate age sequence so we can estimate it
# TODO: min debe ser minoría edad, max revisar
age_seq <- seq(min(db$edad), max(db$edad), by = 1)

# Predict wages for men
pred_men <- predict(model_men, newdata = data.frame(edad = age_seq), interval = "confidence")
pred_men <- data.frame(edad = age_seq, pred_men)

# Predict wages for women
pred_women <- predict(model_women, newdata = data.frame(edad = age_seq), interval = "confidence")
pred_women <- data.frame(edad = age_seq, pred_women)

# Combine predictions into a single data frame
predictions <- bind_rows(
  pred_men %>% mutate(sexo = "Men"),
  pred_women %>% mutate(sexo = "Women")
)

# Plot the predicted age-wage profiles
ggplot(predictions, aes(x = edad, y = fit, color = sexo, fill = sexo)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(
    title = "Predicted Age-Wage Profiles by Gender",
    x = "Age",
    y = "Log(Wage)",
    color = "Gender",
    fill = "Gender"
  ) +
  theme_minimal()

# -----------------------------------------------------
# 3.2) Estimate peak ages
# -----------------------------------------------------

# TODO: Quiero probar como sirve, pero podría solo sacar max de preds

# Function to calculate peak age
calculate_peak_age <- function(model) {
  coef_model <- coef(model)
  peak_age <- -coef_model["edad"] / (2 * coef_model["poly(edad, 2)2"])
  return(peak_age)
}

# Calculate peak ages for men and women
peak_age_men <- calculate_peak_age(model_men)
peak_age_women <- calculate_peak_age(model_women)

cat("Peak Age for Men:", peak_age_men, "\n")
cat("Peak Age for Women:", peak_age_women, "\n")

# -----------------------------------------------------
# 3.3) Estimate confidence intervals
# -----------------------------------------------------

# Bootstrap function for peak age
peak_age_boot_fn <- function(data, index, gender) {
  boot_data <- data[index, ]
  model <- lm(log(salario) ~ edad + I(edad^2), data = boot_data %>% filter(sexo == gender))
  return(calculate_peak_age(model))
}

# Bootstrap for men
set.seed(123)
boot_men <- boot(db, function(data, index) peak_age_boot_fn(data, index, gender = 0), R = 1000)

# Bootstrap for women
set.seed(123)
boot_women <- boot(db, function(data, index) peak_age_boot_fn(data, index, gender = 1), R = 1000)

# Confidence intervals for peak ages
ci_men <- boot.ci(boot_men, type = "perc")
ci_women <- boot.ci(boot_women, type = "perc")

cat("95% CI for Peak Age (Men):", ci_men$percent[4:5], "\n")
cat("95% CI for Peak Age (Women):", ci_women$percent[4:5], "\n")