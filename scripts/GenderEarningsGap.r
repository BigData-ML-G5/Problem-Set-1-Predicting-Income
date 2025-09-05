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