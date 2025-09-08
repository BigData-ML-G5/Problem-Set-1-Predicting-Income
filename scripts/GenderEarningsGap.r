# -----------------------------------------------------
# PART 3: Gender Earnings Gap
# This script is for downloading data from the workshop.
# 1) Estimating the unconditional wage gap
# 2) Conditional earnings gap with worker and job controls
#   2.1) with FWL
#   2.2) with FWL and bootstrap
#   2.3) Table: Wage GAP
# 3) Plot predicted age-wage profile with confidence intervals by gender.
#   3.1) Estimate age-wage profile
#   3.2) Estimate peak ages
#   3.3) Estimate confidence intervals
# -----------------------------------------------------

# -----------------------------------------------------
# 1) Estimating the unconditional wage gap
# -----------------------------------------------------

# Model estimation (keep Y in levels; you'll switch to log later if desired)
wage_gap_model <- lm(log_ingreso_laboral_horas_actuales ~ hombre, data = db)
summary(wage_gap_model)

# -----------------------------------------------------
# 2.1) Conditional earnings gap with worker and job controls (FWL)
# -----------------------------------------------------

# ===============================
# FWL with controls + fixed effects
# ===============================

db$age2 <- db$age^2

# Define controls and fixed effects
controls      <- c("age","estrato1","formal","tamano_empresa",
                   "maximo_nivel_educativo","tiempo_empresa_actual","hoursWorkUsual","age2")
fixed_effects <- c("oficio","mes")

yvar <- "log_ingreso_laboral_horas_actuales"
dvar <- "hombre"

# Use a common sample
vars_needed <- unique(c(yvar, dvar, controls, fixed_effects))
df <- na.omit(db[, vars_needed])

# Build RHS = controls + FE (categoricals as factors; FE as factors)
cat_ctrl <- intersect(c("estrato1","tamano_empresa","maximo_nivel_educativo"), controls)
num_ctrl <- setdiff(controls, cat_ctrl)
rhs_terms <- c(num_ctrl, paste0("factor(", cat_ctrl, ")"), paste0("factor(", fixed_effects, ")"))
rhs <- paste(rhs_terms, collapse = " + ")

# Residualize Y on controls + FE  (Y in levels)
form_y <- as.formula(paste(yvar, " ~ ", rhs))
residuals_y <- resid(lm(form_y, data = df))

# Residualize X (hombre) on the same controls + FE
form_x <- as.formula(paste(dvar, " ~ ", rhs))
residuals_x <- resid(lm(form_x, data = df))

# Regress residualized Y on residualized X (FWL)
fwl_model <- lm(residuals_y ~ residuals_x)
summary(fwl_model)


# -----------------------------------------------------
# 2.2) Conditional earnings gap with worker and job controls (FWL and bootstrap)
# -----------------------------------------------------

#FWL estimation using 2.1)
ry <- resid(lm(as.formula(paste(yvar, " ~ ", rhs)), data = df))
rx <- resid(lm(as.formula(paste(dvar, " ~ ", rhs)), data = df))
fwl_model <- lm(ry ~ rx)
beta_fwl   <- coef(fwl_model)[["rx"]]
se_fwl_ols <- coef(summary(fwl_model))["rx","Std. Error"]

#Non-parametric bootstrap
fwl_boot_fn <- function(data, index){
  bd <- data[index, ]
  ry_b <- resid(lm(as.formula(paste(yvar, " ~ ", rhs)), data = bd))
  rx_b <- resid(lm(as.formula(paste(dvar, " ~ ", rhs)), data = bd))
  coef(lm(ry_b ~ rx_b))[2]
}

set.seed(10101)
b_np <- boot(df, statistic = fwl_boot_fn, R = 1000)
est_np <- b_np$t0
se_np  <- sd(b_np$t)

# CI
boot.ci(b_np, type = "perc", index = 1)


#Step 3: Comparisons
comparison <- data.frame(
  method   = c("FWL OLS", "FWL Bootstrap (pairs)"),
  estimate = c(beta_fwl,  est_np),
  se       = c(se_fwl_ols, se_np)
)
comparison

# ------------------------------
# 2.3) Table: wage gap
# ------------------------------

# ===============================
# Table 1: Wage gap (Female) — LaTeX (sin paquetes)
# ===============================

# Female = 1 - hombre
if (!"female" %in% names(db)) db$female <- 1L - db$hombre
if (exists("df") && !"female" %in% names(df)) df$female <- 1L - df$hombre

# 1) Unconditional OLS
dfA  <- na.omit(db[, c(yvar, "female")])
modA <- lm(as.formula(paste(yvar, "~ female")), data = dfA)

# 2) FWL with controls and FE
modB <- lm(as.formula(paste(yvar, " ~ female + ", rhs)), data = df)

# 3) FWL with non-parametric bootstrap

if (!exists("beta_fwl") || !exists("se_np")) {
  ry <- resid(lm(as.formula(paste(yvar, " ~ ", rhs)), data = df))
  rx <- resid(lm(as.formula(paste("hombre ~ ", rhs)), data = df))
  fwl_model <- lm(ry ~ rx)
  beta_fwl  <- coef(fwl_model)[["rx"]]
  se_np     <- sd(boot::boot(df, statistic = function(data, index){
    bd  <- data[index, ]
    ryb <- resid(lm(as.formula(paste(yvar, " ~ ", rhs)), data = bd))
    rxb <- resid(lm(as.formula(paste("hombre ~ ", rhs)), data = bd))
    coef(lm(ryb ~ rxb))[2]
  }, R = 1000)$t)
}
# We want to report female coefficient in the table so we have to adjust the results as:

betaC <- -as.numeric(beta_fwl)
seC   <-  as.numeric(se_np)
pC    <- 2 * pnorm(abs(betaC / seC), lower.tail = FALSE)

# Numbers to put in the table

coefA <- coef(modA)["female"]; seA <- coef(summary(modA))["female","Std. Error"]; pA <- coef(summary(modA))["female","Pr(>|t|)"]
coefB <- coef(modB)["female"]; seB <- coef(summary(modB))["female","Std. Error"]; pB <- coef(summary(modB))["female","Pr(>|t|)"]

nA <- nobs(modA); nB <- nobs(modB)
r2A <- summary(modA)$r.squared; r2B <- summary(modB)$r.squared

fmt   <- function(x) formatC(x, format = "f", digits = 3)
stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))

# Table with coefficients and Standard Errors in Latex

if (!dir.exists("views")) dir.create("views", recursive = TRUE)

tex <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Wage gap (Female)}\\label{tab:wagegap}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "\\multicolumn{4}{c}{\\textit{Dependent variable: log wage}}\\\\[4pt]",
  " & (A) OLS (unconditional) & (B) FWL with controls (FE) & (C) FWL with controls (bootstrap) \\\\",
  "\\midrule",
  paste0("Female & ", fmt(coefA), stars(pA), " & ", fmt(coefB), stars(pB), " & ", fmt(betaC), stars(pC), " \\\\"),
  paste0(" & (", fmt(seA), ") & (", fmt(seB), ") & (", fmt(seC), ") \\\\"),
  "\\midrule",
  paste0("N & ", nA, " & ", nB, " & ", nB, " \\\\"),
  paste0("R$^2$ & ", fmt(r2A), " & ", fmt(r2B), " & ", fmt(r2B), " \\\\"),
  "FE & No & Yes & Yes \\\\",
  "\\bottomrule",
  "\\multicolumn{4}{l}{\\footnotesize Notes: Column (B) estimates the Female coefficient using the Frisch--Waugh--Lovell (FWL)} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize approach with worker/job controls (age, age$^2$, formal, estrato, firm size, max education, tenure,} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize usual hours) and fixed effects for occupation and month. Column (C) reports the same estimate as (B)} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize with nonparametric bootstrap standard errors (R = 1000).} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex, con = "views/table_wage_gap_main.tex")


# -----------------------------------------------------
# 3) Plot predicted age-wage profile with confidence intervals
# -----------------------------------------------------

# -----------------------------------------------------
# 3.1) Estimate age-wage profile
# -----------------------------------------------------

# Age grid in common support
men   <- subset(db, hombre == 1)
women <- subset(db, hombre == 0)

age_min <- max(min(men$age, na.rm=TRUE),   min(women$age, na.rm=TRUE))
age_max <- min(max(men$age, na.rm=TRUE),   max(women$age, na.rm=TRUE))
age_seq <- seq(age_min, age_max, by = 1)

# Models by gender 
m_men   <- lm(log_ingreso_laboral_horas_actuales ~ age + I(age^2), data = men)
m_women <- lm(log_ingreso_laboral_horas_actuales ~ age + I(age^2), data = women)

# Point predictions on the grid 
p_men_fit   <- predict(m_men,   newdata = data.frame(age = age_seq))
p_women_fit <- predict(m_women, newdata = data.frame(age = age_seq))

# Bootstrap for plot (predictions on the grid)
pred_fun <- function(data, index){
  d <- data[index, ]
  fit <- lm(log_ingreso_laboral_horas_actuales ~ age + I(age^2), data = d)
  predict(fit, newdata = data.frame(age = age_seq))
}

set.seed(123)
b_men_pred   <- boot::boot(men,   statistic = pred_fun, R = 1000)
b_women_pred <- boot::boot(women, statistic = pred_fun, R = 1000)

# Bands
ci_men   <- apply(b_men_pred$t,   2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
ci_women <- apply(b_women_pred$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

# Data frames
pred_men <- data.frame(
  age = age_seq,
  fit = as.numeric(p_men_fit),
  lwr = as.numeric(ci_men[1, ]),
  upr = as.numeric(ci_men[2, ]),
  sexo = "Men"
)

pred_women <- data.frame(
  age = age_seq,
  fit = as.numeric(p_women_fit),
  lwr = as.numeric(ci_women[1, ]),
  upr = as.numeric(ci_women[2, ]),
  sexo = "Women"
)

predictions <- rbind(pred_men, pred_women)
predictions$sexo <- factor(predictions$sexo, levels = c("Men","Women"))

# Plot the predicted age-wage profiles
ggplot(predictions, aes(x = age, y = fit, color = sexo, fill = sexo)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, linewidth = 0) +
  labs(
    title = "Predicted Age–Wage Profiles by Gender",
    x = "Age",
    y = "log(wage)",
    color = "Gender",
    fill  = "Gender"
  ) +
  theme_minimal()

# -----------------------------------------------------
# 3.2) Estimate peak ages
# -----------------------------------------------------

# Peak ages by gender (point estimates)
peak_men   <- -coef(m_men)["age"]   / (2 * coef(m_men)["I(age^2)"])
peak_women <- -coef(m_women)["age"] / (2 * coef(m_women)["I(age^2)"])

# Prediction in log at the peak (for annotation)
y_peak_men   <- as.numeric(predict(m_men,   newdata = data.frame(age = peak_men)))
y_peak_women <- as.numeric(predict(m_women, newdata = data.frame(age = peak_women)))

# -----------------------------------------------------
# 3.3) Estimate confidence intervals (bootstrap, non-parametric)
# -----------------------------------------------------

# Bootstrap of peak ages (FIX: no double log)
peak_fun <- function(data, index){
  d  <- data[index, ]
  cf <- coef(lm(log_ingreso_laboral_horas_actuales ~ age + I(age^2), data = d))
  -cf["age"] / (2 * cf["I(age^2)"])
}

set.seed(123)
b_peak_men   <- boot::boot(men,   statistic = peak_fun, R = 1000)
b_peak_women <- boot::boot(women, statistic = peak_fun, R = 1000)

ci_peak_men   <- boot::boot.ci(b_peak_men,   type = "perc", index = 1)$perc[4:5]
ci_peak_women <- boot::boot.ci(b_peak_women, type = "perc", index = 1)$perc[4:5]

#Difference in peak ages
diff_draws <- as.numeric(b_peak_men$t) - as.numeric(b_peak_women$t)
est_diff   <- as.numeric(peak_men - peak_women)
ci_diff    <- quantile(diff_draws, c(0.025, 0.975), na.rm = TRUE)

# Peak ages table (Women / Men) and export to LaTeX 

peak_tab <- data.frame(
  gender   = c("Women","Men"),
  peak_age = c(peak_women, peak_men),
  CI_low   = c(ci_peak_women[1], ci_peak_men[1]),
  CI_high  = c(ci_peak_women[2], ci_peak_men[2])
)

fmt1 <- function(x) formatC(x, format = "f", digits = 1)
if (!dir.exists("views")) dir.create("views", recursive = TRUE)

tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Peak ages by gender (bootstrap 95\\% CIs)}\\label{tab:peakages}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & Peak age & CI$_{low}$ & CI$_{high}$ \\\\",
  "\\midrule",
  paste0("Women & ", fmt1(peak_tab$peak_age[1]), " & ", fmt1(peak_tab$CI_low[1]), " & ", fmt1(peak_tab$CI_high[1]), " \\\\"),
  paste0("Men & ",    fmt1(peak_tab$peak_age[2]), " & ", fmt1(peak_tab$CI_low[2]), " & ", fmt1(peak_tab$CI_high[2]), " \\\\"),
  "\\midrule",
  paste0("Men $-$ Women & ", fmt1(est_diff), " & ", fmt1(ci_diff[1]), " & ", fmt1(ci_diff[2]), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{flushleft}\\footnotesize",
  "Notes: Peaks from quadratic log-wage models by gender. CIs are nonparametric percentile bootstrap ($R=1000$). ",
  "Positive values in the difference row indicate that men's peak age is higher than women's.",
  "\\end{flushleft}",
  "\\end{table}"
)

writeLines(tex, "views/table_peak_ages.tex")

# -----------------------------------------------------
# 3.4) Plot with peak ages by gender
# -----------------------------------------------------

#Define data frames for the plot

peaks_df <- data.frame(
  sexo   = c("Men","Women"),
  age    = c(peak_men, peak_women),
  y_fit  = c(y_peak_men, y_peak_women),
  ci_low = c(ci_peak_men[1],   ci_peak_women[1]),
  ci_high = c(ci_peak_men[2],  ci_peak_women[2])
)

predictions$sexo <- factor(predictions$sexo, levels = c("Men","Women"))
peaks_df$sexo    <- factor(peaks_df$sexo,    levels = c("Men","Women"))

# Colors
base_cols <- c(Men = "#2C7FB8", Women = "#E34A33")
peak_cols <- c(Men = "#0B4F8A", Women = "#A11B13")

peaks_df$col <- peak_cols[as.character(peaks_df$sexo)]

ggplot(predictions, aes(x = age, y = fit, color = sexo, fill = sexo)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, linewidth = 0) +
  geom_vline(data = peaks_df, aes(xintercept = age, color = sexo),
             linetype = "dashed", alpha = 0.7, show.legend = FALSE) +
  geom_point(data = peaks_df, aes(x = age, y = y_fit),
             inherit.aes = FALSE, color = peaks_df$col, size = 3) +
  geom_text(data = peaks_df,
            aes(x = age, y = y_fit, label = paste0("Peak: ", round(age, 1))),
            inherit.aes = FALSE, color = peaks_df$col,
            vjust = -1.2, size = 3.6, fontface = "bold") +
  scale_color_manual(values = base_cols) +
  scale_fill_manual(values  = base_cols) +
  labs(
    title = "Predicted Age–Wage Profiles by Gender (with peak ages)",
    x = "Age", y = "log(wage)", color = "Gender", fill = "Gender"
  ) +
  theme_minimal()

# Export plot
name <- "Predicted-Age-Wage profiles by gender"
link <- paste0("views/", name, ".png")
ggsave(link, plot = last_plot(), width = 8, height = 6, dpi = 300, bg = "white")