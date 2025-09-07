# -----------------------------------------------------
# PART 4: Predicting Wages
# This script is for downloading data from the workshop.
# 1) Split the sample into two 70% training and 30% sample and set seed 10101
# 2) Report and compare the predictive performance in terms of the RMSE of all
# the previous specifications with at least five (5) additional specifications that
# explore non-linearities and complexity
# 3) Check the the overall performance of the models,  the specification with the lowest prediction error
# and for the specification with the lowest prediction error, explore those observations that seem to ”miss 
# the mark.”
# 4) LOOCV. For the two models with the lowest predictive error
# -----------------------------------------------------

# -----------------------------------------------------
# 1) Split the sample into two 70% training and 30% sample and set seed 10101
# -----------------------------------------------------
# Validation Set Approach
setwd("/Users/gianlucacicco/Desktop/Taller 1 - BigData/Problem-Set-1-Predicting-Income/views")
db <- read.csv("db_ready.csv")

library(caret)
library(dplyr)
set.seed(10101)  # Set for replicability purposes 

##TODO: Cambiar ingreso_por_hora por la variable de ingreso_laboral_horas_actuales
## TODO: Eliminar las lineas de codigo que ELIMMINAN los missing values, esto es para poder hacer los modelos
## mientras realizamos el manejo de los missing

inTrain <- createDataPartition(
  y = db$log_ingreso_laboral_horas_actuales,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

training <- db %>% filter(row_number() %in% inTrain)
testing  <- db %>% filter(!(row_number() %in% inTrain))

# Create data for visualization
split_data <- data.frame(
  Split = factor(c("Training", "Testing")),
  Count = c(nrow(training), nrow(testing)),
  Percentage = c(nrow(training)/nrow(db)*100, nrow(testing)/nrow(db)*100)
)

# Create the visualization
ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")), 
            vjust = -0.5, color = "black", size = 4) +
  labs(title = "Train-Test Split Distribution",
       y = "Number of Observations",
       x = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # Add some space for labels
  theme_classic()

# The training and testing split distribution worked correctly

# -----------------------------------------------------
# 2) Report and compare the predictive performance in terms of the RMSE of all
# the previous specifications with at least five (5) additional specifications
# -----------------------------------------------------

# Train our models - Model 1
form_1 <- log_ingreso_laboral_horas_actuales ~ age + age2 
model1a <- lm(form_1,
               data = training)

predictions <- predict(object = model1a, newdata = testing)
score1a<- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales) # Take into account the scales, compare log with log
score1a

# Train our models - Model 2
form_2 <- log_ingreso_laboral_horas_actuales ~ hombre
model2a <- lm(form_2,
              data = training)

predictions <- predict(object = model2a, newdata = testing)
score2a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score2a

# Train our models - Model 3
form_3 <- log_ingreso_laboral_horas_actuales ~ age + age2 + estrato1 + formal + 
  tamano_empresa + maximo_nivel_educativo + tiempo_empresa_actual + 
  hoursWorkUsual + oficio + mes
model3a <- lm(form_3, data = training)

predictions <- predict(object = model3a, newdata = testing)
score3a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score3a

# 5 more models with  additional specifications that explore non-linearities and complexity
# Train our models - Model 4
form_4 <- log_ingreso_laboral_horas_actuales ~ poly(age,3,raw=TRUE) +
  maximo_nivel_educativo + I(maximo_nivel_educativo^2) +
  hombre + estrato1 +
  poly(age,3,raw=TRUE):maximo_nivel_educativo +
  poly(age,3,raw=TRUE):hombre +
  maximo_nivel_educativo:hombre +
  estrato1:maximo_nivel_educativo + oficio + mes
model4a <- lm(form_4, data = training)
predictions <- predict(object = model4a, newdata = testing)
score4a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score4a

# Train our models - Model 5: Labor market structure interactions
form_5 <- log_ingreso_laboral_horas_actuales ~ poly(age,3,raw=TRUE) +
  maximo_nivel_educativo + hombre + formal + tamano_empresa +
  hoursWorkUsual + tiempo_empresa_actual +
  poly(age,3,raw=TRUE):formal +
  poly(age,3,raw=TRUE):tamano_empresa +
  formal:tamano_empresa + formal:hombre +
  hoursWorkUsual:tamano_empresa + 
  tiempo_empresa_actual:formal + oficio + mes
model5a <- lm(form_5, data = training)
predictions <- predict(object = model5a, newdata = testing)
score5a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score5a

# Train our models - Model 6: Household and demographic interactions
form_6 <- log_ingreso_laboral_horas_actuales ~ poly(age,3,raw=TRUE) +
  maximo_nivel_educativo + hombre + num_minors + bin_head +
  bin_headFemale + estrato1 + hoursWorkUsual +
  poly(age,3,raw=TRUE):num_minors +
  hombre:num_minors + hombre:bin_head +
  estrato1:num_minors + estrato1:hombre +
  hoursWorkUsual:num_minors + bin_headFemale:estrato1 + oficio + mes
model6a <- lm(form_6, data = training)
predictions <- predict(object = model6a, newdata = testing)
score6a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score6a

# Train our models - Model 7: Complex non-linear interactions
form_7 <- log_ingreso_laboral_horas_actuales ~ poly(age,4,raw=TRUE) +
  poly(maximo_nivel_educativo,2,raw=TRUE) + 
  poly(estrato1,2,raw=TRUE) + hombre + formal + tamano_empresa +
  poly(age,3,raw=TRUE):poly(maximo_nivel_educativo,2,raw=TRUE) +
  poly(age,3,raw=TRUE):hombre:formal +
  poly(estrato1,2,raw=TRUE):tamano_empresa +
  hombre:poly(maximo_nivel_educativo,2,raw=TRUE) + factor(oficio) + mes
model7a <- lm(form_7, data = training)
predictions <- predict(object = model7a, newdata = testing)
score7a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score7a

# Train our models - Model 8: Kitchen sink with high-order interactions
form_8 <- log_ingreso_laboral_horas_actuales ~ poly(age,3,raw=TRUE) +
  maximo_nivel_educativo + hombre + formal + tamano_empresa +
  estrato1 + num_minors + hoursWorkUsual +
  poly(age,3,raw=TRUE):hombre +
  maximo_nivel_educativo:hombre + 
  formal:hombre + formal:tamano_empresa + 
  estrato1:maximo_nivel_educativo +
  num_minors:hombre + oficio + mes
model8a <- lm(form_8, data = training)
predictions <- predict(object = model8a, newdata = testing)
score8a <- RMSE(pred = predictions, obs = testing$log_ingreso_laboral_horas_actuales)
score8a

# -----------------------------------------------------
# 3) Check the the overall performance of the models,  the specification with the lowest prediction error
# and for the specification with the lowest prediction error, explore those observations that seem to ”miss 
# the mark.”
# -----------------------------------------------------

# This matter was addressed and resolved in the final report.
tabla1 <- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7, 8),
                      RMSE_vsa = c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a)
)

# -----------------------------------------------------
# 4) LOOCV. For the two models with the lowest predictive error
# -----------------------------------------------------
# Using the linear algebra features of R we can calculate this without running the model N
# times, as LOOCV intends. When the data is large, this can save us some computation time. The 
# two models with the lowest RMSE are the modelX (formXX) and modelX (formXX).

# Model7
# RUN THE MODEL WITH ALL OBS

full_model1 <- lm(form_7, data = db)
X1 <- model.matrix(full_model1)
y1 <- model.response(model.frame(full_model1))
beta_hat1 <- full_model1$coefficients
G_inv1 <- MASS::ginv(t(X1) %*% X1) # Calculate the inverse of  (X'X)
vec1 <- 1/(1-hatvalues(full_model1)) # and 1/1-hi
N1 <- nrow(X1) # Number of observations
LOO1 <- numeric(N1) # To store the errors

for (i in 1:N1) {
  new_beta1 <- beta_hat1 - vec1[i] * G_inv1 %*% as.vector(X1[i, ]) * full_model1$residuals[i] # get the new beta
  new_error1 <- (y1[i] - (X1[i, ] %*% new_beta1))^2  # get the new error
  LOO1[i] <- new_error1
}
looCV_error1 <- mean(LOO1)
rmse_model1 <- sqrt(looCV_error1)

# Model8
# RUN THE MODEL WITH ALL OBS

full_model2 <- lm(form_8, data = db)
X2 <- model.matrix(full_model2)
y2 <- model.response(model.frame(full_model2))
beta_hat2 <- full_model2$coefficients
G_inv2 <- MASS::ginv(t(X2) %*% X2)
vec2 <- 1/(1-hatvalues(full_model2))
N2 <- nrow(X2)
LOO2 <- numeric(N2)

for (i in 1:N2) {
  new_beta2 <- beta_hat2 - vec2[i] * G_inv2 %*% as.vector(X2[i, ]) * full_model2$residuals[i]
  new_error2 <- (y2[i] - (X2[i, ] %*% new_beta2))^2
  LOO2[i] <- new_error2
}
looCV_error2 <- mean(LOO2)
rmse_model2 <- sqrt(looCV_error2)

# Store the RMSE in dataframe (table)
scores2 <- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7, 8),
                     RMSE_vsa = c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a),
                     RMSE_loocv = c(rmse_model1, rmse_model2, 0, 0, 0, 0, 0, 0)
)

head(scores2)

# Graph
RMSE_vsa   <- c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a) 
RMSE_loocv <- c(rmse_model1, rmse_model2)

scores <- data.frame(
  Model = c(1:8, 1:2),  # Modelos 1-8 para VSA, 1-2 para LOOCV
  Approach = c(rep("Validation", 8), rep("LOOCV", 2)),
  RMSE = c(RMSE_vsa, RMSE_loocv)
)

comparison_plot <- ggplot(scores, aes(x = Model, y = RMSE, color = Approach)) + 
  geom_line(size = 0.5) + 
  geom_point() +
  labs(title = "Comparison of RMSE Across Different Validation Approaches",
       subtitle = "Validation Set Approach vs Leave-One-Out Cross-Validation",
       x = "Model Number",
       y = "Root Mean Square Error (RMSE)") +
  theme_bw() +
  scale_x_continuous(breaks = 1:8)

# Save the plot
setwd("/Users/gianlucacicco/Desktop/Taller 1 - BigData/Problem-Set-1-Predicting-Income/views")
ggsave("views/rmse_comparison.png", comparison_plot, 
       width = 10, height = 6, dpi = 300)

# Remember: The validation estimate can be highly variable depending on which observations 
# are included in the training set and which are included in the validation set. This issue is clearly mitigated 
# when we use K-Fold and/or LOOCV.
