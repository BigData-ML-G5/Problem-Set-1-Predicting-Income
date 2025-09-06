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
library(caret)
library(dplyr)
set.seed(10101)  # Set for replicability purposes 

##TODO: Cambiar ingreso_por_hora por la variable de ingreso_laboral_horas_actuales
## TODO: Eliminar las lineas de codigo que ELIMMINAN los missing values, esto es para poder hacer los modelos
## mientras realizamos el manejo de los missing

summary(training$ingreso_por_hora)

## Este es el que se deberia eliminar!!!!
db <- db %>%
  filter(!is.na(ingreso_por_hora) & ingreso_por_hora > 0)

inTrain <- createDataPartition(
  y = db$ingreso_por_hora,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

training <- db %>% filter(row_number() %in% inTrain)
testing  <- db %>% filter(!(row_number() %in% inTrain))

training <- training %>% mutate(age2 = age^2)

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
form_1 <- log(ingreso_por_hora) ~ age + age2 
model1a <- lm(form_1,
               data = training)

predictions <- predict(object = model1a, newdata = testing)
score1a<- RMSE(pred = predictions, obs = log(testing$ingreso_por_hora)) # Take into account the scales, compare log with log
score1a

# Train our models - Model 2
form_2 <- log(ingreso_por_hora) ~ gender
model2a <- lm(form_2,
              data = training)

predictions <- predict(object = model2a, newdata = testing)
score2a <- RMSE(pred = predictions, obs = log(testing$ingreso_por_hora))
score2a

# Train our models - Model 3
form_3 <- log(ingreso_por_hora) ~ gender + age + age2 + maximo_nivel_educativo + (maximo_nivel_educativo*maximo_nivel_educativo) 
+ experiencia + experiencia2 + ocupacion 
+ industria + region
model3a <- lm(form_3, data = training)

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, log(testing$totalHoursWorked))
score3a

# 5 more models with  additional specifications that explore non-linearities and complexity

form_4 <- totalHoursWorked ~ log_ingtot + poly(age,3,raw=TRUE) +
  bin_male + poly(age,3,raw=TRUE):bin_male  +
  maxEducLevel + poly(age,3,raw=TRUE):maxEducLevel  +
  num_minors+ poly(age,3,raw=TRUE):num_minors +
  bin_head +  poly(age,3,raw=TRUE):bin_head +
  bin_headFemale+  poly(age,3,raw=TRUE):bin_headFemale 

modelo4a <- lm(form_4,
               data = training )

predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$totalHoursWorked )
score4a

form_5

form_6

form_7

form_8

# -----------------------------------------------------
# 3) Check the the overall performance of the models,  the specification with the lowest prediction error
# and for the specification with the lowest prediction error, explore those observations that seem to ”miss 
# the mark.”
# -----------------------------------------------------



# -----------------------------------------------------
# 4) LOOCV. For the two models with the lowest predictive error
# -----------------------------------------------------
# Using the linear algebra features of R we can calculate this without running the model N
# times, as LOOCV intends. When the data is large, this can save us some computation time. The 
# two models with the lowest RMSE are the modelX (formXX) and modelX (formXX).

# ModelX
# RUN THE MODEL WITH ALL OBS

full_model1 <- lm(form_XX, data = db)
X1 <- model.matrix(full_model1)
y1 <- model.response(model.frame(full_model1))
beta_hat1 <- full_model1$coefficients
G_inv1 <- solve(t(X1) %*% X1) # Calculate the inverse of  (X'X)
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

# ModelX
# RUN THE MODEL WITH ALL OBS

full_model2 <- lm(form_XX, data = db)
X2 <- model.matrix(full_model2)
y2 <- model.response(model.frame(full_model2))
beta_hat2 <- full_model2$coefficients
G_inv2 <- solve(t(X2) %*% X2)
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
scores <- data.frame( Model= c(1, 2, 3, 4),
                     RMSE_vsa = c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a),
                     RMSE_loocv = c(rmse_model1, rmse_model2)
)

head(scores)

# Graph
RMSE_vsa   <-  c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a) 
RMSE_loocv <-  c(rmse_model1, rmse_model2)


scores<- data.frame( Model= rep(c(1, 2, 3, 4),3),
                     Approach= c(rep("Validation",4),rep("K-Fold",4),rep("LOOCV",4)),
                     RMSE= c(RMSE_vsa, RMSE_loocv)
)

ggplot(scores, ) + 
  geom_line(aes(x=Model,y=RMSE,col=Approach), size=0.5)+
  theme_bw() 

# Remember: The validation estimate can be highly variable depending on which observations 
# are included in the training set and which are included in the validation set. This issue is clearly mitigated 
# when we use K-Fold and/or LOOCV.
