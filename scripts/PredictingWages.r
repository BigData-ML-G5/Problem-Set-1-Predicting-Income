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
library(caret)
library(dplyr)
set.seed(10101)  # Set set for replicability purposes 

inTrain <- createDataPartition(
  y = db$ingreso_por_hora,  ## the outcome data are needed
  p = .70, ## The percentage of training data
  list = FALSE
)

training <- db |> filter(row_number() %in% inTrain)
testing  <- db |> filter(!(row_number() %in% inTrain))

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
score1a<- RMSE(pred = predictions, obs = testing$totalHoursWorked )
score1a

# Train our models - Model 2
form_2 <- log(ingreso_por_hora) ~ gender
model2a <- lm(form_2,
              data = training)

predictions <- predict(object = model2a, newdata = testing)
score2a <- RMSE(pred = predictions, obs = testing$totalHoursWorked )
score2a

# Train our models - Model 3
form_3 <- log(ingreso_por_hora) ~ gender + age + age2 + maximo_nivel_educativo + (maximo_nivel_educativo*maximo_nivel_educativo) 
+ experiencia + experiencia2 + ocupacion 
+ industria + region
model3a <- lm(form_3, data = training)

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, testing$totalHoursWorked )
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

