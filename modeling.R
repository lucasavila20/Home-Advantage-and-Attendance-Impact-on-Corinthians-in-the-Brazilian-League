library(lme4)

# Create a model

data2$venue <- as.factor(data2$venue)
data2$victory <- ifelse(data2$result == "V", 1, 0)
data2$draw_loss <- 1 - data2$victory

mixed_mod <- glmer(
  cbind(victory, draw_loss) ~ venue + attendance + venue:attendance + (1 | Opponent) + (1 | year), #result_binary
  family = binomial, 
  data = data2
)

summary(mixed_mod)

# Fix the model

data2$scaled_attendance <- (data2$attendance - mean(data2$attendance))/sd(data2$attendance)

mixed_mod <- glmer(
  cbind(victory, draw_loss) ~ venue + scaled_attendance + venue:scaled_attendance + (1 | Opponent) + (1 | year), #result_binary
  family = binomial(), 
  data = data2
)

summary(mixed_mod)

# Make few predictions

example_test2 <- data2[1:4,c(5,9, 17, 21)]
example_test2[1,] <- c("Home", "Grêmio", 2014, 20)
example_test2[2,] <- c("Away", "Grêmio", 2014, 20)
example_test2[3,] <- c("Home", "Grêmio", 2014, 40000)
example_test2[4,] <- c("Away", "Grêmio", 2014, 40000)
example_test2$scaled_attendance <- as.numeric(example_test2$scaled_attendance)
example_test2$scaled_attendance <- scale(example_test2$scaled_attendance)

predictions <- predict(mixed_mod, 
                       newdata = example_test2, 
                       type = "response"
)
predictions


# Visualize the model

predictions <- predict(mixed_mod, type = "response", newdata = data2)
# Visualization
ggplot(data2, aes(x = attendance, y = predictions, color = as.factor(venue))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "GLMM Predictions",
       x = "Attendance",
       y = "Predicted Probability",
       color = "venue")