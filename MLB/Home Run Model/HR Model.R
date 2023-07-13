library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)
library(varImp)


source("~/Desktop/random-projects/MLB/All MLB Functions.R")

# Selecting the stats that I feel are predictive for Home Runs
HRdf = subset(batter, select = c(PA, K_pct, BB_pct, Barrel_Rate, brl_pa, wRC_plus, woba, est_woba, max_distance, avg_hr_distance, avg_hit_angle, avg_hit_speed, max_hit_speed, slg, est_slg, anglesweetspotpercent, ev95percent, ISO, GB_pct, LD_pct, FB_pct, Pull_pct, Cent_pct, Oppo_pct, HR_FB, HR, Team))

# Dropping all the rows with NA values
cleaned = na.omit(HRdf)

# Create an empty dataframe with 100 rows
new_df <- data.frame(matrix(ncol = ncol(cleaned) - 1, nrow = 0))

# Set column names of the new dataframe
colnames(new_df) <- names(cleaned)[names(cleaned) != "HR"]

# Perform 100 iterations of a Feature Importance Random Forest model
for (i in 1:100) {
  # Fit random forest model
  model <- randomForest(HR ~ ., data = cleaned, importance = TRUE)
  
  # Calculate variable importance scores
  i_scores <- importance(model, type = 1)
  i_scores <- i_scores[, "%IncMSE"]
  
  # Store the i_scores for the current iteration in the matrix
  new_df[nrow(new_df) + 1,] = i_scores
}

# Averages the 100 i_scores created for each column and then prints in order from least to greatest
average_i_scores <- colMeans(new_df)
print(sort(average_i_scores))

# Selecting the columns that are useful according to the Feature Importance Random Forest model
cleaned2 = subset(cleaned, select = c(PA, ISO, HR_FB, est_slg, slg, FB_pct, brl_pa, max_distance, avg_hit_angle, HR))

# Create an empty dataframe so the players and their stats can be manually inputted using Steamer's ROS projections
pred_set <- data.frame(PA = c(297, 313, 306, 314, 298, 286, 302, 298, 283, 296, 295, 288, 298, 274, 291, 289, 301, 287, 266, 303, 279, 273, 273, 278, 260, 289),
                       ISO = c(0.277, .241, .2, .243, .256, .210, .269, .236, .258, .192, .231, .248, .197, .213, .246, .213, .206, .234, .191, .198, .217, .212, .168, .185, .221, .101),
                       HR_FB = c(28.13, 16.27, 15.5, 19.63, 21.2, 12.9, 26.7, 20.63, 21.07, 14.1, 12.6, 20.0, 17.97, 17.2, 25, 13.97, 17.27, 18.8, 28.6, 18, 16.83, 20.1, 11.6, 16.13, 18.53, 3.57),
                       est_slg = c(.603, .495, .567, .579, .514, .434, .572, .51, .523, .454, .463, .531, .485, .536, .519, .44, .526, .537, .373, .448, .56, .404, .429, .442, .484, .418),
                       slg = c(0.547, .515, .503, .547, .517, .483, .556, .513, .510, .442, .509, .538, .486, .488, .469, .482, .485, .512, .442, .466, .511, .473, .431, .447, .461, .414),
                       FB_pct = c(32.47, 35.33, 29.67, 26.13, 32.1, 34.77, 27.4, 24.4, 32.33, 30.07, 29.17, 21.67, 21.1, 25.07, 32.43, 30.03, 29, 27.27, 14.8, 23.35, 27.33, 22.5, 27.85, 22.43, 29.53, 22.87),
                       brl_pa = c(11.7, 7.17, 8, 10.73, 9.67, 5.87, 9.8, 7.9, 9.53, 6.53, 6.43, 9.77, 7.27, 9.1, 9.9, 6.1, 8.47, 9.27, 3, 7.55, 10.23, 4.9, 5, 6.37, 8.4, 2.27),
                       max_distance = c(493,427,446,481,464,439,477,454,453,440,429,467,468,468,468,429,470,457,458,454,464,437,461,445,468,403),
                       avg_hit_angle = c(14, 19.07, 13.83, 12.33, 16.5, 17.93, 12.1, 6.8, 16.17, 15.67, 19.6, 7.6, 7.13, 11.03, 17.17, 20.03, 15.57, 12.63, 2.7, 9.55, 12, 10.45, 13.65, 9.47, 16, 11.03),
                       HR = c(18,16,12,12,17,13,19,14,18,12,13,16,12,11,17,13,13,14,10,13,13,11,8,10,13,3))


# Create another DF that has the names in it so the prediction values can be put back into this DF
# PA, ISO, SLG, and HR are from Steamer's ROS projections
# HR/FB, xSLG, FB Rate, Barrel Rate, and AVG Launch Angle are averages from 2021 - 2023 All Star Break
# Max Distance is the furthest hit ball from 2021 - 2023 All Star Break
player = data.frame(Name = c("Shohei Ohtani", "Mookie Betts", "Freddie Freeman", "Ronald Acuna Jr", "Matt Olson", "Ozzie Albies", "Fernando Tatis Jr", "Juan Soto", "Pete Alonso", "Francisco Lindor", "Jose Ramirez", "Vladimir Guerrero Jr", "Bo Bichette", "Bryce Harper", "Kyle Schwarber", "Nolan Arenado", "Paul Goldschmidt", "Rafael Devers", "Elly De La Cruz", "Julio Rodriguez", "Corey Seager", "Corbin Caroll", "Adley Rutschman", "Randy Arozarena", "Jorge Soler", "Luis Arraez"),
                    PA = c(297, 313, 306, 314, 298, 286, 302, 298, 283, 296, 295, 288, 298, 274, 291, 289, 301, 287, 266, 303, 279, 273, 273, 278, 260, 289),
                    ISO = c(0.277, .241, .2, .243, .256, .210, .269, .236, .258, .192, .231, .248, .197, .213, .246, .213, .206, .234, .191, .198, .217, .212, .168, .185, .221, .101),
                    HR_FB = c(28.13, 16.27, 15.5, 19.63, 21.2, 12.9, 26.7, 20.63, 21.07, 14.1, 12.6, 20.0, 17.97, 17.2, 25, 13.97, 17.27, 18.8, 28.6, 18, 16.83, 20.1, 11.6, 16.13, 18.53, 3.57),
                    est_slg = c(.603, .495, .567, .579, .514, .434, .572, .51, .523, .454, .463, .531, .485, .536, .519, .44, .526, .537, .373, .448, .56, .404, .429, .442, .484, .418),
                    slg = c(0.547, .515, .503, .547, .517, .483, .556, .513, .510, .442, .509, .538, .486, .488, .469, .482, .485, .512, .442, .466, .511, .473, .431, .447, .461, .414),
                    FB_pct = c(32.47, 35.33, 29.67, 26.13, 32.1, 34.77, 27.4, 24.4, 32.33, 30.07, 29.17, 21.67, 21.1, 25.07, 32.43, 30.03, 29, 27.27, 14.8, 23.35, 27.33, 22.5, 27.85, 22.43, 29.53, 22.87),
                    brl_pa = c(11.7, 7.17, 8, 10.73, 9.67, 5.87, 9.8, 7.9, 9.53, 6.53, 6.43, 9.77, 7.27, 9.1, 9.9, 6.1, 8.47, 9.27, 3, 7.55, 10.23, 4.9, 5, 6.37, 8.4, 2.27),
                    max_distance = c(493,427,446,481,464,439,477,454,453,440,429,467,468,468,468,429,470,457,458,454,464,437,461,445,468,403),
                    avg_hit_angle = c(14, 19.07, 13.83, 12.33, 16.5, 17.93, 12.1, 6.8, 16.17, 15.67, 19.6, 7.6, 7.13, 11.03, 17.17, 20.03, 15.57, 12.63, 2.7, 9.55, 12, 10.45, 13.65, 9.47, 16, 11.03),
                    HR = c(18,16,12,12,17,13,19,14,18,12,13,16,12,11,17,13,13,14,10,13,13,11,8,10,13,3))


# If you want to include the Steamer HR projections
train <- cleaned2

# If don't you want to include the Steamer HR projections
#train <- subset(cleaned2, select = -c(HR))

test <- pred_set

# Applies the Random Forest model
rf <- randomForest(x = train, y = cleaned2$HR, ntree = 500)
predictions <- predict(rf, newdata = test)

player$predictions = predictions

# Calculate the mean squared error
mse <- mean((predictions - test$HR)^2)

# Calculate the root mean squared error
rmse <- sqrt(mse)

# Print the performance metrics
print(paste("Mean Squared Error:", mse))
print(paste("Root Mean Squared Error:", rmse))

# Calculates and prints the MAPE
absolute_error <- abs((test$HR - predictions) / test$HR) * 100
mape <- mean(absolute_error)
print(paste("MAPE:", mape))

