#This document will serve as a report for the homework. Comments are given between
#lines to clarify the questions asked in homework tasks.

wd <- "/Users/oordulu/Documents/IE582/HW2/"
setwd(wd)
#match_data_full<-read.csv("match_data")
#save(match_data_full, file = "match_data_full.RData")
load("match_data_full.RData")

library(rpart)
library(rpart.plot)

set.seed(3808018)

head(match_data_full)

match_data_full <- as.data.frame(match_data_full)
# Filter rows where both columns are not "true"
match_data <- match_data_full[match_data_full$suspended != "True" & match_data_full$stopped != "True", ]


# Select specific columns
columns_to_check <- c("X1", "X2", "X")

# Calculate the number of missing values in each column
missing_counts <- colSums(is.na(match_data[columns_to_check]))

# Print the counts
print(missing_counts)

#No missing X, X1 or X2 values.

#Task 1.1
#First separate 1st and 2nd halves

first_half_data <- match_data[match_data$halftime == "1st-half", ]
second_half_data <- match_data[match_data$halftime != "1st-half", ]

BookProb_Home1=1/first_half_data$X1
BookProb_Away1=1/first_half_data$X2
BookProb_Tie1=1/first_half_data$X
BookProbs1 <- cbind(BookProb_Home1, BookProb_Away1, BookProb_Tie1)
head(BookProbs1)
#Note that these probabilities do not add up to 1.

BookProb_Home2=1/second_half_data$X1
BookProb_Away2=1/second_half_data$X2
BookProb_Tie2=1/second_half_data$X
BookProbs2 <- cbind(BookProb_Home2, BookProb_Away2, BookProb_Tie2)
head(BookProbs2)
#Note that these probabilities do not add up to 1.

#Task 1.2

Norm_Probs1 <- BookProbs1 / rowSums(BookProbs1)
head(Norm_Probs1)

Norm_Probs2 <- BookProbs2 / rowSums(BookProbs2)
head(Norm_Probs2)
#The normalized probabilities add up to 1.

#Task 1.3
#First Halves
BookPHome_Paway1=BookProbs1[, 1] - BookProbs1[, 2]
plot(
  BookPHome_Paway1,           # x-axis: difference (column 1 - column 2)
  BookProbs1[, 3],             # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "First Halves - P(home)-P(Away) vs. P(tie) (Book Probs)", # Title of the plot
  col = "orange",             # Point color
  pch = 10                    # Point style
)

NormPHome_Paway1=Norm_Probs1[, 1] - Norm_Probs1[, 2]  
plot(
  NormPHome_Paway1,           # x-axis: difference (column 1 - column 2)
  Norm_Probs1[, 3],           # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "First Halves - P(home)-P(Away) vs. P(tie) (Norm Probs)",
  col = "blue",               # Point color
  pch = 10                    # Point style
)


#Second Halves
BookPHome_Paway2=BookProbs2[, 1] - BookProbs2[, 2]
plot(
  BookPHome_Paway2,           # x-axis: difference (column 1 - column 2)
  BookProbs2[, 3],             # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "Second Halves - P(home)-P(Away) vs. P(tie) (Book Probs)", # Title of the plot
  col = "orange",             # Point color
  pch = 10                    # Point style
)

NormPHome_Paway2=Norm_Probs2[, 1] - Norm_Probs2[, 2]  
plot(
  NormPHome_Paway2,           # x-axis: difference (column 1 - column 2)
  Norm_Probs2[, 3],           # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "Second Halves - P(home)-P(Away) vs. P(tie) (Norm Probs)",
  col = "blue",               # Point color
  pch = 10                    # Point style
)
#Plots show that P(tie) makes a maximum approximately at P(Home)-P(Away)=0 as
#expected since an unbiased evaluation of P(home)-P(Away) means equal probabilities
#of home and away wins, which leaves tie as the most expected outcome.

#Now start binning

#First Halves

Pdiff_bin_result1 <- data.frame(
  BookPHome_Paway1 = BookPHome_Paway1,  # P(Home) - P(Away) values
  BookProb_Tie1 = BookProb_Tie1,
  result = first_half_data$result       # Game results
)

# Create bins for P(Home) - P(Away) values from -1 to 1 in steps of 0.2
bins <- seq(-1, 1, by = 0.2)  # Bins from -1 to 1 with 0.2 intervals
Pdiff_bin_result1$P_diff_bin <- cut(Pdiff_bin_result1$BookPHome_Paway1, breaks = bins, include.lowest = TRUE)

# Count how many ties ('X') occur in each bin
library(dplyr)

game_counts <- Pdiff_bin_result1 %>%
  group_by(P_diff_bin) %>%
  summarise(
    total_games = n(),          # Total number of games in each bin
    ties = sum(result == 'X'),   # Count occurrences of "X" (ties)
    ties_per_game = ties / total_games,
    avg_P_tie = mean(BookProb_Tie1, na.rm = TRUE)  # Average P(Tie) in each bin
  )
# Display the result
print(game_counts)
#The above matrix is calculated for first halves. The logic is that if ties_per_game
#column is higher than avg_P_tie for any bin, then bookmakers underestimated the
#probability of tie compared to observed tie games per total games. This reaches
#to conclusion that for such cases, betting on tie would be beneficial. Analyzing
#with this logic, there are no cleas benefits of betting tie based on first halves
#data since the two values are only slightly different for all bins.

#Second Halves
Pdiff_bin_result2 <- data.frame(
  BookPHome_Paway2 = BookPHome_Paway2,  # P(Home) - P(Away) values
  BookProb_Tie2 = BookProb_Tie2,
  result = second_half_data$result       # Game results
)

# Create bins for P(Home) - P(Away) values from -1 to 1 in steps of 0.2
bins <- seq(-1, 1, by = 0.2)  # Bins from -1 to 1 with 0.2 intervals
Pdiff_bin_result2$P_diff_bin <- cut(Pdiff_bin_result2$BookPHome_Paway2, breaks = bins, include.lowest = TRUE)

# Count how many ties ('X') occur in each bin
game_counts <- Pdiff_bin_result2 %>%
  group_by(P_diff_bin) %>%
  summarise(
    total_games = n(),          # Total number of games in each bin
    ties = sum(result == 'X'),   # Count occurrences of "X" (ties)
    ties_per_game = ties / total_games,
    avg_P_tie = mean(BookProb_Tie2, na.rm = TRUE)  # Average P(Tie) in each bin
  )

# Display the result
print(game_counts)
#Similar to the observations for the first halves data, analysis for the second
#halves data does not show any clear benefits for betting ties. It seems
#bookmakers do a good job in covering themselves by setting the odds in line 
#with observable ties per game.

#Task 2
ids_to_remove <- second_half_data %>%
  mutate(
    state_change = current_state != lag(current_state)  # Identify when current_state changes
  ) %>%
  filter(
    state_change & minute > 45  # Filter rows where state changes and minute > 45
  ) %>%
  pull(fixture_id)  # Extract fixture_id of the rows to be removed

unique_fixture_ids_removed <- length(unique(ids_to_remove))

# Remove all rows with those fixture_ids
second_half_data_lg <- second_half_data %>%
  filter(!fixture_id %in% ids_to_remove)  # Exclude rows with the identified fixture_ids

cat("Number of different fixture_ids removed:", unique_fixture_ids_removed, "\n")
#Date relevant to tottal of 54 matches are eliminated.

dim(second_half_data)-dim(second_half_data_lg)
#Total of 2543 rows were from matches that had late goals (after 90th minute) 
#that generated noise, now they are eliminated.

ids_to_remove <- first_half_data %>%
  filter(
    (Redcards...away != 0 | Redcards...home != 0) & minute < 20  # Condition: Redcards are nonzero and minute < 20
  ) %>%
  pull(fixture_id)  # Extract fixture_id of the rows to be removed

# Count the number of unique fixture_ids to be removed
unique_fixture_ids_removed <- length(unique(ids_to_remove))

# Remove all rows with those fixture_ids
first_half_data_rc <- first_half_data %>%
  filter(!fixture_id %in% ids_to_remove)  # Exclude rows with the identified fixture_ids

cat("Number of different fixture_ids removed:", unique_fixture_ids_removed, "\n")
#Data relevant to 3 matches are elimated.

dim(first_half_data)-dim(first_half_data_rc)
#Total of 147 rows were from matches that had early red cards (before 20th minute) 
#that generated noise, now they are eliminated.

#Calculate probabilities again for the 2nd task.
BookProb_Home1=1/first_half_data_rc$X1
BookProb_Away1=1/first_half_data_rc$X2
BookProb_Tie1=1/first_half_data_rc$X
BookProbs1 <- cbind(BookProb_Home1, BookProb_Away1, BookProb_Tie1)
head(BookProbs1)

BookProb_Home2=1/second_half_data_lg$X1
BookProb_Away2=1/second_half_data_lg$X2
BookProb_Tie2=1/second_half_data_lg$X
BookProbs2 <- cbind(BookProb_Home2, BookProb_Away2, BookProb_Tie2)
head(BookProbs2)

Norm_Probs1 <- BookProbs1 / rowSums(BookProbs1)
head(Norm_Probs1)

Norm_Probs2 <- BookProbs2 / rowSums(BookProbs2)
head(Norm_Probs2)

#First Half
BookPHome_Paway1=BookProbs1[, 1] - BookProbs1[, 2]
plot(
  BookPHome_Paway1,           # x-axis: difference (column 1 - column 2)
  BookProbs1[, 3],             # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "First Halves - P(home)-P(Away) vs. P(tie) (Book Probs)", # Title of the plot
  col = "orange",             # Point color
  pch = 10                    # Point style
)

NormPHome_Paway1=Norm_Probs1[, 1] - Norm_Probs1[, 2]  
plot(
  NormPHome_Paway1,           # x-axis: difference (column 1 - column 2)
  Norm_Probs1[, 3],           # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "First Halves - P(home)-P(Away) vs. P(tie) (Norm Probs)",
  col = "blue",               # Point color
  pch = 10                    # Point style
)


#Second Half
BookPHome_Paway2=BookProbs2[, 1] - BookProbs2[, 2]
plot(
  BookPHome_Paway2,           # x-axis: difference (column 1 - column 2)
  BookProbs2[, 3],             # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "Second Halves - P(home)-P(Away) vs. P(tie) (Book Probs)", # Title of the plot
  col = "orange",             # Point color
  pch = 10                    # Point style
)

NormPHome_Paway2=Norm_Probs2[, 1] - Norm_Probs2[, 2]  
plot(
  NormPHome_Paway2,           # x-axis: difference (column 1 - column 2)
  Norm_Probs2[, 3],           # y-axis: third column (BookProb_Tie)
  xlab = "P(home)-P(Away)",   # Label for the x-axis
  ylab = "P(tie)",            # Label for the y-axis
  main = "Second Halves - P(home)-P(Away) vs. P(tie) (Norm Probs)",
  col = "blue",               # Point color
  pch = 10                    # Point style
)
#After eliminating noise, the resulting graphs are not different in any particular
#way compared to previous plots.

#First Halves

Pdiff_bin_result1 <- data.frame(
  BookPHome_Paway1 = BookPHome_Paway1,  # P(Home) - P(Away) values
  BookProb_Tie1 = BookProb_Tie1,
  result = first_half_data_rc$result       # Game results
)

# Create bins for P(Home) - P(Away) values from -1 to 1 in steps of 0.2
bins <- seq(-1, 1, by = 0.2)  # Bins from -1 to 1 with 0.2 intervals
Pdiff_bin_result1$P_diff_bin <- cut(Pdiff_bin_result1$BookPHome_Paway1, breaks = bins, include.lowest = TRUE)

# Count how many ties ('X') occur in each bin
library(dplyr)

game_counts <- Pdiff_bin_result1 %>%
  group_by(P_diff_bin) %>%
  summarise(
    total_games = n(),          # Total number of games in each bin
    ties = sum(result == 'X'),   # Count occurrences of "X" (ties)
    ties_per_game = ties / total_games,
    avg_P_tie = mean(BookProb_Tie1, na.rm = TRUE)  # Average P(Tie) in each bin
  )
# Display the result
print(game_counts)

#Second Halves
Pdiff_bin_result2 <- data.frame(
  BookPHome_Paway2 = BookPHome_Paway2,  # P(Home) - P(Away) values
  BookProb_Tie2 = BookProb_Tie2,
  result = second_half_data_lg$result       # Game results
)

# Create bins for P(Home) - P(Away) values from -1 to 1 in steps of 0.2
bins <- seq(-1, 1, by = 0.2)  # Bins from -1 to 1 with 0.2 intervals
Pdiff_bin_result2$P_diff_bin <- cut(Pdiff_bin_result2$BookPHome_Paway2, breaks = bins, include.lowest = TRUE)

# Count how many ties ('X') occur in each bin
library(dplyr)

game_counts <- Pdiff_bin_result2 %>%
  group_by(P_diff_bin) %>%
  summarise(
    total_games = n(),          # Total number of games in each bin
    ties = sum(result == 'X'),   # Count occurrences of "X" (ties)
    ties_per_game = ties / total_games,
    avg_P_tie = mean(BookProb_Tie2, na.rm = TRUE)  # Average P(Tie) in each bin
  )

# Display the result
print(game_counts)
#The analyses on ties_per_game and avg_P_tie are also similar to the previous
#analyses. It seems games that were creating noise did not alter the current
#analyses.


match_data102 <- match_data %>%
  mutate(result = ifelse(result == "X", 0, result))  # Replace "X" with 0

match_data_102_dt <- match_data102 %>%
  select(
    -fixture_id, 
    -halftime, 
    -current_time, 
    -half_start_datetime, 
    -match_start_datetime, 
    -minute, 
    -second, 
    -latest_bookmaker_update, 
    -suspended, 
    -stopped, 
    -name, 
    -ticking, 
    -current_state, 
    -final_score
  )

library(mice)
#Data imputation is used since decision tree is a type of learner that performs
#well under imputation. Missing values are filled using pmm method.

#imputed_data <- mice(match_data_102_dt, m = 1, method = 'pmm', maxit = 10, seed = 500)
#save(imputed_data, file = "imputed_data.RData")
load("imputed_data.RData")
match_data_102_dt_impute_pmm <- complete(imputed_data)

#Columns of the data that are directly related with the match results are
#excluded since a learner that is able to perform in game statistics are
#important for this example. For example knowing that home team scored
#is a direct cause of that team performing better. We are more interested
#in variables that do not change the odds that much, but also are clear
#indicators of a team with a better chance, so that we can make a profitable
#bet.
match_data_102_dt_impute_pmm_odds <- match_data_102_dt_impute_pmm %>%
  select(
    -X1,
    -X2,
    -X,
    -Goals...home,
    -Goals...away,
    -Assists...home,
    -Assists...away
  )

# Fit the decision tree
decision_tree <- rpart(
  formula = result ~ .,  # Use all other columns to predict 'result'
  data = match_data_102_dt_impute_pmm_odds, 
  method = "class",       # Classification mode
  control=rpart.control(cp=0,maxdepth=4,minsplit=0.01*nrow(match_data_102_dt_impute_pmm))
)

# Print a summary of the tree
summary(decision_tree)

# Visualize the tree
rpart.plot(decision_tree, type = 3, extra = 101, fallen.leaves = TRUE)


# Predict on the training data
predictions <- predict(decision_tree, match_data_102_dt_impute_pmm_odds, type = "class")

# Confusion matrix
table(Predicted = predictions, Actual = match_data_102_dt_impute_pmm_odds$result)
#It looks like the decision tree is very hesitant in predicting ties. Home win
#predictions are very large where the actual result was a tie.

# Calculate accuracy
accuracy <- mean(predictions == match_data_102_dt_impute_pmm_odds$result)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))


# Get the top 10 most important variables
importance <- head(sort(decision_tree$variable.importance, decreasing = TRUE), 5)
# Create a horizontal barplot of the top 10 most important variables
barplot(importance, horiz = TRUE, col = "lightblue", main = "Top 5 Most Important Variables")
#Shots.On.Target...away, Saves...home, Shots.Total...away, Key.Passes...away,
#Shots.On.Target...home are found to be the most important variables.