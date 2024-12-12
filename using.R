# install.packages("tidyverse")
# install.packages("glmnet")
# library(tidyverse)  # For data manipulation (optional, for advanced operations)
# library(glmnet) 

########original data, checking and setting up the algorithm 

##file
file_path <- "~/Fall '24/Stor 390/Assignments/PROJECT/compas-scores-two-years.csv"  #the compas data used in this study (and multiple others i believe)
data <- read.csv(file_path, stringsAsFactors = FALSE)

##clean data

cleaned_data <- na.omit(data)

if("race" %in% colnames(cleaned_data)) {
  translation_map <- c("Caucasian" = 1, "African-American" = 2, "Hispanic" = 3, "Asian" = 4, "Native American" = 5, "Other" = 6)
  cleaned_data$race <- as.numeric(translation_map[cleaned_data$race])
}

if ("sex" %in% colnames(cleaned_data)) {
  translation_map <- c("Female" = 1, "Male" = 0)
  cleaned_data$sex <- as.numeric(translation_map[cleaned_data$sex])
}

if("c_charge_degree" %in% colnames(cleaned_data)) {
  translation_map <- c("F" = 1, "M" = 0)
  cleaned_data$c_charge_degree <- as.numeric(translation_map[cleaned_data$c_charge_degree])
}

if("score_text" %in% colnames(cleaned_data)) {
  translation_map <- c("High" = 1, "Medium" = 1, "Low" = 0)
  cleaned_data$score_text <- as.numeric(translation_map[cleaned_data$score_text])
}
cleaned_data <- cleaned_data %>% relocate(score_text, .before = 1)
cleaned_data <- cleaned_data %>% relocate(two_year_recid, .before = 1)

cleaned_data <- cleaned_data[, !names(cleaned_data) %in% c("id", "age_cat", "decile_score", "type_of_assessment", "decile_score.1")]
cleaned_data <- cleaned_data[, !names(cleaned_data) %in% c("score_text")] ##removing compas's prediction bc i don't think i need it - can remove previous adjustments for it if wanted

cleaned_data <- na.omit(cleaned_data)

head(cleaned_data)

##LASSO algorithm (x & y would also be used for EN and regression though)
##

x <- as.matrix(cleaned_data[, -1,]) # independent variables
y <- cleaned_data$two_year_recid         #respond variable


# 10-fold cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(
  x, y,
  alpha = 1,
  family = "binomial",
  nfolds = 10
)

# Optimal λ: One standard deviation greater than minimum
lambda_min <- cv_lasso$lambda.min
index_min <- which(cv_lasso$lambda == lambda_min)
std_error_min <- cv_lasso$cvsd[index_min]
lambda_needed <- lambda_min + std_error_min

final_lasso <- glmnet(
  x, y,
  alpha = 1,
  lambda = lambda_needed,
  family = "binomial"
)

predicted_probabilities <- predict(final_lasso, newx = x, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0) # assuming threshold at 0.5 (not specified in the text)

accuracy <- mean(predicted_classes == y)
cat("Accuracy of the LASSO Model:", accuracy, "\n")

cleaned_data$predicted_classes <- predicted_classes

accuracy <- mean(cleaned_data$predicted_classes == cleaned_data$two_year_recid) 
cat("Accuracy of the LASSO Model:", accuracy, "\n")

white <- cleaned_data[cleaned_data$race == '1',]
accuracy <- mean(white$predicted_classes == white$two_year_recid) 
cat("White Accuracy of the LASSO Model:", accuracy, "\n")

black <- cleaned_data[cleaned_data$race == '2',]
accuracy <- mean(black$predicted_classes == black$two_year_recid) 
cat("Black Accuracy of the LASSO Model:", accuracy, "\n")

##all matrix

all_confusion_matrix <- table(
  Predicted = cleaned_data$predicted_classes,
  Actual = cleaned_data$two_year_recid
)

all_confusion_matrix_percentage <- prop.table(all_confusion_matrix) * 100

print(round(all_confusion_matrix_percentage, 2)) 
all_confusion_matrix_df <- as.data.frame.matrix(round(white_confusion_matrix_percentage, 2))


##white matrix

white_confusion_matrix <- table(
  Predicted = white$predicted_classes,
  Actual = white$two_year_recid
)

white_confusion_matrix_percentage <- prop.table(white_confusion_matrix) * 100

print(round(white_confusion_matrix_percentage, 2)) 
white_confusion_matrix_df <- as.data.frame.matrix(round(white_confusion_matrix_percentage, 2))


##black matrix

black_confusion_matrix <- table(
  Predicted = black$predicted_classes,
  Actual = black$two_year_recid
)

black_confusion_matrix_percentage <- prop.table(black_confusion_matrix) * 100

print(round(black_confusion_matrix_percentage, 2)) 
black_confusion_matrix_df <- as.data.frame.matrix(round(black_confusion_matrix_percentage, 2))

write.csv(all_confusion_matrix_df, "~/Fall '24/Stor 390/Assignments/PROJECT/all_confusion_matrix.csv", row.names = TRUE)
write.csv(white_confusion_matrix_df, "~/Fall '24/Stor 390/Assignments/PROJECT/white_confusion_matrix.csv", row.names = TRUE)
write.csv(black_confusion_matrix_df, "~/Fall '24/Stor 390/Assignments/PROJECT/black_confusion_matrix.csv", row.names = TRUE)

