# Author: Jeremy Boyd (jboyd@ucsd.edu)
# Summary: Predictive model of the cutting dataset.

# Load party package.
library(party)
library(caret)
library(dplyr)

# Read in data.
cuts <- read.delim("cleaned_up_for_prediction.txt", header = TRUE)

##########################################################################
# This function takes a set of seeds as its first argument and a set of
# predictor variables as its second. It creates a random forest model for
# each seed using the predictors specified and returns the predictive
# accuracy for each model.
##########################################################################
 
get_accuracy <- function(seeds, predictors) {
    
    # Set seed.
    set.seed(seeds)
    
    # Use 659 observations for training, test on 100.
    train <- sample(759, 659)
    imbal_train <- cuts[train, ]
    imbal_test <- cuts[-train, ]
    
    # Create down and upsampled training sets to balance the number
    # of observations at each level of cutoff. This doesn't improve
    # performance.
#     down_train <- downSample(x = imbal_train[, -ncol(imbal_train)],
#                              y = imbal_train$cutoff)
#     down_train <- rename(down_train, cutoff = Class)
# 
#     up_train <- upSample(x = imbal_train[, -ncol(imbal_train)],
#                          y = imbal_train$cutoff)
#     up_train <- rename(up_train, cutoff = Class)
    
    # Concatenate predictor names into a single string for use in
    # model formula.
    predictors <- paste(predictors, collapse = " + ")
    
    # Concatenate dependent variable and predictors into formula string.
    formula <- paste("cutoff ~ ", predictors, sep = "")
    
    # User output.
    cat("Formula:", formula, "\n", sep = " ")
    
    # Convert string to an R formula.
    formula <- as.formula(formula)
         
    # Specify random forest model. For 7 predictors, set mtry to
    # sqrt(7) ~ 3. 
    cuts.cf <- cforest(formula, data = imbal_train,
                       controls = cforest_unbiased(mtry = 3,
                                                   ntree = 2500))
    
    # Store cuts.cf predictions as a new column in imbal_test.
    imbal_test$cutoff_predicted <- predict(cuts.cf, imbal_test, OOB = TRUE,
                                     type = "response")

    # Compare observed and predicted cutoff values. Code correct
    # predictions as 1, incorrect as 0.
    imbal_test$correct_prediction <- ifelse(
        imbal_test$cutoff == imbal_test$cutoff_predicted, 1, 0)

    # Calculate and return model's predictive accuracy on the test set.
    return(mean(imbal_test$correct_prediction))    
}

##########################################################################
# Model including all predictors.
##########################################################################

# Create a list of random seeds to use.
seeds <- c(1:10)

# Loop over seeds.
model1 <- unlist(lapply(seeds, get_accuracy,
                                 predictors = c("vehicleStatus", "traffic",
                                                "driverSex", "driverAge",
                                                "time2", "rater", "interID")))

# Mean predictive accuracy across all models.
mean(model1)

##########################################################################
# Compare to baseline.
##########################################################################

# baseline1 is .5.
baseline1 <- .5

# baseline2 is the number of drivers who didn't cut.
baseline2 <- unname(xtabs(~ cutoff, cuts) / nrow(cuts))[1]

# Compare model1 results to baselines 1 and 2. Shows significant
# improvements in accuracy over baseline1, but not baseline2.
wilcox.test(jitter(model1), mu = baseline1)
wilcox.test(jitter(model1), mu = baseline2)