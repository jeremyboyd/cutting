# Author: Jeremy Boyd (jboyd@ucsd.edu)
# Date: July 22, 2015
# Summary: Predictive models of the cutting dataset.

# Load party package.
library(party)

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
    
    # Use 2/3 of data for training, the remaining 1/3 for test.
    train <- sample(759, 506)
    train.dat <- cuts[train, ]
    test.dat <- cuts[-train, ]
    
    # Create model formula.
    # Concatenate predictor names into a single string.
    predictors <- paste(predictors, collapse = " + ")
    
    # Concatenate dependent variable and predictors into formula string.
    formula <- paste("cutoff ~ ", predictors, sep = "")
    
    # User output.
    cat("Formula: ", formula, ".", "\n", sep = "")
    
    # Convert string to an R formula.
    formula <- as.formula(formula)
         
    # Specify random forest model.
    cuts.cf <- cforest(formula, data = train.dat,
                       controls = cforest_unbiased(ntree = 1000))
    
    # Store cut.cf1 predictions as a new column in test.dat.
    test.dat$cutoff_predicted <- predict(cuts.cf, test.dat, OOB = TRUE,
                                         type = "response")
    
    # Compare observed and predicted cutoff values. Code correct
    # predictions as 1, incorrect as 0.
    test.dat$correct_prediction <- ifelse(test.dat$cutoff == test.dat$cutoff_predicted,
                                          1, 0)
    
    # Calculate and return model's predictive accuracy on the test set.
    return(mean(test.dat$correct_prediction))    
}

##########################################################################
# Measure the predictive accuracy of 100 different random forest models
# using the full set of five predictors.
##########################################################################

# Create a list of random seeds to use.
seeds <- c(1:100)

# Loop over seeds.
accuracy_5_pred <- unlist(lapply(seeds, get_accuracy,
                                 predictors = c("vehicleStatus", "traffic",
                                                "driverSex", "driverAge",
                                                "time2")))

##########################################################################
# Measure the predictive accuracy of 100 different random forest models
# using the full set of five predictors, minus vehicleStatus.
##########################################################################

# Loop over seeds.
accuracy_5_pred_vehicleStatus <- unlist(lapply(seeds, get_accuracy,
                                 predictors = c("traffic", "driverSex",
                                                "driverAge", "time2")))