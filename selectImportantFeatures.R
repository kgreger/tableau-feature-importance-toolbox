selectImportantFeatures <- function(df) {
	# ensure the results are repeatable
	set.seed(2811)

	# load the libraries
	library(dplyr)
	library(caret)

	# rearrange data frame
	df <- df %>% 
		select(id, outcome, everything())
	df$outcome <- as.factor(df$outcome)

	# backwards feature selection (Recursive Feature Elimination)
	control <- rfeControl(functions = rfFuncs, 
		                  method = "cv", 
		                  number = 10, 
		                  verbose = TRUE)

	results <- rfe(df[, 2:ncol(df)], 
		           df[, 1], 
		           rfeControl = control)

	print(results)
	iv <- as.data.frame(cbind(importantVariable = predictors(results)))

	# return result
	return(iv)
}


getOutputSchema <- function() {
	return(data.frame(
		importantVariable = prep_string())
	);
}