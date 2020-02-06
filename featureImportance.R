featureImportance <- function(df) {
	# ensure the results are repeatable
	set.seed(2811)

	# load the libraries
	library(dplyr)
	library(caret)

	# rearrange data frame
	df <- df %>% 
		select(id, outcome, everything())
	df$outcome <- as.factor(df$outcome)

	# calculate feature importance (Learning Vector Quantization)
	control <- trainControl(method = "repeatedcv", 
		                    number = 10, 
		                    repeats = 3, 
		                    verboseIter = TRUE)
	model <- train(outcome ~ ., 
		           data = df[, 2:ncol(df)], 
		           method = "lvq", 
		           preProcess = "scale", 
		           trControl = control)

	importance <- varImp(model, 
		                 scale = FALSE)

	fi <- as.data.frame(importance$importance)
	fi <- cbind(Variable = row.names(fi), fi) %>% 
		select(Variable, B, M)

	# return result
	return(fi)
}


getOutputSchema <- function() {
	return(data.frame(
		Variable = prep_string(), 
		B = prep_decimal(), 
		M = prep_decimal())
	);
}