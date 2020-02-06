correlationMatrix <- function(df) {
	# ensure the results are repeatable
	set.seed(2811)

	# load the libraries
	library(dplyr)
	library(corrr)

	# rearrange data frame
	df <- df %>% 
		select(id, outcome, everything())

	# calculate correlation matrix
	cm <- correlate(df[, 3:ncol(df)], 
		            use = "pairwise.complete.obs", 
		            method = "pearson", 
		            diagonal = 1)
	
	# transform to long format
	cm <- stretch(cm)

	# return result
	return(cm)
}


getOutputSchema <- function() {
	return(data.frame(
		x = prep_string(), 
		y = prep_string(), 
		r = prep_decimal())
	);
}