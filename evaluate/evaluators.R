# one file for all evaluators
# these will work on replicated outputs
# not on single output.
bias <- function(result, truth) {
	rowMeans(result) - truth
}

RMSE <- function(result, truth) {
	sqrt(rowMeans((result - truth)^2))
}
