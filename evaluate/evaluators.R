# one file for all evaluators
# these will work on replicated outputs
# not on single output.
bias <- function(result, truth_global) {
	rowMeans(result) - truth_global
}

RMSE <- function(result, truth_global) {
	sqrt(rowMeans((result - truth_global)^2))
}
