# mathematical functions, user defined
expit <- function(x) {
	1 / (1 + exp(-x))
}

# mean outcome
# does nothing, for testing only.
# can be something which works on estimated values
# to compute some quantity of interest.
mean_outcome <- function(alpha, beta) {
	as.vector(c(alpha, beta))
}


