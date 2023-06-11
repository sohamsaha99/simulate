# Files within "intermediate/" directory contains
# functions that are needed for multiple methods
# the outputs are available to methods from knowledge[[".intermediate"]]
split_means <- function(x, ...) {
	n <- length(x)
	s <- floor(n/4)
	q1 <- x[1:s]
	q2 <- x[s + (1:s)]
	q3 <- x[2*s + (1:s)]
	q4 <- x[3*s + (1:s)]
	return(c(base::mean(q1), base::mean(q2), base::mean(q3), base::mean(q4)))
}
