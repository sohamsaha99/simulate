# file for generating data based on parameters
# source("../utils/utils.R", chdir = TRUE, local = TRUE)

# params contain parameters that denote truth and additional parameters
# returns a list with entries: data, truth_local, knowledge_local
generate_data <- function(params) {
	n <- params[["n"]]
	nu <- params[["nu"]]
	# generating truth
	theta <- rnorm(n = 1, mean = params[["mu"]], sd = params[["sigma"]])
	# generating data
	x <- rnorm(n = n,  mean = theta + nu, sd = 1)
	# generating oracle knowledge
	delta <- params[["delta"]]
	knowledge_local <- list(lower = nu + theta - delta, upper = nu + theta + delta)
	list(
		data = x,
		truth_local = list(theta = theta),
		knowledge_local = knowledge_local
	)
}
