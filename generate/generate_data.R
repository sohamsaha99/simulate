# file for generating data based on parameters
# source("../utils/utils.R", chdir = TRUE, local = TRUE)

# params contain parameters that denote truth
# more_params contain parameters that are not part of truth,
# but required for simulation. For example: sample size
generate_data <- function(params, more_params = NULL) {
	n <- more_params[["n"]]
	rnorm(n = n, mean = params[["mu"]], sd = params[["sigma"]])
}
