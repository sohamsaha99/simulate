#################### STEP 1 ###########################
# Set parameters, including the ones varied
params <- list(
	nu = 5,
	mu = 1,
	sigma = 2,
	# additional parameters
	n = 32,
	delta = 1
)

#################### STEP 2 ###########################
# Declare data generation function in file "generate/generate_data.R"
# Also include intermediate truth values in the function output (e.g. Bayesian)
# Also include intermediate oracle knowledge in the output
# If data generation is not completely random but depends on
# something that chamges (for example, 1000 csv files), then modify
# the file "evaluate/replicate.R" : Look for lapply and parLapply

#################### STEP 3 ###########################
# Declare the methods in directory "methods/"
# Form: list(a = "b").
# "a": to be used as column names and in plots
# "b": file b.R contains the definition in a function b()
# Each method takes arguments data and knowledge (optional)
# Knowledge: oracle knowledge and intermediate values from other functions
# Define intermediate functions in "methods/intermediate/"
methods_list <- list(
	arithmetic_mean = "mean",
	# fixed_truncated_mean = "mean_truncated_fixed",
	vary_truncated_mean = "mean_truncated_vary",
	bayes_posterior = "mean_posterior",
	first_three_quarters = "mean_first_threeq",
	last_quarter = "mean_last_oneq",
	.intermediate = c("split_means", "first_three")
)
#################### STEP 4 ###########################
# declare the evaluators in file "evaluate/evaluators.R"
evaluators_list <- list(
	BIAS = "bias",
	rmSE = "RMSE"
)

#################### STEP 5 ###########################
# Declare truth in file "generate/truth_global.R"
# Declare knowledge in file "generate/knowledge_global.R"
# Declare how to store replications in file "utils/storage.R"

#################### STEP 6 ###########################
# Check near line 41 in file "evaluate/replicate.R"
# cbind should work if output from each iteration is vector or numeric
# t <- evaluate(methods_list, evaluators_list, params, B = 8000, cores = 10)

#################### STEP 7 ###########################
# vary params. If multiple variables, do expand.grid
vary_params <- list(
	sigma = seq(from = 0.01, to = 5, length.out = 20)
)
# this will store the results for each variation of the parameters
source("evaluate/vary_over_parameters.R", chdir = TRUE, local = TRUE)
results_list <- vary_over_parameters(vary_params, methods_list, evaluators_list, params, B = 5000, ncores = 12)
##### TODO: Check the code for multiple variables in vary_params

# combine all results from list into a single dataframe
library(dplyr)
result <- bind_rows(results_list)

#################### STEP 8 ###########################
# Optional: Write result to a csv file
# For longer dataframe, refer to next step
write.csv(result, file = "result.csv", row.names = FALSE)

#################### STEP 9 ###########################
# Plot the result as function of the parameter being varied
plot_var <- names(vary_params)[1]
library(tidyr)
library(ggplot2)
result <- result %>% pivot_longer(
		  all_of(1:(length(setdiff(names(methods_list), ".intermediate"))*length(evaluators_list))),
		  names_to = c("method", ".value"),
		  names_sep = "\\."
)
p <- ggplot(result, aes(x = get(plot_var), y = rmSE)) +
	geom_line(aes(group = method, color = method, linetype = method)) +
	labs(x = plot_var) +
	theme_bw()

print(p)
