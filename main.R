#################### STEP 1 ###########################
# Set parameters, including the ones varied
params <- list(
	mu = 1,
	sigma = 7.5
)
more_params <- list(
    n = 100
)

#################### STEP 2 ###########################
# Declare data generation function in file "generate/generate_data.R"
# If data generation is not completely random but depends on
# something that chamges (for example, 1000 csv files), then modify
# the file "evaluate/replicate.R" : Look for lapply and parLapply

#################### STEP 3 ###########################
# Declare the methods in directory "methods/"
# Form: list(a = "b").
# "a": to be used as column names and in plots
# "b": file b.R contains the definition in a function b()
methods_list <- list(
	"ordinary_mean" = "mean",
	"truncated" = "mean_truncated"
)
#################### STEP 4 ###########################
# declare the evaluators in file "evaluate/evaluators.R"
evaluators_list <- list(
	BIAS = "bias",
	rmSE = "RMSE"
)

#################### STEP 5 ###########################
# Declare truth in file "generate/truth.R"
# Declare knowledge in file "generate/knowledge.R"
# Declare how to store replication in file "utils/storage.R"

#################### STEP 6 ###########################
# Check near line 36 in file "evaluate/replicate.R"
# cbind shoudl work if output from each iteration is vector or numeric
source("./evaluate/replicate.R", chdir = TRUE, local = TRUE)
# t <- evaluate(methods_list, evaluators_list, params, more_params, B = 8000, cores = 10)

#################### STEP 7 ###########################
# vary params. If multiple variables, do expand.grid
vary_params <- list(
	sigma = seq(from = 0.01, to = 10, length.out = 20)
)
# this will store the results for each variation of the parameters
vary_length <- length(vary_params[[1]])
results_list <- list()
for(iter_num in 1:vary_length) {
	for(v in names(vary_params)) {
		if(v %in% names(params)) {
			params[[v]] <- vary_params[[v]][[iter_num]]
		} else if(v %in% names(more_params)) {
			more_params[[v]] <- vary_params[[v]][[iter_num]]
		} else {
			print("Warning: variable not present...")
		}
	}
	t <- evaluate(methods_list, evaluators_list, params, more_params, B = 500, cores = 12)
	for(v in names(vary_params)) {
		t[[v]] <- vary_params[[v]][[iter_num]]
	}
	results_list[[iter_num]] <- t
}
##### TODO: Implement nested parallel. Maybe impractical

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
col_indices_to_long <- 1:(length(methods_list)*length(evaluators_list))
result <- result %>% pivot_longer(
		  all_of(col_indices_to_long),
		  names_to = c("method", ".value"),
		  names_sep = "\\."
)
p <- ggplot(result, aes(x = get(plot_var), y = rmSE)) +
	geom_line(aes(group = method, color = method, linetype = method)) +
	labs(x = plot_var) +
	theme_bw()

print(p)
