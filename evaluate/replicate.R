# function for replicating the simulation process
# and store outputs
replication <- function(methods_list, params, more_params, truth, B, cores = 1) {
	source("generate/knowledge.R", chdir = TRUE, local = TRUE)
	knowledge <- knowledge(params)
	replicate_once <- function(methods_list, params, more_params, knowledge, truth) {
		dat <- generate_data(params, more_params)
		estimation <- list()
		for(m in names(methods_list)) {
			estimation[[m]] <- storage(output = get(methods_list[[m]])(dat, knowledge = knowledge), truth = truth)
		}
		estimation
	}
	if(cores == 1) {
		source("utils/storage.R", chdir = TRUE, local = TRUE)
		source("generate/generate_data.R", chdir = TRUE, local = TRUE)
		# read all methods
		for(m in methods_list) {
			source(paste0("methods/", m, ".R"), chdir = TRUE, local = TRUE)
		}
		# no progress bar
		results_temp <- lapply(as.list(1:B), function(i) replicate_once(methods_list, params, more_params, knowledge, truth))
	} else {
		library(parallel)
		cl <- makeCluster(min(cores, detectCores() - 1))
		clusterSetRNGStream(cl, NULL)
		clusterEvalQ(cl, source("utils/storage.R", chdir = TRUE, local = TRUE))
		clusterEvalQ(cl, source("generate/generate_data.R", chdir = TRUE, local = TRUE))
		clusterExport(cl, c("params", "more_params", "knowledge", "truth"), envir = environment())
		clusterExport(cl, "methods_list", envir = environment())
		clusterExport(cl, "replicate_once", envir = environment())
		clusterEvalQ(cl, for(m in methods_list) source(paste0("methods/", m, ".R"), chdir = TRUE, local = TRUE))
		results_temp <- parLapply(cl, as.list(1:B), function(i) replicate_once(methods_list, params, more_params, knowledge, truth))
		stopCluster(cl)
	}
	results <- list()
	for(m in names(methods_list)) {
		results[[m]] <- do.call(cbind, Map(f = function(x) x[[m]], results_temp))
	}
	results
}

# function for evaluating a list of methods,
# evaluated by a list of evaluators
evaluate <- function(methods_list, evaluators_list, params, more_params, B, cores = 1) {
	# find truth based on parameters
	source("generate/truth.R", chdir = TRUE, local = TRUE)
	truth <- truth(params)
	# replicate the process and store
	replication <- replication(methods_list, params, more_params, truth, B, cores)
	# read all evaluators
	source("evaluate/evaluators.R", chdir = TRUE, local = TRUE)
	# evaluate on replicated output
	performace <- list()
	for(m in names(methods_list)) {
		performace[[m]] <- list()
		for(e in names(evaluators_list)) {
			performace[[m]][[e]] <- get(evaluators_list[[e]])(replication[[m]], truth)
		}
	}
	data.frame(performace)
}
