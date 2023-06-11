# function for replicating the simulation process
# and store outputs
replication <- function(methods_list, params, truth_global, B, cores = 1) {
	knowledge <- list()
	source("generate/knowledge_global.R", chdir = TRUE, local = TRUE)
	# Primary oracle knowledge based on fixed parameters
	knowledge[[".global"]] <- knowledge_global(params)
	replicate_once <- function(methods_list, params, knowledge, truth_global) {
		r <- generate_data(params)
		dat <- r$data
		truth_combined <- list(.global = truth_global, .local = r$truth_local)
		# Also get oracle knowledge from data generation method
		knowledge[[".local"]] <- r$knowledge_local
		# iterate through intermediate helper functions and update knowledge
		for(i in methods_list[[".intermediate"]]) {
			knowledge[[".intermediate"]][[i]] <- get(i)(dat, knowledge = knowledge)
		}
		# iterate through all methods
		estimation <- list()
		for(m in names(methods_list)) {
			if(m == ".intermediate") next
			estimation[[m]] <- storage(
				output = get(methods_list[[m]])(dat, knowledge = knowledge),
				truth_combined = truth_combined
			)
		}
		estimation
	}
	if(cores == 1) {
		source("utils/storage.R", chdir = TRUE, local = TRUE)
		source("generate/generate_data.R", chdir = TRUE, local = TRUE)
		for(m in names(methods_list)) {
			if(m == ".intermediate") {
				for(i in methods_list[[m]]) {
					source(paste0("methods/intermediate/", i, ".R"), chdir = TRUE, local = TRUE)
				}
			} else {
				source(paste0("methods/", methods_list[[m]], ".R"), chdir = TRUE, local = TRUE)
			}
		}
		# no progress bar
		results_temp <- lapply(as.list(1:B), function(i) replicate_once(methods_list, params,  knowledge, truth_global))
	} else {
		library(parallel)
		cl <- makeCluster(min(cores, detectCores() - 1))
		clusterSetRNGStream(cl, NULL)
		clusterEvalQ(cl, source("utils/storage.R", chdir = TRUE, local = TRUE))
		clusterEvalQ(cl, source("generate/generate_data.R", chdir = TRUE, local = TRUE))
		clusterExport(cl, c("params", "knowledge", "truth_global"), envir = environment())
		clusterExport(cl, "methods_list", envir = environment())
		clusterExport(cl, c("replicate_once"), envir = environment())
		clusterEvalQ(cl,
			for(m in names(methods_list)) {
				if(m == ".intermediate") {
					for(i in methods_list[[m]]) {
						source(paste0("methods/intermediate/", i, ".R"), chdir = TRUE, local = TRUE)
					}
				} else {
					source(paste0("methods/", methods_list[[m]], ".R"), chdir = TRUE, local = TRUE)
				}
			}
		)
		results_temp <- parLapply(cl, as.list(1:B), function(i) replicate_once(methods_list, params, knowledge, truth_global))
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
evaluate <- function(methods_list, evaluators_list, params, B, cores = 1) {
	# find truth based on parameters. This is global in the sense that
	# it does not depend on replication.
	source("generate/truth_global.R", chdir = TRUE, local = TRUE)
	truth_global <- truth_global(params)
	# replicate the process and store
	replication <- replication(methods_list, params, truth_global, B, cores)
	# read all evaluators
	source("evaluate/evaluators.R", chdir = TRUE, local = TRUE)
	# evaluate on replicated output
	performace <- list()
	for(m in names(methods_list)) {
		if(m == ".intermediate") next
		performace[[m]] <- list()
		for(e in names(evaluators_list)) {
			performace[[m]][[e]] <- get(evaluators_list[[e]])(replication[[m]], truth_global)
		}
	}
	data.frame(performace)
}
