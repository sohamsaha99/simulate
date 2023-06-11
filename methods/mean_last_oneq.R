mean_last_oneq <- function(x, knowledge, ...) {
	t <- knowledge[[".intermediate"]][["split_means"]]
	stopifnot(!is.null(t))
	t[4]
}
