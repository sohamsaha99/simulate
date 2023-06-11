first_three <- function(x, knowledge, ...) {
	t <- knowledge[[".intermediate"]][["split_means"]]
	stopifnot(!is.null(t))
	base::mean(t[1:3])
}
