mean_truncated_vary <- function(x, knowledge, ...) {
	lower <- knowledge[[".local"]][["lower"]]
	upper <- knowledge[[".local"]][["upper"]]
	stopifnot(!is.null(lower), !is.null(upper))
	stats::median(c(lower, upper, base::mean(x)))
}
