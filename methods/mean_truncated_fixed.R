mean_truncated_fixed <- function(x, knowledge, ...) {
	lower <- knowledge[[".global"]][["lower_global"]]
	upper <- knowledge[[".global"]][["upper_global"]]
	stopifnot(!is.null(lower), !is.null(upper))
	stats::median(c(lower, upper, base::mean(x)))
}

