mean_truncated <- function(x, knowledge, ...) {
	lower <- knowledge[["lower"]]
	upper <- knowledge[["upper"]]
	median(c(lower, upper, base::mean(x)))
}
