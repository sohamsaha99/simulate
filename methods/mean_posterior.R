mean_posterior <- function(x, knowledge, ...) {
	data_sigma <- knowledge[[".global"]][["data_sigma"]]
	theta_sigma <- knowledge[[".global"]][["sigma"]]
	theta_mean <- knowledge[[".global"]][["theta_mean"]]
	stopifnot(!is.null(data_sigma), !is.null(theta_sigma), !is.null(theta_mean))
	xbar <- base::mean(x)
	n <- length(x)
	w1 <- n / data_sigma^2
	w2 <- 1 / theta_sigma^2
	w1 <- w1 / (w1 + w2)
	w2  <-  1 - w1
	w1 * xbar + w2 * theta_mean
}
