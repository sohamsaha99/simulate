# generate knowledge based on parameters
# which may be useful in some methods
# that require additional parameters.
# return list of all such info
# In methods, this is accesibly by knowledge[[".global"]]
knowledge_global <- function(params) {
	nu <- params[["nu"]]
	mu <- params[["mu"]]
	delta <- params[["delta"]]
	list(
		lower_global = nu + mu - delta,
		upper_global = nu + mu + delta,
		theta_mean = nu + mu,
		data_sigma = 1,
		sigma = params[["sigma"]]
	)
}
