mean_first_threeq <- function(x, knowledge, ...) {
	t <- knowledge[[".intermediate"]][["first_three"]]
	stopifnot(!is.null(t))
	t
}
