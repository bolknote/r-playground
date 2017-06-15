#!/usr/bin/env Rscript

N <- function (n, k) {
	if (n == 1)
		ifelse(k >= 0 & k <= 9, 1, 0)
	else
		sum(sapply(0:9, function (l) N(n-1, k-l)))
}

lucky <- function (n) {
	0 -> s
	for (k in 0:999) {
		N(3, k) -> v

		if (v == 0) {
			break
		}

		s + v * v -> s
	}

	s
}

print (lucky(3))