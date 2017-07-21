#!/usr/bin/env Rscript

bottles <- function(beer) {
	ifelse(beer == 0, "no bottles",
		ifelse(beer > 1, paste(beer, "bottles"),  "1 bottle")
	)
}

for (i in 99:1) {
	paste(bottles(i), "of beer") -> b

	cat(b, " on the wall, ", b, ".\n", sep = "")
	cat("Take one down and pass it around,", bottles(i - 1), "of beer on the wall.\n\n")
}

cat("No more bottles of beer on the wall, no more bottles of beer.\n")
cat("Go to the store and buy some more, 99 bottles of beer on the wall.\n")