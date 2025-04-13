#!/usr/bin/env Rscript

# At a banquet, n spies are seated around a circular table. Each spy
# independently and randomly chooses one of their two immediate
# neighbors (either left or right) and secretly poisons their wine
# glass. What is the expected number of spies who survive?

library(parallel)

expected_survivors <- \(n, num_experiments, num_cores) {
    if (n == 1) return(1)
    
    experiments_per_core <- ceiling(num_experiments / num_cores)
    
    process_chunk <- \(chunk_size) {
        signs <- matrix(sample(c(1, -1), n * chunk_size, replace = TRUE), ncol = n)
        
        base_pos <- matrix(rep(1:n, each = chunk_size), nrow = chunk_size)
        positions <- (base_pos + signs) %% n
        
        num_poisoned <- apply(positions, 1, \(x) length(unique(x)))
        n - num_poisoned
    }
    
    results <- mclapply(rep(experiments_per_core, num_cores), process_chunk, mc.cores = num_cores)
    mean(unlist(results)[1:num_experiments])
}

num_experiments <- 1e5
n_values <- 2:20
num_cores <- detectCores()

results <- data.frame(
    n = n_values,
    expected = ifelse(n_values == 1, 1, ifelse(n_values == 2, 0, n_values / 4)),
    computed = sapply(n_values, \(n) expected_survivors(n, num_experiments, num_cores))
)

results$error_pct <- ifelse(results$expected == 0, results$computed * 100,
                           (results$computed - results$expected) / results$expected * 100)

cat("n\tExpected\tComputed\tError (%)\n")
cat("--\t--------\t--------\t-------\n")

for(i in 1:nrow(results)) {
    cat(sprintf("%d\t%.4f\t\t%.4f\t\t%+.2f %%\n",
        results$n[i],
        results$expected[i],
        results$computed[i],
        results$error_pct[i]
    ))
}
