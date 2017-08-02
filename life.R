library(methods)

Life <- setRefClass("Life",
    fields = c(
        life = "matrix",
        r = "numeric",
        c = "numeric"
    ),
    methods = c(
        initialize = function(rows, cols, ratio = .2) {
            r <<- rows
            c <<- cols
            life <<- matrix(as.integer(runif(rows * cols) < ratio), rows, cols)
        },
        echo = function(chr = '✿') {
            write.table(ifelse(life, chr, ' '), quote = F, row.names = F, col.names = F, sep = "")
        },
        U = function(life) rbind(life[-1,], life[1,]),
        D = function(life) rbind(life[r,], life[-r,]),
        L = function(life) cbind(life[,-1], life[,1]),
        R = function(life) cbind(life[,c], life[,-c]),
        step = function() {
            life.neighbors <- U(life) + D(life) + L(life) + R(life) +
                              U(L(life)) + U(R(life)) + D(L(life)) + D(R(life))
            life.new <- life

            # Any dead cell with exactly three live neighbours becomes a live cell
            life.new[life == 0 & life.neighbors == 3] <- 1
            # Any live cell with fewer than two or more than three live neighbours dies
            life.new[life == 1 & (life.neighbors < 2 | life.neighbors > 3)] <- 0

            life <<- life.new
        }
    )
)

# Clear entire screen
cat("\033[2J\033[H")

l <- Life$new(25, 60)

repeat {
    l$echo()
    cat("\nPress Ctrl-C to exit.")
    Sys.sleep(.3)
    l$step()

    # Clear current frame
    cat("\033[1A\033[1J\033[H")
}