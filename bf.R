#!/usr/bin/env Rscript

library(methods)

BF <- setRefClass("BF",
	fields = c(
		program = "character",
		cells = "numeric",
		pointer = "numeric",
		ip = "numeric",
		buffer = "numeric",
		jumps = "numeric"
	),
	methods = c(
		initialize = function(program) {
			program <<- program
			cells <<- rep.int(0, 30000)
			pointer <<- 1
			ip <<- 1
			buffer <<- numeric(0)
			jumps <<- numeric(0)
		},
		operator.minus = function() {
			cells[pointer] <<- if (cells[pointer] == 0) 255 else cells[pointer] - 1
		},
		operator.plus = function() {
			cells[pointer] <<- if (cells[pointer] == 255) 0 else cells[pointer] + 1
		},
		operator.right = function() {
			pointer <<- if (pointer == 30000) 1 else pointer + 1
		},
		operator.left = function() {
			pointer <<- if (pointer == 1) 30000 else pointer - 1
		},
		operator.write = function() {
			cat(intToUtf8(cells[pointer]))
		},
		operator.read = function() {
			if (length(buffer) == 0) {
				cat("Enter character then press 'Enter': ")
				input <- utf8ToInt(scan("stdin", w = 'character', n = 1, quiet = T))
				buffer <<- c(buffer, input[-1])
				char <- input[1]
			} else {
				char <- buffer[1]
				buffer <<- buffer[-1]
			}

			cells[pointer] <<- char
		},
		operator.begin = function() {
			if (cells[pointer] == 0) {
				ip <<- as.numeric(names(jumps)[match(ip, jumps)])

				if (is.na(ip)) {
					stop('End of loop is not found', call. = F)
				}
			}
		},
		operator.end = function() {
			ip <<- jumps[[as.character(ip)]] - 1
		},
		createJumpsMap = function() {
			filter <- program %in% c('[', ']')
			cycles <- setNames(program[filter], (1:length(program))[filter])
			stack <- c()

			for (n in names(cycles)) {
				if (cycles[[n]] == '[') {
					stack <- c(as.numeric(n), stack)
				} else {
					jumps[n] <<- stack[1]
					stack <- stack[-1]
				}
			}
		},
		run = function() {
			methods <- c(
				"+" = .self$operator.plus,
				"-" = .self$operator.minus,
				">" = .self$operator.right,
				"<" = .self$operator.left,
				"[" = .self$operator.begin,
				"]" = .self$operator.end,
				"." = .self$operator.write,
				"," = .self$operator.read
			)

			program <<- unlist(strsplit(program, ""))
			program <<- program[program %in% names(methods)]
			len <- length(program)

			createJumpsMap()

			repeat {
				methods[[program[ip]]]()

				if (ip >= len) {
					break
				}
				ip <<- ip + 1
			}
		}
	)
)

program <- commandArgs(trailingOnly = T)[1]
BF$new(program)$run()
