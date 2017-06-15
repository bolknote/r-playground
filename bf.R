#!/usr/bin/env Rscript

library(methods)

BF <- setRefClass("BF",
	fields = c(
		program = "character",
		cells = "numeric",
		pointer = "numeric",
		buffer = "numeric",
		stack = "numeric",
		ip = "numeric"
	),
	methods = c(
		initialize = function(program) {
			program <<- program
			cells <<- rep.int(0, 30000)
			pointer <<- 1
			ip <<- 1
			buffer <<- numeric(0)
			stack <<- numeric(0)
		},
		operator.minus = function() {
			cells[pointer] <<- cells[pointer] - 1
		},
		operator.plus = function() {
			cells[pointer] <<- cells[pointer] + 1
		},
		operator.right = function() {
			pointer <<- pointer + 1
		},
		operator.left = function() {
			pointer <<- pointer - 1
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
				ip <<- match(']', program)

				if (is.na(ip)) {
					stop('End of loop is not found', call. = F)
				}
			} else {
				stack <<- c(ip, stack)
			}
		},
		operator.end = function() {
			ip <<- stack[1] - 1
			stack <<- stack[-1]
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
