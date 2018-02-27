#!/usr/bin/env Rscript
library(methods)

init <- function() {
	code <<- list(
		'+-'	= 'A',
		'-+++'	= 'B',
		'-+-+'	= 'C',
		'-++'	= 'D',
		'+'		= 'E',
		'++-+'	= 'F',
		'--+'	= 'G',
		'++++'	= 'H',
		'++'	= 'I',
		'+---'	= 'J',
		'-+-'	= 'K',
		'+-++'	= 'L',
		'--'	= 'M',
		'-+'	= 'N',
		'---'	= 'O',
		'+--+'	= 'P',
		'--+-'	= 'Q',
		'+-+'	= 'R',
		'+++'	= 'S',
		'-'		= 'T',
		'++-'	= 'U',
		'+++-'	= 'V',
		'+--'	= 'W',
		'-++-'	= 'X',
		'-+--'	= 'Y',
		'--++'	= 'Z',
		'+----'	= '1',
		'++---'	= '2',
		'+++--'	= '3',
		'++++-'	= '4',
		'+++++'	= '5',
		'-++++'	= '6',
		'--+++'	= '7',
		'---++'	= '8',
		'----+'	= '9',
		'-----'	= '0',
		'++++++'= '.',
		'+-+-+-'= ',',
		'---+++'= ':',
		'-+-+-+'= ';',
		'+----+'= "'",
		'+-++-+'= '"',
		'-++++-'= '—',
		'-++-+'	= '/',
		'++--++'= '?',
		'--++--'= '!',
		'-+++-'	= ' ',
		'+--+-+'= '@'
	)

	setGeneric('toMorseStr', function(str) standardGeneric('toMorseStr'))
	setMethod('toMorseStr', 'character', function (str) {
		code.rev <<- setNames(names(code), code)
		str.vec <<- unlist(strsplit(toupper(str), split=NULL))
		morse <<- lapply(str.vec, function(v) code.rev[[v]])

		paste0(c(morse, ''), collapse=".")
	})

	morse <- setClass("morse",
		representation(stack = "character"),
		prototype(stack = c())
	)

	setMethod('show', 'morse', function(object)
		cat(code[[paste0(rev(object@stack), collapse="")]])
	)

	setMethod('-', 'morse', function (e1) {
		e1@stack <- c(e1@stack, '-')
		e1
	});

	setMethod('+', 'morse', function (e1) {
		e1@stack <- c(e1@stack, '+')
		e1
	});

	setMethod('+', signature('morse', 'morse'), function (e1, e2) {
		show(e1)
		+e2
	});

	setMethod('-', signature('morse', 'morse'), function (e1, e2) {
		show(e1)
		-e2
	});


	morse()
}

. <- init()

# cat(toMorseStr("What hath God wrought?"))

+--.++++.+-.-.-+++-.++++.+-.-.++++.-+++-.--+.---.-++.-+++-.+--.+-+.---.++-.--+.++++.-.++--++.
