
# CAR - head
# CDR - tail
# CONS - create cell with head and tail
# EQ - equality
# ATOM


psil <- function() {
  namespace <- list('cdr', 'car', 'x', 'define', 'print')
  symbols <- list('+', '-', '*', '/', '%')
  options(show.error.messages = F)
  x <- TRUE
  while(x) {
    y <- readline(prompt = "psil> ")
    if ( y == 'quit') {
      x <- FALSE; print('I have enjoyed our time together.')
    } else {
      y <- gsub('(', '', y, fixed = TRUE)
      y <- gsub(')', '', y, fixed = TRUE)
      y <- strsplit(y, ' ')[[1]]
      if (y[1] %in% namespace) {
        try(print(eval(call(y[1], as.numeric(y[2])))))
      } else {
        print('I do not understand what you are asking')
      }
    }
  }
  options(show.error.messages = T)
}

# Need to tokenize, spaces and s expressions
token <- function(a) {
  a <- gsub('(', '( ', a, fixed = TRUE)
  a <- gsub(')', ' )', a, fixed = TRUE)
  strsplit(a, ' ')[[1]]
}

sexp <- function(a) {
  x <- which(token(a) == '(')
  y <- which(token(a) == ')')
  list(open = x, close = y)
}

# what are the atomic units
# Numbers
# math operations
# nil - ()
# cons car cdr etc

atomic.type <- function(a) {
  if (is.numeric(a))
}




library(testthat)

numb <- function(x) {
  grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", x)
}




op <- function(x) {
  x %in% c('+', '-', '*', '/', '%')
}




nil <- function(x) {
  x == 'nil' | x == '()'
}



base <- function(x) {
  x %in% c('cdr', 'car')
}



# Test cases
n('29')
n('2.1')
n('-12.311')
n('2.')
n('.32')

op('+')
op('--')
op(4)
op('f%')

nil('nil')
nil('()')
nil(5)







show_condition <- function(code) {
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
  )
}


f <- function() stop("!")
tryCatch(f(), error = function(e) 1)
#> [1] 1
withCallingHandlers(f(), error = function(e) 1)



show_condition(as.numeric(1))
show_condition(as.numeric('a'))
show_condition(as.numeric(|))

show_condition(stop("!"))





options(show.error.messages = T)

options(show.error.messages = F)
stop('k')
y <- '(as.numeric, |)'
y <- gsub('(', '', y, fixed = TRUE)
y <- gsub(')', '', y, fixed = TRUE)
y <- strsplit(y, ', ')[[1]]


xx <- try(eval(call(y[1], y[2])), silent = T))
suppressWarnings(try(log('a'), silent = T))

geterrmessage()[1]

tt <- suppressWarnings(tryCatch(eval(call(y[1], y[2])),error=function(e) e, warning=function(w) w))

if (any(class(tt) == "simpleWarning")) {
  print(tt[1])
} else if ()
t2 <- eval(call(y[1], y[2]))
geterrmessage()[1]
#

withCallingHandlers(5, message = function(e) stop(e))

tt <- suppressWarnings(tryCatch(as.numeric('a'),error=function(e) e, warning=function(w) w))

options(show.error.messages = T)
suppressWarnings()




f2 <- function(x) {
  try(log(x), silent = T)
  10
}
f2("a")

x <- try(as.numeric(||), silent = TRUE)
geterrmessage()
stop('k')
geterrmessage()[1]




