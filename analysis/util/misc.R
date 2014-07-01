push <- function(l, x) {
  assign(l, append(eval(as.name(l)), x), envir = parent.frame())
}

a <- list(1,2)
push("a", 3)
push("a", 44)
