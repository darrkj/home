# Creating an instance of the Scala interpreter

library(jvmr)
a <- scalaInterpreter()
# Defining functions
interpret(a,'
          import scala.io.Source.fromURL
          def fib( n: Int): Int = n match {
            case 0 | 1 => n
            case _ => fib( n -1) + fib( n-2)
          }
          ')
fib <- function(n){
  if( n > 1 ) fib(n-1) + fib(n-2)
  else n
}
system.time(a['fib(33)'])
system.time(fib(33))
# Defining a variable
interpret(a,"num") <- 3.4
# Printing the value of the variable
interpret(a,"num")
# Defining a variable using shortcut syntax
a["x"] <- "Welcome"
# Printing the value of the variable using shortcut syntax
a["x"]
# Using loops
interpret(a,"message") <- c("Hello","World","!")
interpret(a,"message")
interpret(a,'message.foreach{ x => println("<"+x+">") }',echo.output=TRUE)
# Assigning values to variables using the argument list
interpret(a,’
          val d = "Zero"
          val b = "${1}"
          val d2 = "Two"
          ’,"One")
# Illustrating the use of ’simplify’
c <- a['Range(0,100).toArray']