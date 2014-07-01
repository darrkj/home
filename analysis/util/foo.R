foo = function(x){ 
  if(x > 0) salutation = "Hello" else
    salutation = "Goodbye"
  salutation
}

foo(1)


p <- function() runif(1)



queue <- function(x) {
  
}

x <- new.env()
assign('q', 3, x)

queue <- function(x, l) assign('q', l, new.env())

enqueue <- function(x, y) assign('q', c(x, get('q', x)), x)



# An empty function for Comments
Comment <- function(`@Comments`) { invisible() }

#### Comments ####
Comment( `
        
        Put anything in here except back-ticks.
        
        api_idea <- function() {
          return TRUE
        }
        
        #Just to show api_idea isn't really there...
        print( api_idea )
        
        `)
####

#### Code. ####
foo <- function() {
  print( "The above did not evaluate!")
}
foo()