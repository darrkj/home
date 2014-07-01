setClass("BMI", representation(weight = "numeric", 
                               size = "numeric"))

setMethod("show", "BMI",
          function(object) {
            cat("BMI =", 
                object@weight / (object@size ^ 2),
                " \n "
                )
            }
          )

myBMI <- new("BMI", weight = 85, size = 1.84)



setClass("MLBTeam", 
         representation(division = "factor", 
                        salary = "numeric"
                        )
         )

(Reds <- new("MLBTeam", division = "American", 
             salary = 18400000))