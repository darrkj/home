require(doSMP)
workers <- startWorkers(8) # My computer has 2 cores
registerDoSMP(workers)
 
# create a function to run in each itteration of the loop
check <-function(n) {
  for(i in 1:1000)
	{
		sme <- matrix(rnorm(1000), 10,10)
		solve(sme)
	}
}
 
 
times <- 50	# times to run the loop
 
# comparing the running time for each loop
system.time(x <- foreach(j=1:times ) %dopar% check(j))  #  2.56 seconds  (notice that the first run would be slower, because of R's lazy loading)
system.time(for(j in 1:times ) x <- check(j))  #  4.82 seconds
 
# stop workers
stopWorkers(workers)
