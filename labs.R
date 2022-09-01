# lab1
# display all data in global environment:
dkr <- 1
obj <- objects();obj
obj[1:2]

# remove the data form global environment:
rm(obj)
objects()

# and we can remove all data:
rm(list = objects()) # nothing left
objects() # The character(0) produced here is R’s way of showing a zero-length character vector.

# run n time between x and y can be pos and neg, actually a random uniform distribution.
runif(10, 1, 2.9)

# plots
plot(dnorm(1:15, 5))
plot(dpois(1:15, 5))
plot(dchisq(1:15, 5))


# ***************************************************
# lab2
seq(1000, 0)
seq(-100, -1)
rep(seq(1:21), rep(1:3, 7))
seq(1, 100)*c(1,-1)
factorial(100)
(1:100)^(1:2)

# pick a random num from a range, but if not explicitly say how many, it will return a random range
sample(1:10,1)
sample(c(1, 5, 10))
sample(c(sample(-100:100, 27), rep(NA, 3)))




















