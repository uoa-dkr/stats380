# functions:

# two types of function(same logical)
square <- function(x) x * x
square(5)
square(1:5)

squares <- function(x) {
  x * x
}
squares(5)

# both are Compound functions:
sum_square <- function(x) sum(squares(x))
sum_square(1:10)

sum_square_varianc <- function(x) {
  y = squares(x - mean(x))
  sum(y)
}
sum_square_varianc(1:10)

# Anonymous function:
(function(x) x * x)(1999:2021)[c(0,1,0,2)]
(1:length(runif(5)>0.5))[runif(5)>0.5] # 1st part is a seq and the second is indicator
# ()[] can be tricky:
(3:8)[c(0, 1, 1, 1, 1)] # when [] only exist 0 and 1 then it returns four 3's
(10:5)[c(1, 1, 0)]      # same logical return two 10's
(3:-2)[c(0, 5, 7, 3, 5)]   # but [] has other positive num it indicates its index return -1 na 1 -1

# a function which same as case_when and recode:
set.seed(2021/7/28)
runif(5) # care that the runif is diff from below
# which only consider that the parameter is logical
which(runif(5)>.025 & runif(5) < .975) # return the index which satisfy the condition(true)
which(c(rep(TRUE, 3), FALSE, rep(TRUE, 4))) # only return the true

# as.logical be false when input is 0(num), 'false'/'F'(chr)
as.logical(-0.0123456789)
as.logical(0.00000)
as.logical('F')
as.logical('false')
as.logical('T')

# refining the function:
whichs <- function(x) {
  seq(along = x)[!is.na(as.logical(x)) &
                   as.logical(x)]
}
whichs(c(0, 1, 0, 1))
whichs(c("false", "true"))
whichs(c(TRUE, NA, FALSE))

# compound function: always return last expression
{10; 20; 2021*1999} ; 10; 20; 2021*1999 # print each expression without the bracket
{x = 10 ; y = x^2; x + y} # but the value will be stored in the environment
{x = pi; x}

# if else function:
(if (x>0) y = sqrt(x) else y = -sqrt(-x))
(y <- if (x>0) sqrt(x) else -sqrt(-x))

# for loop: try to avoid looppppppp
s <- 0
for (elt in seq(1:5)){  # elt stands for element, run num of elt in x 
  s = s + 1 
}
s

# next in loop:
for(elt in c(NA, TRUE, FALSE, c(1, 11, 4))) {
  y = 1:10
  if (elt %in% y){ # return f t f t f t 
    s = s + 1
    next   # if the condition is true then jump to next element, otherwise do next argument s=s-1
  }
  s = s - 1
}
s

# break in loop:
# if the condition is F in this eg, loop will be terminated
for(elt in c(3, TRUE, FALSE, c(1, 11, 4))) {
  y = 1:10
  if (elt %in% y){
    s = s + 1
  }
  else {
    break 
  }
}
s


# while loop: return 
remainder <- 22
divisor <- 5
wholes <- 0
while (remainder >= divisor) {
  remainder = remainder - divisor
  wholes = wholes + 1
}
c(wholes, remainder)


# A Square Root Algorithm:
# By averaging the bigger and smaller values we get a value which is closer to √x than either of them
guesses <- numeric(10) 
sqrts <- 2
g <- 1
for (i in 1:10) {
  g = .5 * (g + sqrts/g)
  guesses[i] = g
}
guesses-print(sqrt(2), digits = 11)

# improved square root function:
root = function(x) { 
  g=1
  while(abs(1 - g^2/x) > 1e-10)
    g = .5 * (g + x/g)
g }
root(2) - sqrt(2)


# Evaluation of Functions
sumsq = function(x, about = mean(x)){
  sum((x - about)^2)
}
sumsq(1:10)
sumsq(1:10, 1)

sumsq = function(x, about = mean(x)) {
  x = x[!is.na(x)]
  sum((x - about)^2)       # 'about' will not be computed until we need 'about'
}                          # so, x = x[!is.na(x)] can be done prior to doing 'about'



x <- 'abcd'
strrev = function(x){
  paste(substring(x, nchar(x):1, nchar(x):1), collapse = "")
}
strrev(x) # cannot reverse by x[4:1] bc it is string


# Vectorizing Functions: to deal with if a var is a vector(eg: c('abcd', 'dkr'))

# 1. Vectorizing Functions – Inline Approach
strrev = function(x) {
  ans = character(length(x))
  for(i in 1:length(x))
    ans[i] = paste(substring(x[i], nchar(x[i]):1, nchar(x[i]):1), collapse = "")
  ans 
}
strrev(c('abcd', 'dkr'))

# 2. Helper Function Approach
strrev1 = function(x){
  paste(substring(x,nchar(x):1,nchar(x):1),collapse = "")
}
strrev = function(x) {
  ans = character(length(x))
  for(i in 1:length(x))
    ans[i] = strrev1(x[i])
  ans
}
strrev(c('abcd', 'dkr'))


# Vectorizing a Two-Argument Numeric Function
# Assuming that f1 is a numerical function that is not vectorized, the following code creates a vectorized version
f = function(x, y) {
  n = max(length(x), length(y))
  if (length(x) < n) x = rep(x, length = n)
  if (length(y) < n) y = rep(y, length = n)
  ans = numeric(n)
  for(i in seq(along = ans))
    ans[i] = f1(x[i], y[i])
  ans
}


# Vectorized: 
# 1. Vectorize() allows to vectorize a function become vectorized function
vec_strrev <- Vectorize(strrev)
vec_strrev(c('abcd', 'dkr'))   # but output a vector

# 2. lapply() applies a given function to each element of a list and returns the computed values in a list
lapply(list('abcd', 'dkr'), strrev)
lapply(c('abcd', 'dkr'), strrev)
unlist(lapply(c('abcd', 'dkr'), strrev))   # get rid of list
lapply(modelr::sim1, lag, 5, default = 100)# allow additional argument

# 3. sapply() behaves just like the lapply, but output a vector or array
sapply(c('abcd', 'dkr'), strrev)
sapply(list('abcd', 'dkr'), strrev)
sapply(list(2, 3), sqrt)
sapply(modelr::sim1, lead, 5, default = 100)# same logical

# 4. tapply() it allows function work in groups(aggregation function)
tapply(starwars$height, starwars$sex, max, na.rm = T)

starwars %>%
  group_by(sex) %>% 
  summarise(max = max(height, na.rm = T))

#  Computing p Values
# 2P(Tn1+n2−2 < −|tval|):

ttest = function(y1, y2) {
  n1 = length(y1)
  n2 = length(y2)
  tval = diff.means(y1, y2) / pooled.se(y1, y2)  # t-value
  pval = 2 * pt(- abs(tval), n1 + n2 - 2)        # p-value
  list(t = tval, df = n1 + n2 - 2, pval = pval)
}


# Abandoning Computations: stop will terminate the code
fake.fun = function(x)
  if (x > 10) stop("bad x value") else x
fake.fun(-1)
fake.fun(11)

# Warnings: warning will keep running but give us the warning
if (any(x < 2))
  warning("negative weights encountered")

# calculate the execution time
system.time(for (i in 1:100) x = x + 1)
system.time(sapply(x, function(x) x + 1))

# Invisible Return Values: do not print when they are not assigned
invisible_function = function(x){
  invisible(x^2)
}
invisible_function(1:10)         # nothing will be executed
y <- invisible_function(1:10); y # when functions return values which can be assigned

# useful functions:
which(c(TRUE,FALSE,FALSE,TRUE)) # return indices of the true values
which.max(c(6:10,6:10))  # Index of the (first) maximum (minimum).
diff(c(1,3,6,4)) # Consecutive (or suitably lagged) differences
double(1) == numeric(1)
vector('double', 1)
diff(c(1,3,6,4), lag = 2) # start at 6, 6-1 and 4-3

r <- rbinom(20,10,0.5);r 
# diff between below two is that table only exhibit exist, whereas tabulate shows 1:highest
table(r)    # return frequencies with regard to its value
tabulate(r) # same logical but diff display
tabulate(r, nbins = 11) # 11th means frequencies until 11
