# num go col by default
matrix(1:6, nrow = 3, nc = 2)

# arrange the vector of values by row(default is col)
x <- matrix(1:6, nr = 3, ncol = 2, byrow = T); x
y <- matrix(1:12, nr = 3);y

# Determining Matrix Dimensions
nrow(x)
ncol(x)
dim(x)

# row() and col() works differently see output
row(x) # all num in 1st row is 1, 2rd row is 2, etc
col(x) # all num in 1st col is 1, 2rd col is 2, etc
row(y) # with same matrix dimension
col(y)

# the recycling rule can be applied
matrix(1, nr=3, nc=2)
matrix('1', nr=2, nc=3)

# R can work out the number of rows in a matrix given the number of columns and the elements, vice versa
matrix(1:6, nr = 3)
matrix(1:6, nc = 3)


# allow us to change the num of cols and the num of rows
# care that the position of some values will changes
x;dim(x);dim(x) <- c(2,3);x
x;dim(x) = c(1,6);x
x;dim(x) = c(3, 2);x

# cbind: bind cols to a matrix(recycling rule)
cbind(1:3, 4:6)
cbind(a = 1:4, b = 3:6)
# rbind: bind rows to a matrix(recycling rule)
rbind(1:3, 4:6)
rbind('1', 3:6) # matrix only exist one type of format, so if two formats occur, coerce one of them to transpose

# Naming a matrix (same as names()) # bc matrix is column-major order storage, have to name col 1st
dimnames(x) = list(c('a', 'b', 'c'), month.name[2:3]);x # dimname has to be a list
dimnames(x)
rownames(x) = month.abb[10:12];x # we can change as well
colnames(y) = LETTERS[2:5];y     # or dimnames(y)[[2]] = ...
unname(y);colnames(y) <- NULL;y  # set null means unname

# extract element from a matrix
# c(1,3) means row1 and row3, 1:2 means col1 and col2, then 11,12,31,32
x;x[c(1, 3),1:2]

# care that if index only contain num
y[1,] # 1st row
y[,1:2] # 1st and 2nd cols

# bc matrix is a type of vector, which output a value with a valid input, but remb col-major
y[10];x[2] # index of each matrix, index of 10 in y and 2 in x


# also we can assign a new value to matrix
y[1,2] <- 66;y
y[3,] <- 10:13;y      # change row
y[2:1, 1:3] = 21:22;y # change a specific row and col, notice that the order is 21 11 23 13

# basic computation with matrix(matrix is a special case of a vector)
sum(x)
x + 1 - 1
x + x^2
x + 3:2 # col-major
x * 6
matrix(1:12, nr = 3) %*% matrix(1:12, nr = 4)

# create a 4 × 4 tridiagonal matrix with diagonal values being 2 and the non-diagonal elements being 1
tridiagonal = matrix(0, nrow = 4, ncol = 4);tridiagonal
tridiagonal[row(tridiagonal) == col(tridiagonal)] = 2
tridiagonal[abs(row(tridiagonal) - col(tridiagonal)) == 1] = 1;tridiagonal


# find the mean of each row without utilize a explicitly loop, turn out it outputs the overall mean
rm = numeric(nrow(x));rm
rm[1:nrow(x)]=sapply(1:nrow(x),function(i) mean(x[i,]));rm


# apply function can be used to compute row or column summaries
x
apply(x, 1, mean) # 1 means each row
y
apply(y, 1, sd)
apply(y, 1, range) # recall that range return min and max from the input range

col_max=apply(y, 2, max);col_max  # 2 means each col
names(col_max) = 1:4;col_max # get rid of named vector, attention that col_max is a vector rather a matrix
apply(x, 2, function(x) sum((x - mean(x))^2))


# sweep function used for compute between matrices
y;apply(y, 2, mean);sweep(y, 2, apply(y, 2, mean)) # default is subtraction

# we firstly compute the mean of 2 cols return 2 10 then do x %% (2 10)
sweep(cbind(1:3, 9:11), 2, apply(cbind(1:3, 9:11), 2, mean), '%%') # 1st col divided by 2, 2nd col divided by 10

# creating a matrix
mortality = matrix(c(25.3, 25.3, 18.2, 18.3, 16.3,
                     32.1, 29.0, 18.8, 24.3, 19.0,
                     38.8, 31.0, 19.3, 15.7, 16.8,
                     25.4, 21.1, 20.3, 24.0, 17.5), nrow = 4, byrow = TRUE)

dimnames(mortality) =
  list(Region = c("Northeast", "North Central","South", "West"),
       "Father's Education (Years)" =
                c("<=8", "9-11", "12", "13-15", ">=16"))
dimnames(mortality)
rownames(mortality)
colnames(mortality)


r <- mortality-mean(mortality);r
rm <- apply(r, 1, mean);rm
r <- sweep(r, 1, rm);r
cm <- apply(r, 2, mean);cm
r <- sweep(r, 2, cm);r     # same as outer(r, cm, '-'), more later
round(apply(r, 1, mean), 4)
round(apply(r, 2, mean), 4)

# Packaging as a Function 
twoway = function(y) {
  mu = mean(y)
  y = y - mu
  alpha = apply(y, 1, mean)
  y = sweep(y, 1, alpha)
  beta = apply(y, 2, mean)
  y = sweep(y, 2, beta)
  list(overall = mu, rows = alpha, 
       cols = beta, residuals = y)
}
twoway(mortality)


# outer function return a matrix
# eg 2:4, 6:9 --> 2*6,27,28,29,36,37,38,39,46,47,48,49 
2:4 %o% 6:9             # default outer function
outer(2:4, 6:9)         # default is multiplication, and col-major no long apply
outer(1:4, 1:9, '+')
outer(1:4, 1:9, 'paste')
outer(1:4, 1:9, function(a, b) (a+b)^2) # also can be applied by a function

# easy outer() implementation
nearest = function(x, y){
  x[apply(outer(x, y, function(x, y) abs(x - y)), 2, which.min)]
}
set.seed(814)
runif(4)
set.seed(814)
nearest(0:10/10, round(runif(4), 4))


# Matrix Transposes, two ways to do that, but works different
x <- matrix(1:6, nr = 3, ncol = 2, byrow = T); x
dim(x) = c(2, 3);x            # it's like a coercion(follow the col-major rule)

x <- matrix(1:6, nr = 3, ncol = 2, byrow = T); x
Transposes <- t(x);Transposes # 1st row -> 1st col, 1st col -> 1st row(no long apply col-major rule)


# Matrix Diagonals: Note that diag(A) is equivalent to A[row(A)==col(A)]
x;diag(x) # bc the diagonal line is 1 4, then return 1 4


# Solving Systems of Linear Equations: note that the matrix input must be a square, 2x2 5x5 10x10 ...
# 1. linear combination
solve(2, 4) 

# 2. Given a non-singular n × n matrix A and vector b of length n, the linear system Ax = b
solve(matrix(c(1, 3, 5, 7), nr=2), matrix(7:8, nr=2))
solve(matrix(sample(1:100, 9), nr=3), 1:3)

# 3. The expression solve(A) computes the inverse of A (because the default value for b is a suitable identity matrix)
solve(matrix(sample(1:100, 9), nr=3))
solve(matrix(runif(9, -1, 1), nc=3))

log2(4)
log10(4)/log10(2)
