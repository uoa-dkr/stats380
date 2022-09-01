# Q1

# a
seq(8.7, 29.7, 3)

# b
(cumsum(factorial(0:5) * 0:5) + 0.8)[6:1]

# c
rep(2.5, 7) + cumsum(2:8) - 2

# d
x <- cumsum(1:8)
ifelse(x > 20, 20, x)

# e
x <- 1:4
rep(ifelse(x == 4, 0, x), 3)

# f
seq(10, 10.9, 0.3)

# g
rep(seq(0.4, 0.9, 0.1), rep(3:1, 2))

# h
x <- numeric(14)
{x[5] <- 1; x[9] <- 2; x[12] <- 3; x[14] <- 4; x}

# i
paste(rep('x', 10), rep(-1:-4, 4:1), sep = '-^')

# j
1 + sum(cumprod(seq(3, 30, 3)) / cumprod(seq(2, 47, 5)))


# Q2 

name <- function(x, n=1000000) {
  if (x == 0){
    return (1)
  }
  sums=1
  for (elt in n:1){
    sums = 1+x*sums/elt # use the formula for approximation e^x = 1 + (x/1)(1+x/2)(1+x/3)(...)
  }
  sums
}
q2 <- function(x) {
  sapply(x, name)
}



# Q3a

ML <-  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
dates <- function(x, year = 2021) {
  if (x <= 31){ # if input less than 31, it must be in Jan
    paste(x, 1, year, sep = '/')}
  else {
    tr_fl = ifelse(x %% cumsum(ML) == x, FALSE, TRUE)
    valid_date = x %% cumsum(ML)[tr_fl]
    num <- length(valid_date)
    x = if (valid_date[num] == 0) valid_date[num - 1] else valid_date[num]
    y = if (valid_date[num] == 0) num else num + 1
    paste(x, y, year, sep = '/')}
}
date <- function(x) {
  sapply(x, dates)
}



# Q3b

ML <-  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
cumML <- cumsum(ML)
week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
day.of.weeks <- function(x) {
  if (nchar(x) == 8) { # eg: 1/1/2021
    day = as.numeric(substring(x, 1, 1))
    month = as.numeric(substring(x, 3, 3))
  } 
  if (nchar(x) == 10) {# eg: 31/12/2021
    day = as.numeric(substring(x, 1, 2))
    month = as.numeric(substring(x, 4, 5))
  }
  if (nchar(x) == 9) { # eg: 10/1/2021 or 1/10/2021
    day = as.numeric(ifelse(substring(x, 2, 2) == '/', substring(x, 1, 1), substring(x, 1, 2)))
    month = as.numeric(ifelse(substring(x, 3, 3) == '/', substring(x, 4, 4), substring(x, 3, 4)))
  }
  date <- ifelse(month != 1, (cumML[month-1] + day - 3) %% 7, (day - 3) %% 7)
  week[ifelse(date == 0, 7, date)]
}
day.of.week <- function(x) {
  unlist(lapply(x, day.of.weeks))
}



# Q4

name <- function(z, n) {
  num = z %/% 1 # j has to be integer
  zero <- ifelse(num <= 0 | num == Inf, numeric(1), numeric(num + 1))
  if (0 <= z & z <= n){
    j = 0:num
    zero[seq(along = j)] = (-1)^j*choose(n, j)*(z-j)^(n-1)/factorial(n-1)
    return (sum(zero))
  } 
  return (zero)
}
f <- function(z, n) {
  sapply(z, name, n)
}


