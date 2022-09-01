# Basic R
# call datasets to find the dataset 
# datasets::

library(tidyverse)

print(sqrt(2), digits = 15)
sqrt(2) ^ 2 == 2;near(sqrt(2) ^ 2, 2)
log(4, base = 2) # default is e
sin(1);sin(pi/6) # angles are in radians not degrees(sin(1) ~ sin(pi/3))
asin(1)          # inverse sine(反正弦) which is x = arcsiny domain is -1 to 1, codomain is -inf to inf

100 %% 3                                  # remainder
100 %/% 3; 5%/%3; floor(5/3);ceiling(5/3) # floor division

1 / 0 # Inf (any num / 0 is Inf, other than 0)
0 / 0 # NaN: not a num
Inf - Inf # NaN
# sqrt(-1)  NaN
1 + sin(NA) # NA

sign(2:-4) # return 1,0,-1 for positive,zero,negative nums respectively
factorial(1:5);cumprod(1:5);cumsum(1:5)
choose(5, 2) # it is a permutation/binomial coefficient   P(x, k), x >= k,  5!/(5-2)!2!
choose(c(6, 8), 3) # two outputs 
lchoose(5, 2) # is In(P(x, k)) = log(choose(5,2))
ceiling(2.000001)
floor(3.9989)
round(pi, 4)    
round(pi, 9) # R has default 6/7 digits
round(.789023456, 9) # 9 dp
round(1499.56789,-3) # the nearest 1000
round(15000.56789,-4)# the nearest 10000
signif(1234.56789,4) # start from front and keep 4sf
signif(123.456789,8) # 8 figures

# also there is some other distris : dnorm(norm distri), dpois, dchisq(chi-square), dt(t-distri), dF(f-distri),
dbinom(7, 30, .25) # pdf/pmf of binomial
pbinom(7, 30, .5)  # cdf of binom
qbinom(.7, 30, .5) # reverse of cdf = 16, if we put pbinom(16, 30, .5) ~ .7
rbinom(10, 50, 0.6)# 10 attempts, each trial is between 0 and 50 with prob 0.6

plot(dbinom(1:80, size = 80, prob = 0.2), type = "h", lwd = 2,
     main = "Binomial probability function",
     ylab = "P(X = x)", xlab = "Number of successes")
lines(dbinom(1:80, size = 80, prob = 0.3), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))
lines(dbinom(1:80, size = 80, prob = 0.4), type = "h",
      lwd = 2, col = rgb(0, 1, 0, 0.7))

# careful they are not equal in R, in practice they are
print(dbinom(0, 3, .3) + dbinom(1, 3, .3), digits = 20)
print(pbinom(1, 3, .3), digits = 20)
near(dbinom(0, 3, .3) + dbinom(1, 3, .3), pbinom(1, 3, .3)) # but near works perfectly


range(c(1:20, 54, -Inf-1)) # return the min and the max
prod(1:5) == factorial(5)  # prod = product of the seq

min(NaN, NA);min(NA,1:5) #NA
max(NaN, NA);max(NA,1:5) #NA
max(NaN, NULL);min(NULL,-1:5)
min(NULL, NA);min(NaN,1:5)

# no na.rm in cumsum
cumsum(1:5)  # cumulative sum eg: 1st is 1st, 2nd = 1st + 2nd = 3, ......
cumprod(1:5) # same logical
cummax(c(1:10, 5))

lead(1:5,3,default = 3);lag(1:5,3,default = 3)
pmax(1:5,3);pmin(1:5,3)
pmax(0, c(-1, 0, 1)) # consider a matrix which x = 0 0 0 compared to y = -1 0 1
pmin(c(1, 10), c(10, 2, 3, 5)) # must be x = ay or y = bx
pmax(seq(1,10,length=5),11:20,50:31)

x <-  c(1, 2, 3, 4);x
y <- seq(1,10,by=2);y
s <-  c("first", "second", "third")
all(x > 1)
any(y > 2)
cut(y,3)
union(x,y) 
intersect(x,y)
setdiff(y,x)

TRUE & NA # NA
FALSE & NA # F
TRUE | NA # T
FALSE | NA # NA
# NA&NA == NA|NA

ifelse(x>pi | x<exp(1), 'bingo', 'suck')
nchar(s);str_count(s)

# if the num is the multiple of string's length, then run num times, otherwise run string's length times 
substr(s, 2, 6)  # each word start with 2 and end in 6
substr(s, 2:3, 3:5)  # only run 3 times 2-3, 3-4, 2-5
substring(s, 1, 3:5) # 1 means start from 1st letter of each word, 3:5 means 1st word will have
# length of 3 and 2nd word will have length of 4, so on, ......

substr(s, 1:2, 1:6)    # only return 3 times
substring(s, 1:2, 1:6) # return 6 times, 1st return 1st word which have length 1 with 1st letter
# 2nd return 2nd word which has length 2 but start with 2nd letter, recycling, ......

# paste strings together
paste('1', '2', '3', sep = '->') == paste(x[-4], collapse = '->')
# care that sep only works when there is ≥ 2 parameter in paste, outputs a vector or a character
paste('1', 2, '3', sep = '->') # connect irrelatives varbs by a separator
paste(x, 5, sep = '.');  paste(x, 5, sep = '.',collapse = ' ')
# won't function when have only one parameter
paste(x, sep = '1 ')

glue::glue('1', 2, '3', .sep = '->');glue::glue('1', 2, '3', .sep = '->') == cat(paste('1', 2, '3', sep = '->'))
glue::glue_collapse(glue::glue(1,5, .sep='.'), sep = '.')

# however collapse works for one or more parameter: always outputs a single character
paste(x[-4], collapse = '->')    # separate relatives by a collapse
paste(x, 1, collapse = ',')


mode(s) # to identify a type of the varb : "logical" "numeric" "complex"  "character"
length(1999:2021) # 2021-1999+1
!logical(100) # logical(x) return x times false
character(10) # return 10 times ""
complex(1) # return 0+0i
numeric(1) # return 0
numeric(0)
vector('raw', 3)
vector(length = 5, mode = 'logical')

c(TRUE, 17)  # automatically coerced by R  
c('TRUE', 17)
c(TRUE, 17, 'FALSE')
c(TRUE, complex(1))

sum(c(TRUE, FALSE)) # sum(TRUE) = 1 and false = 0
cumsum(c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))

as.numeric(c('1', '5')) + as.numeric('8') # Explicit Coercion

# name the vector, but it does not affect anything, we can still do whatever we want
r <- 1:3;r
names(r) <- list(1, 2, 3);r  # c('1'=1,'2'=2,'3'=3)
names(r) <- c('d','k','r');r # c('d'=1,'k'=2,'r'=3)
r[1:2]

names(s) <- c('', 's');s
s.1 <- s[1];s.1
names(s.1)=NULL;s.1
names(s) = NULL;s  # unname
# unname
names(s) <- letters[1:3];s
unname(s)
setNames(r, month.abb[1:3]) # set_names()


(lst <- list(r, 'T', 1+2i, x))
# extract an element from a sublist(if exist)
lst[[1]][1]
lst[[4]][-2]
# if does not then a single element of a list, use [[ ]]. When asking for a subset, use [ ]
lst[2]
lst[[3]]

# create an empty list and assign a value
empty_lst <- vector(mode = 'list', length = 3);empty_lst
empty_lst[[1]] = 1:10
empty_lst[[2]] = lst;empty_lst

(named_lst <- list(a=r, b="r", 1+3i))
named_lst[['a']]==named_lst$a # same as named_lst[[1]] 
named_lst$a[1]
named_lst$b
named_lst[[3]]

(nested_list <- list(a = named_lst, b = 'r', c = x))
nested_list$a$a;nested_list$a[[1]];nested_list$a[['a']]
nested_list$c[-(3:4)]

length(NULL)

# seq usually have max 3 arguments:
seq(1, 10, by = 2)
seq(1, 23, length = 25)
# from 2 back 8 nums:
seq(to = 2, by = 0.5, length = 9)
# start with 1 and increase 0.75 each until 12 nums:
seq(1, by = 0.75, length = 12)
# along argument show the index of the var
seq(along = c(s, x))

# care that if the second argument is not a single num, it outputs diff, see below two
# ***
rep(x, 3)       
rep(x, each = 3) # same as c(3, 3, 3, 3)

rep(1:4, c(2, 3, 2, 3)) # means repeat 1 two times, 2 three times
rep(r, each = 3)
rep(r, rep(3, 3))



