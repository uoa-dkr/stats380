# basic concept

# readlines: read each line of file
readLines('text.txt')

# strsplit: to separate a string by a certain notation and return several strings(or list)
readLines('simple.txt')
strsplit(readLines('simple.txt')[-1],',')
strsplit('D K R d k r', '[kK]') # regular expression: check below


# do.call: function command(can be str or itself) by passing a list
do.call(cbind,strsplit(readLines('simple.txt')[-1],','))
do.call('c',list(c(1,2,3))) # coerce list to vector
do.call(max,list(c(1,2,3))) # or do the some command

## call: function command must be str and only work with eval
call('sum',1:10)
eval(call('sum',1:10))


# read the html(or format like html) and extract some stuff we craving for
# writeLines(): create lines of code via a new file 
web = 'https://www.stat.auckland.ac.nz/~yongwang/stats380/Data/2005-01.html'
readin = readLines(web);length(readin)
writeLines(readin,'~/Desktop/R basic and visualization/stats380-R/stats380/2005-01.html')
head(readin)
readin[1:2]
paste(readin[1:5],collapse = '')
# grep and grepl() to look up if a certain symbol or string exist in the text and return its indexes 
grep('<',readin)   # '<' symbol
grepl('html',readin)# a certain word and return logical for each 
idx = grep('<tr><td class=\"aws\"',readin);idx

# break the long tedious(:c) string
info = strsplit(readin[idx[1]],"</td><td>");info  # 2 and 4 is straightforward

# attempt to extract info from 1 3 5
# sub and gsub() allow us to transform a specific piece of string(desired) to a new one
# more importantly
# there are a few regular expression that help us to extract string easier
# * zero or more
# + one or more 
# ? zero or one
# . any character [a-zA-Z] letters
# [0-9] digits
# | or
# () and 
# ^ at the start $ at the end
# $ at the end
set.seed(15)
eg = paste(c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
             'Saturday','Sunday'),sample(10:30,7),month.name[sample(1:12,1)]);eg
sub('d','D','d d r')   # only 1st d
gsub('d','DK','d d k') # all d (g stand for global)
sub(' ','', 'd k r')
gsub(' ','  ','d k r')
sub('ay','DAY',eg)     # 'ay' in dates have changed, not for May 
gsub('ay','DAY',eg)    # for all 'ay'
gsub('.*?','1',eg)



sub('[a-z]','-',eg)   # each letter(l) replace by -
gsub('[a-z]+','-',eg)  # one or more letter(l) replace by -
gsub('[A-Z]','-',eg)   # each letter(c) replace by -
gsub('[a-zA-Z]','-',eg)# each letter(c and l) replace by -

gsub('[0-9]','-',eg)   # each num replace by -
gsub('[0-9]+','-',eg)  # at least one or more adjacent num(s) replace by - 

gsub('.','.',eg)       # all char to .
gsub('^.','.',eg)      # one char that at the first pos
gsub('^.+','...',eg)   # one or more char that at the first pos(all)
gsub('.$','.',eg)      # one char that at the last pos
gsub('. ','/',eg)      # all char and a extra blank will be replaced by /

gsub(' .+','',eg)           # Mon - Sun     (1st part)
gsub(" [0-9]+.+$", "", eg)  # Mon - Sun
gsub('^[a-zA-Z]+ ','',eg)   # date and month(2nd part) 
gsub('^.+[ady] ','',eg)     # date and month 

info[[1]][1];info[[1]][5]
s1 = gsub('^.+">','',info[[1]][1]);s1
s2 = gsub('<.+$','',s1);s2             # unlock 1

s3 = gsub('<.+$','',info[[1]][5]);s3   # 3 has been complete
as.numeric(s3)

as.numeric(sub('KB','',info[[1]][3]))  # a space does not interfere as.numeric
as.numeric(gsub(' KB','',info[[1]][3]))# num 3 is accomplished through efforts

substr(info[[1]][3], 1, nchar(info[[1]][3])-3) # recall that we could achieve by substr or substring
# in general substring(substr) is more efficient than gsub(sub) 
# due to substr sole pick indexed we assigned rather research every single word then replace it.


# recall that we can use grep() to find the location of we craving under whole vectors
grep('o',eg) # only exist in 1st vector
#  if we are interested in finding the location of our desired string inside of a certain vector
#  we can use regexpr(c) to output the 1st pattern that we want
regexpr('o',eg) # not only show which vector have, but also show the index(-1 means not exist)
attr(regexpr('o',eg), "match.length")
regexpr("[a-z] ", eg) # not surprisingly, we are allowed to use regular expressions

pos = regexec('<',info[[1]][5]);pos # extract
substr(info[[1]][5],1,pos[[1]]-1)

# however gregexpr enable us to grab globally, and return a list
gregexpr(' ',eg); gregexpr(' ',eg)[[1]][1]; gregexpr(' ',eg)[[1]][2]
space = gregexpr('. ',eg); space[[1]][1]; space[[1]][2]

# also we rematch our desired
regmatches(eg, space)
regmatches(eg, space) <- '~';eg  # kind of like gsub
eg = paste(c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
             'Saturday','Sunday'),sample(10:30,7),month.name[sample(1:12,1)]);eg



#######################################################################################################
#######################################################################################################
#######################################################################################################
web = 'https://www.stat.auckland.ac.nz/~yongwang/stats380/Data/'
readin = readLines(web)
dataLines <- grep('<td>', readin)
dataText <- readin[dataLines];dataText
strsplit(readin[seq(5,49,4)],'<td><a href=') # to obtain the date pattern like this --- 3 ways

########### 1st way via sprintf(): convert num or char to the formatted string
# sprintf(): deal with num
num = 1:5;rand = rnorm(5);dkr = c('ddd','kk','r')
sprintf('%d',num);sprintf('%2d',num)      # d means digit(integer), and 2 means len 2
sprintf('%3.f',rand);sprintf('%.2f',rand) # 3.f means float num len 3 and round to int, .2f mean 2dp
sprintf('%05d',num);sprintf('%-10.f',rand)# 0 means replace all blanks by '0', length 10 and left align 
sprintf('integer: %3d',num);sprintf('random number: %10f',rand) # combinations
sprintf('%e',num);sprintf('%E',rand)      # format in 1e1 with lower and cap case of 'e'
sprintf('random number %s: %s',num,rand)  # no idea why s can be applied in here, %d: %f is also correct

# sprintf(): deal with char
sprintf('%3s',dkr);gsub(' ','@',sprintf('%10s',dkr))
cat(sprintf('%d. %s',1:3,dkr),sep = '\n')
cat(sprintf('%d. %s',1:3,sprintf('%3s',dkr)),sep = '\n')

# back to our goal, to gain 'data/2005-01.html'
yrs = rep(2005:2009,each=12)[1:54];yrs
mons = rep(1:12,5)[1:54];mons
paste('data/',yrs,sprintf('-%02d.html',mons),sep = '') # or
sprintf('data/%s-%02d.html',yrs,mons) # look likes more convenient


########### 2nd way via date: we can perform date arithmetic and date comparisons
as.Date('2022-12-1');as.Date('2022/12/1')

# %d:	day as a number (0-31)   eg:	01-31
# %a: abbreviated weekday      eg:  Mon
# %A:	unabbreviated weekday    eg:	Monday
# %m:	month (00-12)	           eg:  00-12
# %b: abbreviated month        eg:  Jan
# %B: unabbreviated month      eg:  January
# %y: 2-digit year             eg:  01-22
# %Y: 4-digit year             eg:  2001-2022

as.Date('12 September 2015',format='%d %B %Y');as.Date('12+ September 2015',format='%d+ %B %Y')
ori = as.Date('Jan?1 -05',format='%b?%d -%y');ori;c(class(ori),typeof(ori))
ori+1;seq(ori,as.Date('2005/01/25'),by='week');Sys.Date()>ori # date arithmetic and comparison
format(ori,'%d %Y %B');format(ori,'%d %m %Y is %a');format(Sys.Date(),'today: %a') # modify its format
format(lubridate::today(),'today is %Y %B %d(%a)')

# back to our goal, to gain 'data/2005-01.html'
format(seq(ori,by='month',length=54),'Data/%Y-%m.html')
file.path('Data',format(seq(ori,by='month',length=54),'Data/%Y-%m.html'))


#######################################################################################################
#######################################################################################################
#######################################################################################################
# data manipulation and reshape
data_set <- data.frame(price = round(rnorm(25, sd = 10, mean = 30)),
                       types = sample(1:4, size = 25, replace = TRUE),
                       store = sample(paste("Store", 1:4), size = 25, replace = TRUE))
data_set_lst <- data.frame(price = round(rnorm(25, sd = 10, mean = 30)),
                           types = sample(1:4, size = 25, replace = TRUE),
                           store = sample(paste("Store", 1:4), size = 25, replace = TRUE))
head(data_set,16) # types is numeric

######
# tapply: summarise the data.frame or list(array) [same function as apply in matrix]
with(data_set, tapply(price, price, length)) # freq of each price(same as table(price))
with(data_set_lst,tapply(price, store, mean))# list works as well
with(data_set, tapply(price, store, mean)) # mean price for each store
with(data_set, tapply(price, store, sum, simplify = F)) # sum price for each store and return a list
with(data_set, tapply(price, as.factor(types), mean)) # mean price for each type

data_set$types = factor(data_set$types,labels = c("toy", "food", "electronics", "drinks"))
with(data_set, tapply(price, types, mean)) # make it more readable

with(data_set, tapply(as.factor(types), store, function(x) sample(x,5,replace = T)))
with(data_set, table(types, store)) # contingency table 
with(data_set, tapply(price, list(types,store), mean)) # contingency table weighted by price
with(data_set, tapply(price, list(types,store), sum, default = 0)) # replace NA by 0


######
# aggregate: summarise the data.frame or list(array) [similar as tapply above]
data_set$n = rnorm(nrow(data_set))

with(data_set_lst, aggregate(price,list(types),mean))              # data can be a list
with(data_set, aggregate(price,list(price),length))                # freq of each price
with(data_set, aggregate(price,list(types),mean))                  # same as above
aggregate(data_set['price'],list(types=data_set$types),mean)       # rename to be more readable
aggregate(data_set$price,list(data_set$types,data_set$store),mean) # contingency table
aggregate(cbind(data_set$price,data_set$n),list(data_set$types,data_set$store),mean) # complex

aggregate(price~types,mean,data = data_set)                        # formula allowed in this function
aggregate(price~types+store,mean,data=data_set)                    # mean of each store at each types
aggregate(price~types+store,mean,data=data_set,drop=F)             # include the missing value
aggregate(cbind(price,n)~types+store,mean,data=data_set)                            # complex

aggregate(cbind(price,n)~types,cov,data=data_set) 
####### brief summary about diff between aggregate and tapply:
# 1. aggregate return a data.frame, whereas tapply return a list or vector(simplify=F)
# 2. formula can be used in aggregate, and allowed multiple var(both numeric and char)
# 3. aggregate ignore the missing value(drop=T), whereas tapply keep and easy to modify(default=...)
# 4. the shape of output is diff, tapply output contingency table like(sas freq or tabulate), aggregate output contingency table like(sas summary or mean)

###### just some basic
# 3-way contingency table 
data_set$l = sample(letters[sample(1:26,4,replace = F)],25,replace = T)

xtabs(~store+types+l,data = data_set) # last var will dominate 
xtabs(~store+l+types,data = data_set) # by types; table store*l;

ftable(xtabs(~store+l+types,data = data_set)) # proc tabulate data=data_set;
ftable(xtabs(~store+types+l,data = data_set)) #     class store types l;
ftable(xtabs(~l+store+types,data = data_set)) #     table (l*store),types; run;


####################### data reshaping
wide <- data.frame(name=c("a", "b"), t1=c(3, 12), t2=c(6, 15), t3=c(9, 18));wide # wide format

(long <- data.frame(name=rep(c("a", "b"),each=3),time=rep(1:3, 2),value=c(3, 6, 9, 12, 15, 18))) # long format

lst = list(long,long[-3],long[-2,],long[-6,],long[-5:-4,]);lst
# merge(only for two datasets) final goal is to merge lst

## for diff varb names
merge(wide,long);merge(wide,long,by='name') # equivalent(1st is implicitly tell R by='name')

## for same varb names
merge(wide,wide[-4],by='name') # automatically add sth to distinguish
merge(wide[-2],wide[-4],by='name',suffixes = letters[2:3]) # better change colnames rather this argument

## keep all
merge(wide, wide[-1,], by='name')            # [inner join](interaction)
merge(wide, wide[-1,], by='name', all = T)   # [full join]
merge(wide, wide[-1,], by='name', all.x = T) # value that exist in wide       -- [left join]
merge(wide, wide[-1,], by='name', all.y = T) # value that exist in wide[-1,]  -- [right join]

# but we cannot merge multiple dataset other than 2
# so a function called reduce() can help solve this out

## reduce(): only works for two inputs(arguments) function
Reduce('+',1:3);Reduce(function(x,y) (x+y),1:3) # this is how addition arithmetic works(only pass two each time)
Reduce('/',1:100) # also work for n inputs
Reduce(function(x,y) if(x>y) x else y, c(1,5,2,8)) # same as max(c(1,5,2,8)), compare 1/5,5/2,5/8

## final
funs <- function(x, y) {
  merge(x,y,by='name',all=T)
}
mergedatasets = Reduce(funs,lst) # not appropriate, unexpected output
head(mergedatasets,3) # have to pre-process they col names

## mapply(): mapply is a multivariate version of sapply
c(7,8,11,12);mapply(sum, 1:4, 1:4, 5:4) # 1+1+5/2+2+4/...

list(rep(1, 5), rep(2, 4), rep(3, 3), rep(4, 2), rep(5,1));mapply(rep, 1:5, 5:1) # so efficient

fun1 <- function(lst,l) {
  len = 1:(length(colnames(lst))-1)
  colnames(lst)[-1] <- paste('product',l,len,sep = '_')
  lst
}
new_name_lst=mapply(fun1, lst, LETTERS[1:length(lst)]);new_name_lst
Reduce(funs,new_name_lst) # no warnings prefect !!!


## call library
library(reshape2) # pivot_longer/pivot_wider talk about later
melt(wide,id='name');long  # wide to long
dcast(long,name~time);wide # long to wide




#######################################################################################################
#######################################################################################################
#######################################################################################################
set.seed(1000)
mat = matrix(sample(c(NA,NULL,1:20),9),nr=3);mat
attr(mat,'num of NA value') <- length(mat[is.na(mat)])
mat

# undo this by assign the NULL value
attr(mat,'num of NA value') <- NULL
mat


############ Debug
word <- function() {
  paste(sample(letters, 6, replace=TRUE), collapse="")
}
find <- function(n) {
  haystack <- sapply(1:n, function(i) word())
  location <- grep("needle", haystack)
  subset(haystack, location)
}
find(1000) # error: 'subset' must be logical

debug(find)# we can type ls() var name to see what we got so far or Q to quit
find(10)   # call function again, R automatically examine each line of function, until find error
undebug(find) # cancel the debugging process
find(10)   # back to normal
traceback()

find_mod1 <- function(n) {
  browser()
  haystack <- sapply(1:n, function(i) word())
  location <- grep("needle", haystack)
  browser()
  subset(haystack, location)
}
find_mod1(10)

find_mod <- function(n) { 
  haystack <- sapply(1:n, function(i) word())
  location <- grepl('[dkr]', haystack)  # modify the bug
  subset(haystack, location)
}
find_mod(100)



coercion <- function() {
  l = sample(c('NA,','dk', ' 1'),8,replace = T)
  n = sample(1:20,8,replace = T)
  as.numeric(paste0(l,n))
}
options(warn=-1) # when we change warn to be a neg, it would be no warning message occurred(ignored)
coercion()

options(warn=1) # default is 0, switch to 1 does not change much 
lapply(1:6, function(i) coercion())

options(warn=2) # when we change to 2 or more, R considers the first warning message as a error
lapply(1:6, function(i) coercion()) # would be no output

options(warn = 0)
lapply(1:6, function(i) coercion())
traceback()


coercion1 <- function() {
  l = sample(c('NA,','dk', ' 1'),8,replace = T)
  n = sample(1:20,8,replace = T)
  browser()
  as.numeric(paste0(l,n))
}
as.list(body(coercion1))
coercion1()

coercion2 <- function() {
  l = sample(c('NA,','dk', ' 1'),8,replace = T)
  n = sample(1:20,8,replace = T)
  browser()
  as.numeric(paste0(l,n))
  browser()
}
as.list(body(coercion2))
coercion2() # we can identify the warning message from our function


# trace: add a expression or function to adjust 
as.list(body(coercion))
trace(coercion,quote(ifelse(l%in%c('NA,','dk'),0,l)),at=3)
lapply(1:6, function(i) coercion())
untrace(coercion)

trace(coercion2,quote(ifelse(l=="NA,"|l=='dk',0,l)),at=4)
lapply(1:6, function(i) coercion2())
untrace(coercion2)






