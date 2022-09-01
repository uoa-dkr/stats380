# factor(): creates data objects which represent variables containing unordered categorical data
(eyes = c("hazel", "blue", "brown", "green", "blue", "brown"))
(eyecol = factor(eyes)) # sorted into ascending order (either numerically or alphabetically)
levels(eyecol)

# we can switch it to a new level
eyecol = factor(eyes,levels = c('green','blue','brown','hazel'));eyecol
factor(eyes,levels = 'blue') # note that if we just specify a single char, it causes an error


#*
# ordered(): In this case factors are described as ordered factors
pain = ordered(c("low", "medium", "medium", "high", "medium", "low"));pain

# we can switch it to a new level
(pain = ordered(c("low", "medium", "medium", "high", "medium", "low"),levels = c('low','medium','high')))
levels(pain)


#*
# cut(): transfer a single into a desired range factor lines
c = cut(rnorm(1000),c(-Inf, -3:3, Inf));c
levels(c)
table(c)


########################################################################################################
# Factor Predicates: To tell whether a value is a factor
is.factor(eyecol);is.factor(pain)  # attention that ordered factor is a kind of factor
is.ordered(eyecol);is.ordered(pain)


## Operations on Factors:
# 1. One of operations that makes sense on an unordered factor is using == or 
eyes == 'blue'
eyecol == "blue"
eyecol < "blue" # error

# 2. For ordered factors, comparisons using <, <=, > and >=
pain <= 'low'
pain > 'medium' | pain == 'low'


### one of few things that to create a contingency table by its level occurs
# 1. table: return a named vector or 1 dim matrix
table(eyes)   # by alphabetically
table(eyecol) # ordered by predefined
table(pain)

# 2. Cross-Tabulation: count num of times each combination of the levels of two (or more) factors occurs
table(eyes,eyecol) # matrix


#### Obtaining Summaries over Factor Levels:
# 1. tapply(): Factors provide a way of defining subgroups in a dataset and gain summaries for these subgroups
ht = sample(140:180,31,replace = T);ht
sex = sample(c('male','female'),31,replace = T);sex
sex_fct = factor(sex)
tapply(ht,sex_fct,max) # output a table with its frequency, 1st two arguments have to be same
tapply(ht,sex_fct,var)
tapply(ht,sex_fct,function(x) sum((mean(x)-x)^2)/(length(x)-1))

# more complex: with two parameters
lt = sample(LETTERS[sample(1:26,5)],31,replace = T);lt
lt_fct = factor(lt)
list(sex_fct,lt_fct);tapply(ht, list(sex_fct,lt_fct),max) # return a contingency table but may produce NA


##### basic for data frame: with() ; sort() ; order()
df = data.frame(height = ht, gender = sex_fct, letter = lt_fct);df
is.data.frame(df)
with(df,tapply(height, gender, '-')) # a bit of review with()
with(df,tapply(height, gender, function(x) 2*pt(abs(x-165)/stderr(),length(x)-1,lower.tail = F)))
with(df,sort(height))
df$gender=with(df, sort(gender,decreasing = T));df
o = order(df$letter)
with(df, letter[o])


# there are many ways to produce new col(s), in general, 3 ways that i'd like to do
# 1. use df$new = ...  directly
df$range1 = with(df, cut(height, seq(130,180,10)));df

# 2. transform(): kind of like how with works
df = transform(df, dob = sample(month.abb,31,r=T));df

# 3. use df[, n] = new also add two obs
{df[, 6]<-(df$height-80)*sample(seq(.6,.8,.01),31,r=T)+sample(seq(0,.6,.01),31,r=T);names(df)[6] = 'weight';df}
df=rbind(df,c(150,rep(NA,4),75));df
df[nrow(df)+1,] = c(178,rep(NA,4),45);df

# subset apply in the df when analysing
lm = lm(weight~height,data = df)
plot(weight~height,data = df)
points(c(150,178),c(75,45),col='red',pch=16)
abline(lm)
plot(lm,1)
s20x::normcheck(lm)
s20x::cooks20x(lm)  # 34/33/32 may be the undue influence points
summary(lm)

lm = lm(weight~height,data=df,subset=-c(32,33))
plot(weight~height,data = df,subset=-(32:33))
abline(lm)
plot(lm,1)
s20x::normcheck(lm)
s20x::cooks20x(lm)
summary(lm)

lm = lm(weight~height,data=df,subset=!is.na(gender)) # same logical as above
plot(weight~height,data = df,subset=!is.na(gender))
abline(lm)
plot(lm,1)
s20x::normcheck(lm)
s20x::cooks20x(lm)
summary(lm)

# subset() work in general
subset(df, !is.na(gender), select = -c(gender,dob))



########################################################################################################
# read the data in and some command
car1 = read.table('practice.txt');car1
types = sapply(car1, class);types

car2 = read.table('practice.txt',col.names = c('velocity','distance'));car2
car3 = read.table('practice.txt',row.names = c(letters[-1],LETTERS[-26]));car3[-1]
car4 = read.table('practice.txt',colClasses = types);car4$speed;car4$dist

