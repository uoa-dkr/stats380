# plot with given x, y
x=1:100;y=rnorm(100)
plot(x,y)
abline(a = -2, b = 0.1)
lines(seq(10,100,by=10), c(-2:2,2:-2),col='red',lty=2,lwd=2)

# plot with one parameter
plot(rnorm(100)) # R will consider indexes(0:100) as a-axis


######################################################################################################
# The default annotation and labelling can be overridden with optional main, xlab, ylab and sub arguments.
plot(x, y,
     main = "An Overall Title",
     xlab = "A Label for the X-Axis",
     ylab = "A Label for the Y-Axis",
     sub = "A Plot Subtitle")        # sub title under the x-axis label


######################################################################################################
# An optional argument called pch controls the plotting symbol(s) used by plot
# The argument can be either a single character, or a numerical index into a table of symbols
# there are 25 default symbols, and recycling rule can be applied
plot(x,y,pch=ifelse(x%%2==1,1,2))
plot(x,y,pch=2:6)
plot(x,y,pch=10)


######################################################################################################
# Plotting symbol colours can be customised with the col argument.
# Colors can be specified by name with character strings; e.g. "red", "blue" or use col in color()
plot(x,y,col=sample(colors(),length(x)),pch=17)
plot(x,y,col=c('red','blue'),pch=17)
plot(x,y,col=100:103,pch=16) # better to specify the name of color


######################################################################################################
# Different Types of Plot
# The optional type argument makes it possible to produce other types of plot by describing their content
# 1. type = 'p' points (i.e. a scatter plot)
# 2. type = "l" lines (i.e. a line plot)
# 3. type = "b" both (points and lines)
# 4. type = "c" just the lines from type="b"
# 5. type = "o" points and lines overplotted
# 6. type = "h" high-density needles
# 7. type = "s" step function, horizontal step first
# 8. type = "S" step function, vertical step first
# 9. type = "n" nothing (i.e. no plot contents)
plot(dpois(0:15, 4.25), type = 'p')
plot(dpois(0:15, 4.25), type = 'l')
plot(dpois(0:15, 4.25), type = 'b')
plot(dpois(0:15, 4.25), type = 'c')
plot(dpois(0:15, 4.25), type = 'o')
plot(dpois(0:15, 4.25), type = 'h')
plot(dpois(0:15, 4.25), type = 's')
plot(dpois(0:15, 4.25), type = 'S')
plot(dpois(0:15, 4.25), type = 'n')

plot(rnorm(10), type = 'p')
plot(rnorm(10), type = 'l')
plot(rnorm(10), type = 'b')
plot(rnorm(10), type = 'c')
plot(rnorm(10), type = 'o')
plot(rnorm(10), type = 'h')
plot(rnorm(10), type = 's')
plot(rnorm(10), type = 'S')
plot(rnorm(10), type = 'n')


######################################################################################################
# Line Types
# The line type can be specified with an argument of the form lty=type either name or num or string
plot(rnorm(10), type = 'l', lty=2)
plot(rnorm(10), type = 'h', lty='dashed')
plot(rnorm(10), type = 'c', lty='1221',col='red')
# Line width
# Line thickness can be set with lwd = w 
plot(rnorm(10), type = 'o', lty=2, lwd=2)


######################################################################################################
# Controlling Axis Limits and extension
# The plot arguments xlim and ylim supply two values which are used to determine the limits on the x and y axes
# by default, plot add 7% extra to each end points, to undo this via xaxs/yaxs = 'i'
# also we can drop the axes
plot(rnorm(10),xlim = c(-10,20))
plot(rnorm(10), xaxs='i', yaxs='i')
plot(rnorm(10),xlim = c(-10,20), ylim = c(-2,1.5))
plot(rnorm(10),axes = F)


######################################################################################################
#1. points:    draw points
#2. lines:     draw connected line segments(it could have multiple(inf) points, and lines between them)
#3. abline:    add a straight line to a plot
#4. segments:  draw disconnected line segments(only have two points, and line between two points)
#5. arrows:    add arrows to a plot
#6. rect:      add rectangles to a plot
#7. polygon:   add polygons to a plot
#8. text:      add text to a plot
set.seed(380)
pts=runif(10)
plot(pts, ylim = c(0,1))
points(6,.6,pch='6',cex=2,col='red') 
points(7,.7,pch='71',cex=2.5) # only allowed single character
lines(c(2,3),c(0.3,.2), type = 'c',lty=2,lwd=2,col='blue')
lines(2:4,seq(0,.4,length=3),type = 'b')
abline(a=0,b=.1, lty=5,lwd=1.2,col='green')
abline(a=.1,b=.1, lty=5,lwd=1.2,col='green')
segments(2,.8,4,.7)
segments(2,.7,4,.8)
arrows(7,.3,8,.4,angle = 45,code = 1) # default is 2 and towards end point 
arrows(8,.4,7,.3,angle = 15,length = .5,col = 'tomato') # if code = 3 then both direction
rect(5.5,0,6.5,.05)
rect(5.6,0.05,6.4,.1)
polygon(seq(8.5,9.5,.5),c(.7,.8,.7))
polygon(c(9.1,8.85,9.6,8.6,9.35),.1*c(4.5,4,4.25,4.25,4))


# more complicated version
plot(pts, ylim = c(0,1), xlab = 'indexes',
     ylab='uniform distribution', main='10 random uniform distribution')
points(6:7,c(0.6,0.7),pch=c('6','7'),cex=c(2.5, 3),col=hcl(0))
lines(c(2:3,3),c(.1,.3,.1))
abline(.01,.1)
segments(2,c(.8,.7),4,c(.7,.8))
arrows(8,.4,7,.3,code = 3,angle = 91,length = .4599)
rect(seq(5.5,5.9,.1),seq(0,.2,.05),seq(6.5,6.1,-.1),seq(.05,.25,0.05))
polygon(c(4.25,4.49,4.25,4.75,4.51,4.75),c(.19,.1,.01,.01,.1,.19))
points(4.5,.1,pch=16,cex=.6)

# rect and drop the axes
plot(c(-10,10),c(0,10),type='n',axes = F) # drop the axes
rect(-(10:1), 0:9, 10:1, 1:10,col = 'tan') 
 
# points and arrows
groups = rep(1:5, each = 20);groups
means = c(5, 7, 6, 9, 8);means
z = rnorm(100) + rep(means, each = 20);z
boxplot(z~groups, main = "Simulated Grouped Data")
# z = multiple times of groups, eg split(1:10,1:2), 1 belongs to 1, 2 belongs to 2, 3 belongs to 1...
zbar = sapply(split(z, groups), mean);zbar
zsd = sapply(split(z, groups), sd)
plot(groups, z, xlim = c(1,5.25))
points(1:5+.25,zbar,pch=16,cex=1.25)
arrows(1:5+.25,zbar-1.96*zsd/sqrt(20),
       1:5+.25,zbar+1.96*zsd/sqrt(20),code = 3,angle = 90,length = .13)

# polygon time-series
year = 1940:2010
temp = runif(71,-1,1)
plot(temp~year,type='n',xaxs='i',yaxs='i')
polygon(c(min(year),year,max(year)),c(-1,temp,-1),col='lightgoldenrod',border = 'red')


#####################################################################################################
# Choosing Colours to Fill Areas
# hcl(h,c,l,alpha) the range [0,1] for transparency (0 means transparent and 1 means opaque)
# by hue (dominant colour wavelength), chroma (colourfulness), and lightness (brightness)
# hue(0 = red, 60 = yellow, 120 = green, 180 = cyan, 240 = blue, 300 = magenta)
# chroma depends on h and l
# l [0,100]

# implement
meat = c(8, 10, 16, 25, 41)
names(meat) = c("Lamb","Mutton","Pigmeat","Poultry","Beef")
cols = hcl(seq(120, 360, by = 60))
pie(meat,col = cols)
barplot(rep(60,6),col = hcl(h=seq(0,300,60)),axes = F,xlab = 'hue')
barplot(meat, col = hcl(300,120,100,alpha = .5))
barplot(meat, col = hcl(300),hor = T, xlim = c(0,50)) # because x-axes is frequencies, then only can be int

# Dotcharts provide a good alternative to barcharts
# consider the highest point instead of whole col
dotchart(meat,xlim = range(meat),main = "New Zealand Meat Consumption",
         xlab = "Percent in Category") # note that range(meat) and c(0,41) are different


#####################################################################################################
# Building Plots Step-by-Step usually 6 steps
# 1. To obtain a clear area (new piece of paper) to plot in call the function [plot.new()]

# 2. To set up the coordinates in the plotting area call [plot.window()](the arguments)
# xlim, ylim, xaxs="i", yaxs="i", log="xy"(for logged axes), asp=1(aspect ratio)
 
# 3. Once a plot is set up then the low-level graphics functions, eg: [points() lines() rect()]

# 4. The [axis] function can be used to draw axes on any of the four sides of the plot

# 5. The [box] function can be used to draw a box around the plot

# 6. The [title] function can be used to add a main title, x-axis label, y-axis label and subtitle

# simple practice
x = 1:30
y = rnorm(30)
plot.new()
plot.window(xlim = range(x),ylim = range(y)+1)
points(x,y)
axis(side = 1)
axis(side = 2)
box()
title(main = "A Simple Plot", xlab = "x", ylab = "y")

# implement in the function
scat = function(x, y, xlim = range(x), ylim = range(y),
                main = "", xlab = "", ylab = "") {
                plot.new()
                plot.window(xlim = xlim, ylim = ylim)
                points(x, y)
                axis(1)
                axis(2)
                box()
                title(main = main, xlab = xlab, ylab = ylab)
                }
scat(x,y)

x = 1000*x;x
y = 1000*y;y
plot.new()
plot.window(range(x),range(y))
points(x,y)
axis(1)
axis(2)
box()


#####################################################################################################
# The par Function(R graphics are controlled by use of the par function)
# Each figure consists of a rectangular plot region surrounded by four margins

# Controlling The Margins(the function par before calling plot.new())
# 1. Set the {margin sizes in (inches)}. 
par("mai") # default

par(mai=c(0.5, 1, 1, 1.2)) # distance between the axes(xy) to margins of figure region(or xlab/subtitle)
plot.new()
plot.window(range(x),range(y))
points(x,y)
axis(1)
axis(2)
box()
par(mai=c(1.02, 0.82, 0.82, 0.42)) # reset

# 2. Set the {margin sizes in lines of text}.
par("mar") # default 

par(mar=c(5, 1, 1, 2.5)) # distance between the axes(xy) to margins of figure region (or xlab/subtitle)
plot.new()
plot.window(range(x),range(y))
points(x,y)
axis(1)
axis(2)
axis(4)
box()
par(mar=c(5.1, 4.1, 4.1, 2.1)) # reset

# 3. Set the plot width and height in inches.
par('pin')

mod1=par(pin=c(2,1))
mod2=par(mar=c(5,2,2,1))
plot.new()
plot.window(range(x),range(y))
points(x,y)
axis(1)
axis(side = 3)
axis(2)
box()
par(c(mod1,mod2)) # reset


# Multifigure Layouts ---> mfrow(multi-frame row-wise) and mfcol(multi-frame col-wise)
par('mfrow')
par(mfrow=c(2,2)) # by row
plot(x,y,type = 'n')
plot(x,y,type = 'p')
plot(x,y,type = 'l')
plot(x,y,type = 'b',col='red')

par(mfcol=c(2,2)) # by col, the order have been changed
plot(x,y,type = 'n')
plot(x,y,type = 'p')
plot(x,y,type = 'l')
plot(x,y,type = 'b',col=hcl(h=325,c = 200, l=20))

# one concern about above graphs is that there exist some redundant spaces(or inner margin) between plots
# par() can be useful to trim the margins, that is:
# two arguments omi(outer margins in term of inches) and oma(outer margins in term of lines of text)
par(mfrow = c(2,2),
    mar = c(4,4,2,1),
    oma = c(0,0,1,5))
plot(x,y,type = 'n')
plot(x,y,type = 'p')
plot(x,y,type = 'l')
plot(x,y,type = 'b',col='red')
title(main = "Plots with Margins Trimmed",outer = T)
      
# we can assign these changes to a var, and call it later, in order to reset, via dev.off() or use varb
dev.off()
opar = par(mfrow = c(2, 2),
           mar = rep(0, 4),
           oma = c(4.1, 4.1, 6.1, 4.1))
par('mfrow')
par('mar')
par('oma')

par(opar)
par('mfrow')
par('mar')
par('oma')
par()

# Scatterplot Matrices
# A scatterplot matrix is a multidimensional generalisation of a simple scatterplot
# we can draw scatterplot matrix via s20x pairs()
s20x::pairs20x(iris)

# but we can draw by hands: see extra for graphics.R



#######################################################################################################
# More Flexible Layouts
# The multifigure layouts which can be specified with par are very rigid(inflexible)
# The layout function provides an alternative way of producing multiple figures(ignores the outer margin)
# ****** each cell can have diff size and adjacent two cells can be combined ******
# 3 arguments mat(matrix or array), widths(width of x-axis), heights(height of y-axis)

# 1. normal, the num of matrix has to be consecutive, eg(1:4,4:8) but not (1:4, 6:9), 5 is missing
layout(mat =  matrix(1:9, nc = 3, byrow = TRUE), widths = c(1,2,3), heights = c(3,2,1))
layout.show(9)

layout(mat=matrix(c(0,0,0,0,0,
                    0,1,0,2,0,
                    0,3,3,3,0,
                    0,3,3,3,0,
                    0,3,3,3,0,
                    0,0,0,0,0),nc=5,byrow = T), widths = c(1,5,1,5,1))
layout.show(3)

# 2.1 merge adjacent cells
layout(matrix(c(1:5,5,6:8),3,byrow=T))
layout.show(8)

# 2.2 if it is not an adjacent, it causes collision
layout(matrix(c(1:6,7,8:7),3,byrow=T))
layout.show(8)

# 3. 0 means nothing
layout(rbind(c(0, 4, 4, 0),
        c(0, 2, 0, 0),
        c(0, 1, 3, 0),
        c(0, 0, 0, 0)))
layout.show(4)

# 1. width(distance between cols) can be eg c(1, 2) specifies that the second column is twice as big as the first
layout(matrix(c(1:5,5,6:8),3,byrow=T),widths = 1:3)
layout.show(8)

# 2. The specification, the 1st is exactly 1cm wide, and the third column is twice the width of the second
layout(matrix(c(1:5,5,6:8),3,byrow=T),widths = c(lcm(1),1.5,3))
layout.show(8)

# 3. height(distance between rows) same inputs as width
layout(matrix(c(1:5,5,6:8),3,byrow=T),widths = c(lcm(1.5),1,3),heights = c(1,2,1))
layout.show(8)


# example
layout(rbind(c(0, 4, 4, 0),
             c(0, 2, 0, 0),
             c(0, 1, 3, 0),
             c(0, 0, 0, 0)),
       heights = c(lcm(2), lcm(2), 1, lcm(2)),
       widths = c(lcm(2), 1, lcm(2), lcm(1)))
layout.show(4)
mods = par(mar = rep(0, 4), cex = 1) # size of xy-axis labels
x = rnorm(30,sd=1)
y = runif(30,-3.5,3.5)
plot(y~x,las=3) # direction of value of axis(default is 0)
boxplot(x,horizontal = T,axes=F)
boxplot(y,axes=F)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(.5, .25, "An Enhanced Scatterplot", cex = 1.5, font = 2)
par(mods)


## mtext and legend and grid

# mtext: Write Text into the Margins of a Plot(4 main arguments) 
# 1. text and line(specify how far between the text and the axis)
plot(rnorm(30))
mtext('rnorm',line = 1.5)

# 2. adj(right/left align(0,1) or center(0.5,also default)) or at(align to a specific num)
plot(rnorm(30))
mtext('rnorm',adj = 1,at=20) # start at left, go to right 20
mtext('rnorm',adj = 0,at=10) # start at right, go to left 10

# 3. side(1:4) same as mar in par()
plot(rnorm(30))
mtext('rnorm',adj = 0, side = 1)
mtext('rnorm',adj = 0, side = 4)

# 4. outer/font/cex/col
par(mfrow=c(1,2),
    oma = c(0,0,2,0),
    mar = c(5,5,5,5))
plot(rnorm(30))
plot(rnorm(30))
mtext('rnorm',outer = T,cex=2,font = 2,col='red')
dev.off()

# legend: Add Legends to Plots 2 main arguments
# 1. position can be either a coordinate or 'specific'(eg:'topright') and title
plot(c(0,100),c(0,100),type = 'n',xaxs='i',axes = T, yaxs='i')
abline(0,1, col='red',lty=2)
abline(-20,.5)
legend(85,60,legend=c('y=x','y=0.5x-20'))
legend('topleft',legend=c('y=x','y=0.5x-20'),cex = .65,lty = 2:1,col = c('red','black'),bg='lightblue')

# 2. title and col and bg and type for either box or text
# text: pt.bg, pt.cex, pt.lwd/text.bg, text.cex, text.lwd
# box: box.bg, box.cex, box.lwd 
# title: title, title.col

plot(c(0,100),c(0,100),type = 'n',xaxs='i',axes = T, yaxs='i')
abline(0,1, col='red',lty=2)
abline(-20,.5)
legend('left',legend=c('y=x','y=0.5x-20'),text.col = 'red',text.font = 2,cex=.65)
legend('bottom',legend=c('y=x','y=0.5x-20'),pt.cex = 3.5,pt.lwd = 2,cex=.65)
legend('top',legend=c('y=x','y=0.5x-20'),box.lwd = 1,box.lty = 3,box.col = 'magenta',cex=.65)
legend('center',legend=c('y=x','y=0.5x-20'),title = 'regression',title.col = 'green4',cex=.25)


# grid: usually two most common arguments
plot(1:100,xaxs='i',axes = T, yaxs='i')
grid(lwd = 5)

plot(1:100,xaxs='i',axes = T, yaxs='i')
grid(ny=5,col='red')
grid(nx=5,col='red')

