###  A Simplified Population Pyramid Plot
###
###  Takes percentages of population in intervals described
###  by character strings in the "labels" argument and contrained
###  in the arguments "female" and "male".
###
###  An overall title can be passed as the value of "main" and
###  a label for the x-axes in "xlab".

ppyramid = function(female, male, labels,
                    genderlabels=c("Female", "Male"),
                    col="lightgray", bg="white",
                    mar=c(3,1,3,1), main="Population", xlab="") {
  if(length(female) != length(male))
    stop("unequal length data vectors")
  if(length(female) != length(labels))
    stop("incorrect length of label vector")
  col = rep(col, length = 2)
  mar = rep(mar, length = 4)
  
  opar = par(mar = rep(0, 4), cex = 1, bg = bg)
  layout(matrix(c(0, 0, 0, 0, 0,
                  0, 1, 3, 2, 0,
                  0, 0, 0, 0, 0),
                nc = 5, byrow = TRUE),
         widths = c(lcm(mar[2]), 1, lcm(2.5),
                    1, lcm(mar[4])),
         heights = c(lcm(mar[3]), 1, lcm(mar[1])))
  par(cex = 1)
  xlim = c(0, 1.06 * max(female, male))
  ylim = c(0, length(female))
  yt = 1:length(female)
  yb = yt - 1
  
  plot.new()
  plot.window(xlim = rev(xlim), ylim = ylim,
              xaxs = "i", yaxs = "i")
  panel.bg(xlim, ylim, "white")
  grid(ny = 0, col = "black")
  rect(0, yb, female, yt, col = col[1])
  axis(1)
  mtext(genderlabels[1], at = xlim[2], adj = 0, line = .25)
  box()
  
  plot.new()
  plot.window(xlim = xlim, ylim = ylim,
              xaxs = "i", yaxs = "i")
  panel.bg(xlim, ylim, "white")
  grid(ny = 0, col = "black")
  rect(0, yb, male, yt, col = col[2])
  axis(1)
  mtext(genderlabels[2], at = xlim[2], adj = 1, line = .25)
  box()
  
  plot.new()
  plot.window(xlim = c(0, 1), ylim = ylim,
              xaxs = "i", yaxs = "i")
  text(0.5, (yb + yt)/2, labels)
  mtext(main, line = 1.5, font = 2, cex = 1.25)
  mtext(xlab, line = 2.5, side = 1)
  
  par(opar)
}

###  Draw the background of a panel in the
###  the given colour

panel.bg = function(xlim, ylim, col="white")
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col=col, border=NA)
