opar = par(mfrow = c(ncol(x), ncol(x)),
           mar = rep(0, 4),
           oma = c(4.1, 4.1, 6.1, 4.1))
par(opar)
n=4
x=iris[,-5]
for(i in 1:n){
  for(j in 1:n) {
    xlim = range(x[,j])
    ylim = range(x[,i])
    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    if (i == j)
      text(mean(xlim), mean(ylim),
           colnames(x)[j])
    else
      points(x[,j], x[,i])
    box() 
    if (i == 1 && j %% 2 == 0)
      axis(3)
    if (i == n && j %% 2 == 1)
      axis(1)
    if (j == 1 && i %% 2 == 0)
      axis(2)
    if (j == n && i %% 2 == 1)
      axis(4)
  }
}
title("Anderson's Iris Data", outer = TRUE, font=2)

