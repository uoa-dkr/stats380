female =
  c(3.74, 3.89, 3.55, 3.58, 3.78,
    3.89, 4.17, 4.03, 3.58, 3.34,
    2.58, 2.21, 1.88, 1.87, 1.71,
    1.35, 0.96, 0.52, 0.19, 0.05)

male =
  c(3.98, 4.08, 3.75, 3.69, 3.73,
    3.66, 3.94, 3.85, 3.47, 3.32,
    2.58, 2.18, 1.86, 1.80, 1.43,
    0.93, 0.56, 0.23, 0.07, 0.01)

agegroup =
  c("0-4", "5-9", "10-14", "15-19", "20-24",
    "25-29", "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-59", "60-64", "65-69", "70-74",
    "75-79", "80-84", "85-89", "90-94", "95+")

ppyramid(female, male, agegroup, 
         main = "New Zealand Population, 1996 Census", 
         xlab = "Percentage in Agegroup")

ppyramid(female, male, agegroup, col = "gray90",
         main = "New Zealand Population, 1996 Census", 
         xlab = "Percentage in Agegroup") 

ppyramid(female, male, agegroup,
         col = hcl(c(0, 180)),
         main = "New Zealand Population, 1996 Census", 
         xlab = "Percentage in Agegroup",
         bg = "gray95")

ppyramid(male, female, agegroup,
         gender = c("Tane", "Wahine"),
         col = hcl(c(180, 0)),
         main = "Aotearoa Population, 1996 Census", 
         xlab = "Percentage in Agegroup",
         bg = "gray95") 







cols=hcl(h=c(231,145,90,351),c=64,l=75)
data$Species=factor(y)
par(mfrow=c(2,1),
    mar=c(5,2,0,0),
    oma=c(0,1,2,1))
nms=levels(data$Species)
hist(data$flush.dist, breaks = breaks, col = cols[1], ylab = '',
     main = '', xlab = '', xaxs='i', yaxs='i', ylim = c(0,48), las=1)

hist(data[data$Species%in%nms[-1],]$flush.dist,
     breaks = breaks, add = T, col = cols[2])

hist(data[data$Species%in%nms[3:4],]$flush.dist,
     breaks = breaks, col = cols[3], add = T)

hist(data[data$Species=='Stint',]$flush.dist, col = cols[4],
     xlim = c(0,20), add = T, breaks = 10)

mtext('Flush.dist',side = 1,font = 2,line = 2.5)
mtext('Frequency of Flush & Land distance',col='darkred',
      cex=1.47,outer=T,font=2,at=.535,line = .45)
legend('topright',legend = nms,cex = .71,lty = 1,lwd=9,col = cols[1:4])
box()

hist(data$land.dist, breaks = 20,col = cols[1], ylab = '',
     main = '', xlab = '', xaxs = 'i', yaxs = 'i',las = 1, ylim = c(0,65))

hist(data[data$Species%in%nms[-1],]$land.dist,
     breaks = breaks, add = T, col = cols[2])

hist(data[data$Species%in%nms[3:4],]$land.dist,
     col = cols[3], xlim = c(0,200), breaks = 10, add = T)

hist(data[data$Species=='Stint',]$land.dist,
     col = cols[4], xlim = c(0,200), add = T)

mtext('Land.dist',side = 1,font = 2,line = 2.5)
legend('topright',legend = nms,cex = .71,lty = 1,lwd=9,col = cols[1:4])
box()