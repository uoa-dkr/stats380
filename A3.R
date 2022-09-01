url = 'https://www.stat.auckland.ac.nz/~yongwang/stats380/population-by-country.html'
readin = readLines(url);length(readin)
grep('/world-population/',readin)


readin[11]
sep = strsplit(readin[11],'-population/\">');sep
subs = sub('<.+$', '', sep[[1]])[-1:-2]

mod = grep('&amp;',subs)
subs[mod] <- sub('&amp;','&',subs[mod]);subs

subs[grep(';',subs)] # 53 162 193

info1[[235]]
matrix[230:235,];matrix[230:235,8:10]
matrix[,7:10]
matrix[235,10]
matrix[,c(1,3,5,6)]

z = matrix(1:9,nr=3);z
z[2,2] <- as.character(z[2,2]);z
df









