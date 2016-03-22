xyplot(Ozone ~ Wind, data = airquality, pch=8, col="red", main="Big Apple Data")
xyplot(Ozone ~ Wind | as.factor(Month), data = airquality, layout=c(5,1))
xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(5,1))
p <- xyplot(Ozone ~ Wind, data = airquality)
print(p)
names(p)
mynames[myfull]
p[["formula"]]
p[["x.limits"]]
table(f)
xyplot(y ~ x | f, layout = c(2, 1))
v1
v2
myedit("plot1.R")
source(pathtofile("plot1.R"),local=TRUE) 
myedit("plot2.R")
source(pathtofile("plot2.R"),local=TRUE)
str(diamonds)
diamonds $color
table(diamonds $color)
table(diamonds $color, diamonds $cut)


# ggplot
qplot(displ, hwy, data = mpg)
