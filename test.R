library(mvcwt)


###DATA INPUT
gazel_dat <- read.csv('abund_Gazel-Damie.csv')
gazel_dat$date <- as.Date(gazel_dat$date, format = "%m/%d/%Y")

#Date starts at Julian Day of 327
#Following the Tim Kiett's standard 
#format(gazel_dat$date[1],format="%j")

gazel_dat$Day_Length <- c(327,diff(gazel_dat$date))
gazel_dat$Day_Year <- cumsum(gazel_dat$Day_Length)
gazel_dat$Day_Year_Y <- (gazel_dat$Day_Year)/365.25


plot(gazel_dat$Day_Year_Y, log(gazel_dat$Di.gazel+1),type='l')

x = gazel_dat$Day_Year_Y
y = log(gazel_dat$Di.gazel+1)
w = mvcwt(x, y, min.scale = get.min.scale(x), max.scale = get.max.scale(x))


w2<-(w$z)
w3<-matrix(w2, nrow=1356, ncol=1356 )

a <- w$y

image(w,fun=z)
rec = 0.6*apply(Re(w$z)/sqrt(a),1,sum)/(0.776*(pi^(-1/4)))


sel=a<1

rec2=0.6*apply(Re(w3[,sel])/sqrt(a[sel]),1,sum)/(0.776*(pi^(-1/4)))
plot(scale(rec2),type='l')
lines(scale( log(gazel_dat$Di.gazel+1)),col='blue')

rec3= 2*(0.6*apply(Mod(w3[,sel])/sqrt(a[sel]),1,sum)/(0.776*(pi^(-1/4))))
plot(gazel_dat$Day_Year_Y,rec3,type='l')

MannKendall(rec3)
