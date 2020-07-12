ZJBM <- shapefile("~/CEBA/data/JardinBotanicoMaracaibo/ZonificacionJardin.shp")
plot(ZJBM[ZJBM@data$id!=1,])
rnull <- raster(extent(ZJBM))
rnull <- disaggregate(rnull,5)

rzona <- rasterize(ZJBM[ZJBM@data$id!=1,],rnull)
rzona <- rzona>0
xys <- coordinates(rzona)
require(spatstat)
mtz <- t(matrix(rev(values(rzona)),ncol=luq(xys[,2]),nrow=luq(xys[,1])))
##mtz <- mtz[nrow(mtz):1,]
mtz <- mtz[,ncol(mtz):1]
myim <- im(mtz,
           xcol=unique(xys[,1]),yrow=unique(xys[,2]))
plot(myim)
plot(ZJBM,add=T)

rpts <- rpoint(105, f=myim)

plot(ZJBM)
points(y~x,rpts)


## solo en la zona de las lagunas
slc <- ZJBM@data$id %in% c(7,8,11)
plot(ZJBM[slc,])


rzona <- rasterize(ZJBM[slc,],rnull)
rzona <- rzona>0
xys <- coordinates(rzona)
require(spatstat)
mtz <- t(matrix(rev(values(rzona)),ncol=luq(xys[,2]),nrow=luq(xys[,1])))
##mtz <- mtz[nrow(mtz):1,]
mtz <- mtz[,ncol(mtz):1]
myim <- im(mtz,
           xcol=unique(xys[,1]),yrow=unique(xys[,2]))
plot(myim)
plot(ZJBM,add=T)

n <- 105 ## localidades
r <- 3 ## repeticiones
g <- 20 ## grabacion
i <- 5 ## intervalo entre grabaciones
m <- 10 ## microfonos

n/m * (g+i)/60 ##

set.seed(12495567)
rpts <- rpoint(n, f=myim)

par(mar=c(0,0,0,0))
plot(ZJBM)
points(y~x,rpts,pch=3,col=2,cex=.8)
text(ZJBM,"nombre",cex=.8)
dev.copy(png,"MuestreoAnfibiosJBM.png")
dev.off()

par(mar=c(4,4,1,1))
plot(ZJBM,xlim=range(rpts$x),ylim=range(rpts$y))
points(y~x,rpts[1:15],pch=c(19,rep(1,13),4))

with(subset(rpts,(1:100)<21),
     segments(x[-15],y[-15],x[-1],y[-1],lty=3))
dev.copy(png,"EjemploSecuenciaMuestreo.png")
dev.off()

system("mkdir ~/CEBA/lib/gpx/JBM")

dts <- data.frame(Longitude=rpts$x,
                Latitude=rpts$y,
                Name=sprintf("%s%02d",rep(LETTERS[1:7],rep(15,7)),rep(1:15,7))
                )
write.csv(file="~/CEBA/lib/gpx/JBM/20141006_EnsayoJBM.csv",
         dts,row.names=F,quote=F)
system("gpsbabel -i unicsv -f /home/jferrer/CEBA/lib/gpx/JBM/20141006_EnsayoJBM.csv -o gpx -F /home/jferrer/CEBA/lib/gpx/JBM/20141006_EnsayoJBM.gpx")

for (k in LETTERS[1:7]) {
  write.csv(file=sprintf("~/CEBA/lib/gpx/JBM/20141006_EnsayoJBM_%s.csv",k),
            subset(dts,grepl(k,Name)),row.names=F,quote=F)
##system(sprintf("gpsbabel -i unicsv -f /home/jferrer/CEBA/lib/gpx/JBM/20141006_EnsayoJBM_%s.csv -o gpx -F /home/jferrer/CEBA/lib/gpx/JBM/20141006_EnsayoJBM_%s.gpx",k,k))
system(sprintf("gpsbabel -i unicsv -f /home/jferrer/CEBA/lib/gpx/JBM/20141006_EnsayoJBM_%s.csv -o kml -F /home/jferrer/CEBA/lib/gpx/JBM/20141006_EnsayoJBM_%s.kml",k,k))
}

#### respaldo viejos
##sudo gpsbabel -i garmin -f /dev/ttyUSB0 -o gpx -F viejos/G04/20141006_viejos.gpx
##sudo gpsbabel -t -i garmin -f /dev/ttyUSB0 -o gpx -F viejos/G04/20141006_viejos_tracks.gpx

#### cargar nuevos
##sudo gpsbabel -i gpx -f JBM/20141006_EnsayoJBM.gpx -o garmin -F usb:
##sudo gpsbabel -i gpx -f JBM/20141006_EnsayoJBM_C.gpx -o garmin -F /dev/ttyUSB0

########### arreglar problema gps
## agregar archivo
##/etc/udev/rules.d/51-garmin.rules
## con contenido
##SUBSYSTEM=="usb", ATTRS{idVendor}=="091e", ATTRS{idProduct}=="0003", MODE="666"

## y luego
##sudo udevadm control --reload-rules




rpD <- pointDistance(cbind(rpts$x,rpts$y),longlat=T)
 diag(rpD) <- NA
apply(rpD,1,min,na.rm=T)


sum(apply(rpD[81:100,81:100],1,min,na.rm=T)[-1])
summary(diag(rpD[-1,-100]))


## todo el jardin pero mas densidad en la zona de las lagunas
slc <- ZJBM@data$id %in% c(7,8,11)
plot(ZJBM[slc,])


rzona <- rasterize(ZJBM[ZJBM@data$id != 1,],rnull)
r1 <- rzona %in% c(1,7,9)
r2 <- rzona>0  
rzona <- (r1*2) +r2
xys <- coordinates(rzona)
require(spatstat)
mtz <- t(matrix(rev(values(rzona)),ncol=luq(xys[,2]),nrow=luq(xys[,1])))
##mtz <- mtz[nrow(mtz):1,]
mtz <- mtz[,ncol(mtz):1]
myim <- im(mtz,
           xcol=unique(xys[,1]),yrow=unique(xys[,2]))
plot(myim)
plot(ZJBM,add=T)

n <- 100 ## localidades
r <- 3 ## repeticiones
g <- 10 ## grabacion
i <- 5 ## intervalo entre grabaciones
m <- 10 ## microfonos

n/m * (g+i)/60


rpts <- rpoint(n, f=myim)

plot(ZJBM)
points(y~x,rpts)

