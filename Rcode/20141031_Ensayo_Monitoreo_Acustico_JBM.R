### R code from vignette source '~/Dropbox/CEBA/doc/400_InventarioJardinBotanicoMaracaibo/Documento2_MonitoreoAcustico.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Documento2_MonitoreoAcustico.Rnw:54-68
###################################################
require(chron)
require(raster)
require(RColorBrewer)

paquetes <- (.packages())
paquetes <- paquetes[!(paquetes %in% c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base", "deldir", "DBI", "RMySQL"))]

luq <- function(x,contar.NA=FALSE) {
	if (contar.NA==F) {
	x <- x[!is.na(x)]
	}
 length(unique(x))
 }



###################################################
### code chunk number 2: citas paquetes
###################################################
cat(paste("\\emph{",paquetes,"} \\citep{pqt::",paquetes,"}",sep="",collapse="; "))


###################################################
### code chunk number 3: LeerDatos
###################################################
Puntos <- read.csv("~/CEBA/data/JardinBotanicoMaracaibo/20141028_MA_Puntos.csv")
Muestreo <- read.csv("~/CEBA/data/JardinBotanicoMaracaibo/20141028_MA_Muestreo.csv")
Grabaciones <- read.csv("~/CEBA/data/JardinBotanicoMaracaibo/20141028_MA_Grabaciones.csv")


###################################################
### code chunk number 4: Incertidumbre
###################################################
Puntos$DAP <- as.numeric(sub(",",".",as.character(Puntos$Distancia.al.punto)))
Puntos$DAP <- ifelse(is.na(Puntos$DAP),mean(Puntos$DAP,na.rm=T),Puntos$DAP)
Puntos$Error.GPS <- Puntos$EPE 
Puntos$Error.GPS <- ifelse(is.na(Puntos$Error.GPS),mean(Puntos$Error.GPS,na.rm=T),Puntos$Error.GPS)
Puntos$Extent <- 10
Puntos$Incertidumbre <- Puntos$DAP + Puntos$Error.GPS + Puntos$Extent



###################################################
### code chunk number 5: objetoEspacial
###################################################
coordinates(Puntos) <- c("Longitude","Latitude")
proj4string(Puntos) <- "+proj=longlat +datum=WGS84"
plot(Puntos)


###################################################
### code chunk number 6: mapaJardinTodo
###################################################
JBM.ll <- shapefile("~/CEBA/data/JardinBotanicoMaracaibo/ZonificacionJardin.shp")
plot(JBM.ll)
text(JBM.ll,"nombre",cex=.75)
points(Puntos,col=2)


###################################################
### code chunk number 7: mapaJardinMuestreo
###################################################
plot(JBM.ll[JBM.ll@data$id %in% c(7,8,11),])
text(JBM.ll,"nombre",cex=.75)
points(Puntos,col=2)


###################################################
### code chunk number 8: objetoEspacialProyectado
###################################################
pts <- spTransform(Puntos,CRS("+proj=utm +zone=19n +datum=WGS84"))
JBM.xy <- spTransform(JBM.ll,CRS("+proj=utm +zone=19n +datum=WGS84"))

plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,pch=3)
draw.arc(x=pts@coords[,1],
         y=pts@coords[,2],
         radius=pts@data$Incertidumbre,
         deg1=0,deg2=360,lty=2,
         col=c("grey77",1)[1+(pts@data$Anotador != "No muestreado")])



###################################################
### code chunk number 9: Cobertura
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Cobertura,cex=pts@data$Cobertura/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Cobertura",horiz=T)


###################################################
### code chunk number 10: Sustrato
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Sustrato,cex=as.numeric(pts@data$Sustrato)/2,pch=19)
legend("bottomright",legend=levels(pts@data$Sustrato),col=1:6,pch=19,pt.cex=c(1:6)/2,title="Sustrato",horiz=T)


###################################################
### code chunk number 11: Hojarasca
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=1+pts@data$Hojarasca,cex=(1+pts@data$Hojarasca)/2,pch=19)
legend("bottomright",legend=0:3,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Hojarasca",horiz=T)


###################################################
### code chunk number 12: DistribucionHorizontal
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Distribución.horizontal,cex=(pts@data$Distribución.horizontal)/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Distribución.horizontal",horiz=T)


###################################################
### code chunk number 13: AlturaEstratoSuperior
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Altura.estrato.superior,cex=(pts@data$Altura.estrato.superior)/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Altura.estrato.superior",horiz=T)


###################################################
### code chunk number 14: CuerpodeAgua
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=1+pts@data$Cuerpo.de.Agua,cex=(1+pts@data$Cuerpo.de.Agua)/2,pch=19)
legend("bottomright",legend=c("0","1"),col=1:2,pch=19,pt.cex=c(1:2)/2,title="Cuerpo.de.Agua",horiz=T)


###################################################
### code chunk number 15: areaEstudio
###################################################
rnull <- raster(extent(JBM.xy[JBM.xy@data$id %in% c(7,8,11),]),
                nrows=18, ncols=24)
mask <- distanceFromPoints(rnull,pts)<50

JBM.grd <- SpatialPixelsDataFrame(points=coordinates(mask)[values(mask)==1,],
                                  data=data.frame(values(mask)[values(mask)==1]),
                                  proj4string=JBM.xy@proj4string)

image(JBM.grd)
points(pts)


###################################################
### code chunk number 16: Documento2_MonitoreoAcustico.Rnw:211-212
###################################################
require(gstat)


###################################################
### code chunk number 17: variograma
###################################################
ss <- !is.na(pts@data$Cobertura)
v = variogram(I(Cobertura > 2)~1,pts[ss,])
plot(v)


###################################################
### code chunk number 18: variofit
###################################################
vm = fit.variogram(v, vgm(1, "Exp", 70, .1))
plot(v,vm)


###################################################
### code chunk number 19: indicatorKrigging
###################################################
ik = krige(I(Cobertura > 2)~1, pts[ss,], JBM.grd, vm)
colores <- colorRampPalette(brewer.pal(9,"BuGn"))
print(spplot(ik[1],col.regions=colores(30)))


###################################################
### code chunk number 20: modeloCobertura
###################################################
image(ik[1],col=colores(30))
points(pts,col=pts@data$Cobertura,cex=pts@data$Cobertura/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Cobertura",horiz=T)


###################################################
### code chunk number 21: semivarianceAgua
###################################################
ss <- !is.na(pts@data$Cuerpo.de.Agua)
v = variogram(I(Cuerpo.de.Agua > 0)~1,pts[ss,])
vm = fit.variogram(v, vgm(.3, "Exp", 100, 0.1))
plot(v,vm)


###################################################
### code chunk number 22: iKrAgua
###################################################
colores <- colorRampPalette(brewer.pal(9,"Blues"))
ik = krige(I(Cuerpo.de.Agua > 0)~1, pts[ss,], JBM.grd, vm)
spplot(ik[1],col.regions=colores(30))


###################################################
### code chunk number 23: ajustariKrAgua
###################################################
summary(ik[[1]])
ik[[1]][ik[[1]]<0] = 0
ik[[1]][ik[[1]]>1] = 1
summary(ik[[1]])



###################################################
### code chunk number 24: CuerposAguaModelo
###################################################
image(ik[1],col=colores(30))
points(pts,col=1,pch=c(1,19)[pts@data$Cuerpo.de.Agua+1])
legend("bottomright",legend=c("Ausente","Presente"),col=1,pch=c(1,19),pt.cex=1,title="Cuerpos de Agua",horiz=T)


###################################################
### code chunk number 25: mapa1
###################################################
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),],col="green",border="wheat")

legend("bottom",legend=c("no caracterizado","caracterizado sin grabación",NA,"una grabación","dos grabaciones","tres grabaciones"),pch=c(3,3,NA,19,19,19),pt.cex=c(.65,.65,NA,.85,1.25,1.75),horiz=F,col=c(2,1,NA,1,1,1),ncol=2)


ss <- pts@data$Anotador != "No muestreado"
points(pts,col=c("red",1)[1+(ss)],pch=3,cex=.65)
tt <- table(Grabaciones$Punto)
ss <- pts@data$Código %in% names(tt)[tt==1]
points(pts[ss,],pch=19,cex=.85)
ss <- pts@data$Código %in% names(tt)[tt==2]
points(pts[ss,],pch=19,cex=1.25)
ss <- pts@data$Código %in% names(tt)[tt==3]
points(pts[ss,],pch=19,cex=1.75)



###################################################
### code chunk number 26: HistFechas
###################################################
require(chron)
fechas <- chron(dates.=as.character(Muestreo$Fecha),
              times.=sprintf("%s:00",as.character(Muestreo$Hini)),
              format = c(dates = "ymd", times = "h:m:s"))
horas <- chron(times.=sprintf("%s:00",as.character(Muestreo$Hini)),
              format = c(dates = "ymd", times = "h:m:s"))
layout(1:2)
hist(fechas)
title(main="Muestreos por fechas")
hist(horas)
title(main="Muestreos por horas")


###################################################
### code chunk number 27: Striphoras
###################################################
par(mar=c(2,2,0,0))
stripchart(horas~Muestreo$Punto,axes=F)
axis(1,c(13,15,17,19,21)/24,c("1pm","3pm","5pm","7pm","9pm"))
axis(2,1:75,levels(Muestreo$Punto),
     las=2, cex.axis=.5)
box()
abline(h=15*c(1,2,3,4)+.5,lty=3)


###################################################
### code chunk number 28: Documento2_MonitoreoAcustico.Rnw:334-337
###################################################
table(Muestreo$Viento)
table(Muestreo$Ruido)
table(Muestreo$Cielo)


###################################################
### code chunk number 29: Documento2_MonitoreoAcustico.Rnw:342-344
###################################################
table(Muestreo$Posición.mic)
table(Muestreo$Sustrato.mic)


###################################################
### code chunk number 30: Documento2_MonitoreoAcustico.Rnw:349-351
###################################################
table(Muestreo$Humedad)
table(Muestreo$Temperatura)


###################################################
### code chunk number 31: HR
###################################################
HR <- as.numeric(sub(",",".",sub("%","",Muestreo$Humedad)))
plot(HR~horas)



###################################################
### code chunk number 32: Temp
###################################################
Temp <- as.numeric(sub(",",".",sub("°[CF]","",Muestreo$Temperatura)))
Temp[!is.na(Temp) & Temp>50] <- (Temp[!is.na(Temp) & Temp>50]-32)*5/9
plot(Temp~horas)


