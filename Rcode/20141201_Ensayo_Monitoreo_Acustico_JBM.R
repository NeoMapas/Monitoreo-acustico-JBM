### R code from vignette source '~/Dropbox/CEBA/doc/400_InventarioJardinBotanicoMaracaibo/Documento2_MonitoreoAcustico.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Documento2_MonitoreoAcustico.Rnw:54-69
###################################################
require(chron)
require(raster)
require(RColorBrewer)
require(plotrix)

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
colnames(pts@coords) <- c("x","y")

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
layout(matrix(1:6,ncol=2))
par(mar=c(0,0,0,0))
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Cobertura,cex=pts@data$Cobertura/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Cobertura",horiz=T)
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Sustrato,cex=as.numeric(pts@data$Sustrato)/2,pch=19)
legend("bottomright",legend=levels(pts@data$Sustrato),col=1:6,pch=19,pt.cex=c(1:6)/2,title="Sustrato",horiz=T)

plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=1+pts@data$Hojarasca,cex=(1+pts@data$Hojarasca)/2,pch=19)
legend("bottomright",legend=0:3,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Hojarasca",horiz=T)
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Distribución.horizontal,cex=(pts@data$Distribución.horizontal)/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Distribución.horizontal",horiz=T)
plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=pts@data$Altura.estrato.superior,cex=(pts@data$Altura.estrato.superior)/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Altura.estrato.superior",horiz=T)

plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),])
points(pts,col=1+pts@data$Cuerpo.de.Agua,cex=(1+pts@data$Cuerpo.de.Agua)/2,pch=19)
legend("bottomright",legend=c("0","1"),col=1:2,pch=19,pt.cex=c(1:2)/2,title="Cuerpo.de.Agua",horiz=T)


###################################################
### code chunk number 10: areaEstudio
###################################################
rnull <- raster(extent(JBM.xy[JBM.xy@data$id %in% c(7,8,11),]),
                nrows=18, ncols=24)
mask <- distanceFromPoints(rnull,pts)<50

##coordinates(mask)[values(mask)==1,]

JBM.grd <- SpatialPixelsDataFrame(points=xyFromCell(mask,1:ncell(mask))[values(mask)==1,],
                                  data=data.frame(values(mask)[values(mask)==1]),
                                  proj4string=JBM.xy@proj4string)

image(JBM.grd)
points(pts)


###################################################
### code chunk number 11: Documento2_MonitoreoAcustico.Rnw:198-199
###################################################
require(gstat)


###################################################
### code chunk number 12: variograma
###################################################
ss <- !is.na(pts@data$Cobertura)
v = variogram(I(Cobertura > 2)~1,pts[ss,])
plot(v)


###################################################
### code chunk number 13: variofit
###################################################
vm = fit.variogram(v, vgm(1, "Exp", 70, .1))
plot(v,vm)


###################################################
### code chunk number 14: indicatorKrigging
###################################################
ik = krige(I(Cobertura > 2)~1, pts[ss,], JBM.grd, vm)
colores <- colorRampPalette(brewer.pal(9,"BuGn"))
print(spplot(ik[1],col.regions=colores(30)))


###################################################
### code chunk number 15: modeloCobertura
###################################################
image(ik[1],col=colores(30))
points(pts,col=pts@data$Cobertura,cex=pts@data$Cobertura/2,pch=19)
legend("bottomright",legend=1:4,col=1:4,pch=19,pt.cex=c(1:4)/2,title="Cobertura",horiz=T)


###################################################
### code chunk number 16: semivarianceAgua
###################################################
ss <- !is.na(pts@data$Cuerpo.de.Agua)
v.Ag = variogram(I(Cuerpo.de.Agua > 0)~1,pts[ss,])
vm.Ag = fit.variogram(v.Ag, vgm(.3, "Exp", 100, 0.1))
print(plot(v.Ag,vm.Ag))


###################################################
### code chunk number 17: iKrAgua
###################################################
colores <- colorRampPalette(brewer.pal(9,"Blues"))
ik.Ag = krige(I(Cuerpo.de.Agua > 0)~1, pts[ss,], JBM.grd, vm.Ag)
spplot(ik.Ag[1],col.regions=colores(30))


###################################################
### code chunk number 18: ajustariKrAgua
###################################################
summary(ik.Ag[[1]])
ik.Ag[[1]][ik.Ag[[1]]<0] = 0
ik.Ag[[1]][ik.Ag[[1]]>1] = 1
summary(ik.Ag[[1]])



###################################################
### code chunk number 19: CuerposAguaModelo
###################################################
image(ik.Ag[1],col=colores(30))
points(pts,col=1,pch=c(1,19)[pts@data$Cuerpo.de.Agua+1])
legend("bottomright",legend=c("Ausente","Presente"),col=1,pch=c(1,19),pt.cex=1,title="Cuerpos de Agua",horiz=T)


###################################################
### code chunk number 20: semivarianceHojarasca
###################################################
ss <- !is.na(pts@data$Hojarasca)
v = variogram(I(Hojarasca > 0)~1,pts[ss,])
vm = fit.variogram(v, vgm(.3, "Exp", 100, 0.1))
##plot(v,vm)
colores <- colorRampPalette(brewer.pal(9,"Greens"))
ik.Hj = krige(I(Hojarasca > 0)~1, pts[ss,], JBM.grd, vm)
spplot(ik.Hj[1],col.regions=colores(30))


###################################################
### code chunk number 21: mapa1
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
### code chunk number 22: HistFechas
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
### code chunk number 23: Striphoras
###################################################
par(mar=c(2,2,0,0))
stripchart(horas~Muestreo$Punto,axes=F)
axis(1,c(13,15,17,19,21)/24,c("1pm","3pm","5pm","7pm","9pm"))
axis(2,1:75,levels(Muestreo$Punto),
     las=2, cex.axis=.5)
box()
abline(h=15*c(1,2,3,4)+.5,lty=3)


###################################################
### code chunk number 24: Documento2_MonitoreoAcustico.Rnw:334-337
###################################################
table(Muestreo$Viento)
table(Muestreo$Ruido)
table(Muestreo$Cielo)


###################################################
### code chunk number 25: Documento2_MonitoreoAcustico.Rnw:342-344
###################################################
table(Muestreo$Posición.mic)
table(Muestreo$Sustrato.mic)


###################################################
### code chunk number 26: Documento2_MonitoreoAcustico.Rnw:349-351
###################################################
table(Muestreo$Humedad)
table(Muestreo$Temperatura)


###################################################
### code chunk number 27: HR
###################################################
HR <- as.numeric(sub(",",".",sub("%","",Muestreo$Humedad)))
plot(HR~horas)



###################################################
### code chunk number 28: Temp
###################################################
Temp <- as.numeric(sub(",",".",sub("°[CF]","",Muestreo$Temperatura)))
Temp[!is.na(Temp) & Temp>50] <- (Temp[!is.na(Temp) & Temp>50]-32)*5/9
plot(Temp~horas)


###################################################
### code chunk number 29: LeerDatosDetecciones
###################################################
require(gdata)
VM <- read.xls("~/CEBA/data/JardinBotanicoMaracaibo/Occupancy_HistCapt_20141124.xlsx",as.is=T)

VM$Código <- sapply(VM$Archivo,function(x) strsplit(x,"_")[[1]][1])
VM$fecha <- as.numeric(sapply(VM$Archivo,function(x) strsplit(x,"_")[[1]][2]))
VM$hora <- sapply(VM$Archivo,function(x) {
  y <- strsplit(x,"_")[[1]][3]
  as.numeric(substr(y,1,2)) +
    as.numeric(substr(y,3,4))/60
})




###################################################
### code chunk number 30: EspeciesDetectadas
###################################################
colSums(VM[,3:9])


###################################################
### code chunk number 31: MetodoDeteccion
###################################################
table(VM$Método)


###################################################
### code chunk number 32: DatosCurvaAcumulacion
###################################################
spps <- prds <- sfrzs <- c()
spp2 <- prd2 <- sfrz2 <- c()
for (k in 1:nrow(VM.df)) {
  j <- sample(1:nrow(VM.df),k)
  ds <- VM.df@data[j,]
  spps <- c(spps,sum(colSums(ds[,c("Lf", "Hp", "Dm", "Rh", "Rm", "Pb", "SpA"),drop=F])>0))
  prds <- c(prds,length(unique(ds$Código)))
  sfrzs <- c(sfrzs, nrow(ds)*5)

  ds <- subset(ds,hora>17)
  spp2 <- c(spp2,sum(colSums(ds[,c("Lf", "Hp", "Dm", "Rh", "Rm", "Pb", "SpA"),drop=F])>0))
  prd2 <- c(prd2,length(unique(ds$Código)))
  sfrz2 <- c(sfrz2, nrow(ds)*5)


}

sfrzs <- sfrzs/prds
sfrz2 <- sfrz2/prd2


###################################################
### code chunk number 33: Documento2_MonitoreoAcustico.Rnw:463-497
###################################################
##STAR <- try(nls(spps~C*(prds^x)*(sfrzs^z), start=c(C=10, x=-.1, z=.3)))
STAR <- try(nls(spps~C+(alpha * log(prds)+1)+(beta * log(sfrzs)+1), start=c(C=10, alpha=-.2, beta=3)))
STAR2 <- try(nls(spp2~C+(alpha * log(prd2)+1)+(beta * log(sfrz2)+1), start=c(C=10, alpha=-.2, beta=3)))

layout(matrix(1:2,ncol=2))
plot(prds,spps,pch=NA,log="", ylim=c(0,10), xlim=c(1,80),xlab="puntos", ylab="especies",axes=F,main="Todos")
box()
axis(1) 
axis(2)
symbols(prds,spps,circles=sfrzs, inches=.05, bg=rgb(.49,.39,.29), fg=rgb(.77,.67,.57),add=T)
nw.dt <- data.frame(prds=seq(1, 80,  length=50), sfrzs= rep(5,50))
lines(seq(1,100,length=50), predict(STAR, nw.dt),col="cyan")
nw.dt$sfrzs <- 6
  lines(seq(1,100,length=50), predict(STAR, nw.dt), col="green")
  nw.dt$sfrzs <- 8
  lines(seq(1,100,length=50), predict(STAR, nw.dt), col="blue",lty=1)


plot(prd2,spp2,pch=NA,log="", ylim=c(0,10), xlim=c(1,80),ylab="especies", xlab="puntos",axes=F,main="hora>5pm")
box()
axis(1) 
axis(2)
symbols(prd2,spp2,circles=sfrz2, inches=.05, bg=rgb(.49,.39,.29), fg=rgb(.77,.67,.57),add=T)
nw.dt <- data.frame(prd2=seq(1, 80,  length=50), sfrz2= rep(5,50))
lines(seq(1,100,length=50), predict(STAR2, nw.dt),col="cyan")
nw.dt$sfrz2 <- 6
  lines(seq(1,100,length=50), predict(STAR2, nw.dt), col="green")
  nw.dt$sfrz2 <- 8
  lines(seq(1,100,length=50), predict(STAR2, nw.dt), col="blue",lty=1)


summary(STAR)




###################################################
### code chunk number 34: ObjetoEspacialDetecciones
###################################################
ss <- pts@data$Código %in% VM$Código
pts@data$Lf <- pts@data$Código %in% subset(VM,Lf> 0)$Código
pts@data$Hp <- pts@data$Código %in% subset(VM,Hp> 0)$Código
pts@data$Pb <- pts@data$Código %in% subset(VM,Pb> 0)$Código
pts@data$Dm <- pts@data$Código %in% subset(VM,Dm> 0)$Código
pts@data$Rh <- pts@data$Código %in% subset(VM,Rh> 0)$Código
pts@data$Rm <- pts@data$Código %in% subset(VM,Rm> 0)$Código
pts@data$SpA <- pts@data$Código %in% subset(VM,SpA> 0)$Código

colores <- colorRampPalette(brewer.pal(9,"Greens"))


###################################################
### code chunk number 35: Variogramas
###################################################
layout(matrix(1:8,ncol=2))
par(mar=c(3,3,4,1))
v.Dm = variogram(Dm~poly(x,y,degree=2),pts[ss,])
##v.Dm = variogram(Dm~1,pts[ss,])
vm.Dm = fit.variogram(v.Dm, vgm(0.1, "Mat", 10, 0))
plot(gamma~dist,v.Dm,main="Dm",ylim=c(0,.35))
lines(variogramLine(vm.Dm,maxdist=250),col=2)

v.SpA = variogram(SpA~y,pts[ss,])
vm.SpA = fit.variogram(v.SpA, vgm(.01, "Mat", 10, 0.1))
plot(gamma~dist,v.SpA,main="SpA",ylim=c(0,.035))
lines(variogramLine(vm.SpA,maxdist=250),col=2)
##plot(v.SpA,vm.SpA)

v.Lf = variogram(Lf~poly(x,y,degree=1),pts[ss,])
vm.Lf = fit.variogram(v.Lf, vgm(.3, "Exp", 100, 0.1))
##plot(v.Lf,vm.Lf)
plot(gamma~dist,v.Lf,main="Lf",ylim=c(0,.35))
lines(variogramLine(vm.Lf,maxdist=250),col=2)


v.Hp = variogram(Hp~poly(x,y,degree=1),pts[ss,])
vm.Hp = fit.variogram(v.Hp, vgm(.3, "Exp", 100, 0.1))
##plot(v.Hp,vm.Hp)
plot(gamma~dist,v.Hp,main="Hp",ylim=c(0,.35))
lines(variogramLine(vm.Hp,maxdist=250),col=2)

v.Pb = variogram(Pb~poly(x,y,degree=2),pts[ss,])
vm.Pb = fit.variogram(v.Pb, vgm(.01, "Sph", 10, 0.1))
##plot(v.Pb,vm.Pb)
plot(gamma~dist,v.Pb,main="Pb",ylim=c(0,.35))
lines(variogramLine(vm.Pb,maxdist=250),lty=2)


##v.Rm = variogram(Rm~poly(x,y,degree=2),pts[ss,])
v.Rm = variogram(Rm~1,pts[ss,])
vm.Rm = fit.variogram(v.Rm, vgm(0.1, "Nug", 0))
plot(gamma~dist,v.Rm,main="Rm",ylim=c(0,.35))
lines(variogramLine(vm.Rm,maxdist=250),lty=2)

v.Rh = variogram(Rh~poly(x,y,degree=2),pts[ss,])
vm.Rh = fit.variogram(v.Rh, vgm(0.1, "Nug", 0))
##plot(v.Rh,vm.Rh)
plot(gamma~dist,v.Rh,main="Rh",ylim=c(0,.35))
lines(variogramLine(vm.Rh,maxdist=250),lty=2)



###################################################
### code chunk number 36: KrModelos
###################################################
ik.Dm = krige(Dm~poly(x,y,degree=2), pts[ss,], JBM.grd, vm.Dm)
ik.SpA = krige(SpA~y, pts[ss,], JBM.grd, vm.SpA)
ik.Lf = krige(Lf~poly(x,y,degree=1), pts[ss,], JBM.grd, vm.Lf)
ik.Hp = krige(Hp~poly(x,y,degree=1), pts[ss,], JBM.grd, vm.Hp)
ik.Pb = krige(Pb~1, pts[ss,], JBM.grd)
ik.Rm = krige(Rm~1, pts[ss,], JBM.grd)
ik.Rh = krige(Rh~1, pts[ss,], JBM.grd)



###################################################
### code chunk number 37: iKrDm
###################################################
layout(matrix(1:8,ncol=2))
par(mar=c(0,0,0,0))
for (spp in c("Dm","SpA","Lf","Hp","Pb","Rm","Rh")) {
  ik.spp <- get(paste("ik",spp,sep="."))
  ik.spp[[1]][ik.spp[[1]]<0] = 0
  ik.spp[[1]][ik.spp[[1]]>1] = 1
  image(ik.spp[1],col=colores(30),breaks=seq(0,1,length=31))
  points(pts[ss,],col=1,pch=c(1,19)[pts@data[ss,spp]+1])
  legend("bottomright",legend=c("No detectado","Detectado"),col=1,pch=c(1,19),pt.cex=.7,title=spp,horiz=T,cex=.7)
}


###################################################
### code chunk number 38: iKrVar
###################################################
colores <- colorRampPalette(brewer.pal(9,"Greens"))
colores <- colorRampPalette(brewer.pal(9,"Oranges"))
layout(matrix(1:4,ncol=2))
par(mar=c(0,0,0,0))
for (spp in c("Dm","SpA","Lf","Hp")) {
  ik.spp <- get(paste("ik",spp,sep="."))
  image(ik.spp[2],col=colores(30),breaks=seq(0,ifelse(spp %in% "SpA",.03,.3),
                                    length=31))
  points(pts[ss,],col=1,pch=c(1,19)[pts@data[ss,spp]+1])
  legend("bottomright",legend=c("No detectado","Detectado"),col=1,pch=c(1,19),pt.cex=.7,title=spp,horiz=T,cex=.7)
}


###################################################
### code chunk number 39: Documento2_MonitoreoAcustico.Rnw:618-629
###################################################

area.est <- data.frame(especie=c("Dm","SpA","Lf","Hp","Pb","Rm","Rh"),
                       area.m2.krig=c(prod(ik.Hp@grid@cellsize)*sum(ik.Dm[[1]]),
                         prod(ik.Hp@grid@cellsize)*sum(ik.SpA[[1]]),
                         prod(ik.Hp@grid@cellsize)*sum(ik.Lf[[1]]),
                         prod(ik.Hp@grid@cellsize)*sum(ik.Hp[[1]]),
                         prod(ik.Hp@grid@cellsize)*sum(ik.Dm[[1]]),
                         prod(ik.Hp@grid@cellsize)*sum(ik.Rm[[1]]),
                         prod(ik.Hp@grid@cellsize)*sum(ik.Rh[[1]]))
                       )
area.est


###################################################
### code chunk number 40: DeteccionHora
###################################################
y <- rowSums(VM[,3:9])>0
x <- VM$hora
plot(x,y,xlab="hora",ylab="detecciones de una o más especies")
mdet <- glm(y~x,family=binomial(logit))
x <- seq(12,22,length=30)
lines(x,predict(mdet,data.frame(x),type="response"))

wdet <- predict(mdet,type="response")


###################################################
### code chunk number 41: ObjetoDeteccionesPrediccion
###################################################

VM.df <- cbind(VM,
              pts[match(VM$Código,pts$Código),c("Sustrato","Hojarasca","Distribución.horizontal","Cobertura", "Altura.estrato.superior","Cuerpo.de.Agua")],
              pts@coords[match(VM$Código,pts$Código),],
              Muestreo[match(VM$Archivo,with(Muestreo,paste(Punto,Fecha,sub(":","",Hinicio),sep="_"))),c("Posición.mic","Sustrato.mic", "Micrófono", "Viento", "Ruido", "Cielo","Temperatura", "Humedad")],
               wdet=wdet)

coordinates(VM.df) <- c("x","y")
proj4string(VM.df) <- pts@proj4string
VM.df$Hojarasca <- over(VM.df,ik.Hj)[,1]
VM.df$Cuerpo.de.Agua <- over(VM.df,ik.Ag)[,1]


###################################################
### code chunk number 42: Documento2_MonitoreoAcustico.Rnw:667-677
###################################################
mdl00.Dm <- glm(Dm~poly(x,y,degree=2)+Hojarasca+Cuerpo.de.Agua,VM.df,family=quasibinomial(logit),weight=wdet)
mdl00.Lf <- glm(Lf~poly(x,y,degree=2)+Hojarasca+Cuerpo.de.Agua,VM.df,family=quasibinomial(logit),weight=wdet)
mdl00.Hp <- glm(Hp~poly(x,y,degree=2)+Hojarasca+Cuerpo.de.Agua,VM.df,family=quasibinomial(logit),weight=wdet)
mdl00.Rh <- glm(Rh~poly(x,y,degree=2)+Hojarasca+Cuerpo.de.Agua,VM.df,family=quasibinomial(logit),weight=wdet)

mdl00.SpA <- glm(SpA~poly(x,y,degree=1),VM.df,family=quasibinomial(logit),weight=wdet)
mdl00.Pb <- glm(Pb~poly(x,y,degree=1)+Cuerpo.de.Agua,VM.df,family=quasibinomial(logit),weight=wdet)
mdl00.Rm <- glm(Rm~poly(x,y,degree=1)+Cuerpo.de.Agua,VM.df,family=quasibinomial(logit),weight=wdet)




###################################################
### code chunk number 43: Documento2_MonitoreoAcustico.Rnw:683-704
###################################################

nwdt <- data.frame(ik.Hj@coords,Hojarasca=ik.Hj@data[,1],Cuerpo.de.Agua=ik.Ag@data[,1])

prd00 <- ik
prd01 <- ik

colores <- colorRampPalette(brewer.pal(9,"Greens"))
layout(matrix(1:8,ncol=2))
par(mar=c(0,0,0,0))
for (spp in c("Dm","SpA","Lf","Hp","Pb","Rm","Rh")) {
  mdl00 <- get(paste("mdl00",spp,sep="."))
  prd <- predict(mdl00,nwdt,type="response",se.fit=T)
  prd00@data[,spp] <- prd$fit
  prd01@data[,spp] <- prd$se.fit
  image(prd00[spp],col=colores(30),breaks=seq(0,1,
                                    length=31))
  points(pts[ss,],col=1,pch=c(1,19)[pts@data[ss,spp]+1])
  legend("bottomright",legend=c("No detectado","Detectado"),col=1,pch=c(1,19),pt.cex=.7,title=spp,horiz=F,cex=.7)
}




###################################################
### code chunk number 44: Documento2_MonitoreoAcustico.Rnw:709-719
###################################################
colores <- colorRampPalette(brewer.pal(9,"Oranges"))
layout(matrix(1:8,ncol=2))
par(mar=c(0,0,0,0))
for (spp in c("Dm","SpA","Lf","Hp","Pb","Rm","Rh")) {
  image(prd01[spp],col=colores(30),breaks=seq(0,.3,
                                    length=31))
  points(pts[ss,],col=1,pch=c(1,19)[pts@data[ss,spp]+1])
  legend("bottomright",legend=c("No detectado","Detectado"),col=1,pch=c(1,19),pt.cex=.7,title=spp,horiz=F,cex=.7)
}



###################################################
### code chunk number 45: Documento2_MonitoreoAcustico.Rnw:726-731
###################################################

v.resid = variogram(resid(mdl00.Lf)~1,VM.df)
plot(v.resid)




###################################################
### code chunk number 46: Documento2_MonitoreoAcustico.Rnw:736-747
###################################################

area.est$area.m2.glm <- round(c(prod(prd00@grid@cellsize)*sum(prd00[["Dm"]]),
                          prod(prd00@grid@cellsize)*sum(prd00[["SpA"]]),
                          prod(prd00@grid@cellsize)*sum(prd00[["Lf"]]),
                          prod(prd00@grid@cellsize)*sum(prd00[["Hp"]]),
                          prod(prd00@grid@cellsize)*sum(prd00[["Pb"]]),
                          prod(prd00@grid@cellsize)*sum(prd00[["Rm"]]),
                          prod(prd00@grid@cellsize)*sum(prd00[["Rh"]])),3)
area.est
 



###################################################
### code chunk number 47: PrepararObjetoOccu
###################################################
require(unmarked)


fch <- hrs <- mtz <- matrix(nrow=length(unique(VM$Código)),
                            ncol=3,dimnames=list(unique(VM$Código),1:3))

for (j in unique(VM$Código)) {
  fch[j,] <- c(subset(VM,Código %in% j & Carpeta %in% "A5M")$fecha,NA,NA)[1:3]
  hrs[j,] <- c(subset(VM,Código %in% j & Carpeta %in% "A5M")$hora,NA,NA)[1:3]
}

fch <- fch-20141008

sC <- cbind(VM.df@coords[match(rownames(mtz),VM.df@data$Código),],VM.df@data[match(rownames(mtz),VM.df@data$Código),c("Sustrato","Hojarasca","Distribución.horizontal","Cobertura","Altura.estrato.superior","Cuerpo.de.Agua")])

oC <- list(fecha=fch,
           hora=(hrs-mean(hrs,na.rm=T))/sd(as.vector(hrs),na.rm=T))

for (spp in c("Lf","Hp","Rm","Rh","Dm","Pb","SpA")) {
mtz <- matrix(nrow=length(unique(VM$Código)),
                            ncol=3,dimnames=list(unique(VM$Código),1:3))

  for (j in unique(VM$Código)) {
    mtz[j,] <- c(subset(VM,Código %in% j & Carpeta %in% "A5M")[,spp],NA,NA)[1:3]
  }
  assign(sprintf("uF.%s",spp),unmarkedFrameOccu(mtz, 
                                                 siteCovs=sC,
                                                 obsCovs=oC))
}

##plot(mtz~hrs)




###################################################
### code chunk number 48: Documento2_MonitoreoAcustico.Rnw:793-806
###################################################
## no converge
mdlhA.Rm <- occu(~1 ~ 1, uF.Rm[-17,])

## no converge el modelo completo
mdlhA.SpA <- occu(~1 ~ 1, uF.SpA[-17,])
mdlhA.Dm <- occu(~hora ~ Hojarasca, uF.Dm[-17,],start=c(-2,0,-2,2))

##Convergen
mdlhA.Pb <- occu(~hora ~ poly(x,degree=2)+Cuerpo.de.Agua+Hojarasca, uF.Pb[-17,])
mdlhA.Lf <- occu(~hora ~ poly(x,degree=2)+Hojarasca+Cuerpo.de.Agua, uF.Lf[-17,])
mdlhA.Rh <- occu(~hora+I(hora^2) ~ poly(x,degree=2)+Hojarasca+Cuerpo.de.Agua, uF.Rh[-17,])
mdlhA.Hp <- occu(~hora ~ poly(x,degree=2)+Hojarasca+Cuerpo.de.Agua, uF.Hp[-17,])



###################################################
### code chunk number 49: ProbabilidadDeteccion
###################################################
layout(matrix(1:6,ncol=2))
nwdt <- data.frame(hora=(seq(12,22,length=30)-mean(hrs,na.rm=T))/sd(as.vector(hrs),na.rm=T),o=seq(12,22,length=30))

for (spp in c("Dm","SpA","Lf","Hp","Pb","Rh")) {
  mdl00 <- get(paste("mdlhA",spp,sep="."))
  matplot(nwdt$o,predict(mdl00,nwdt,type="det")[,-2],type="l",lty=c(1,2,2),ylim=c(0,1),main=spp)
}




###################################################
### code chunk number 50: Documento2_MonitoreoAcustico.Rnw:831-858
###################################################

nwdt <- data.frame(ik.Hj@coords,Hojarasca=ik.Hj@data[,1],Cuerpo.de.Agua=ik.Ag@data[,1])

prd02 <- ik
prd03 <- ik
prd04 <- ik
prd05 <- ik


colores <- colorRampPalette(brewer.pal(9,"Greens"))
layout(matrix(1:6,ncol=2))
par(mar=c(0,0,0,0))
##excluimos Rm porque no converge
for (spp in c("Dm","SpA","Lf","Hp","Pb","Rh")) {
  mdl00 <- get(paste("mdlhA",spp,sep="."))
  prd <- predict(mdl00,nwdt,type="state")
  prd02@data[,spp] <- prd$Predicted
  prd03@data[,spp] <- prd$SE
  prd04@data[,spp] <- prd$lower
  prd05@data[,spp] <- prd$upper
  image(prd02[spp],col=colores(30),breaks=seq(0,1,
                                    length=31))
  points(pts[ss,],col=1,pch=c(1,19)[pts@data[ss,spp]+1])
  legend("bottomright",legend=c("No detectado","Detectado"),col=1,pch=c(1,19),pt.cex=.7,title=spp,horiz=F,cex=.7)
}




###################################################
### code chunk number 51: Documento2_MonitoreoAcustico.Rnw:863-873
###################################################
colores <- colorRampPalette(brewer.pal(9,"Oranges"))
layout(matrix(1:6,ncol=2))
par(mar=c(0,0,0,0))
for (spp in c("Dm","SpA","Lf","Hp","Pb","Rh")) {
  image(prd03[spp],col=colores(30),breaks=seq(0,.3,
                                    length=31))
  points(pts[ss,],col=1,pch=c(1,19)[pts@data[ss,spp]+1])
  legend("bottomright",legend=c("No detectado","Detectado"),col=1,pch=c(1,19),pt.cex=.7,title=spp,horiz=F,cex=.7)
}



###################################################
### code chunk number 52: Documento2_MonitoreoAcustico.Rnw:878-905
###################################################

area.est$area.m2.occ <- round(c(prod(prd00@grid@cellsize)*sum(prd02[["Dm"]]),
                                prod(prd00@grid@cellsize)*sum(prd02[["SpA"]]),
                                prod(prd00@grid@cellsize)*sum(prd02[["Lf"]]),
                                prod(prd00@grid@cellsize)*sum(prd02[["Hp"]]),
                                prod(prd00@grid@cellsize)*sum(prd02[["Pb"]]),
                                prod(prd00@grid@cellsize)*sum(prd02[["Rm"]]),
                                prod(prd00@grid@cellsize)*sum(prd02[["Rh"]])),3)

area.est$lower.occ <- round(c(prod(prd00@grid@cellsize)*sum(prd04[["Dm"]]),
                                prod(prd00@grid@cellsize)*sum(prd04[["SpA"]]),
                                prod(prd00@grid@cellsize)*sum(prd04[["Lf"]]),
                                prod(prd00@grid@cellsize)*sum(prd04[["Hp"]]),
                                prod(prd00@grid@cellsize)*sum(prd04[["Pb"]]),
                                prod(prd00@grid@cellsize)*sum(prd04[["Rm"]]),
                                prod(prd00@grid@cellsize)*sum(prd04[["Rh"]])),3)

area.est$upper.occ <- round(c(prod(prd00@grid@cellsize)*sum(prd05[["Dm"]]),
                                prod(prd00@grid@cellsize)*sum(prd05[["SpA"]]),
                                prod(prd00@grid@cellsize)*sum(prd05[["Lf"]]),
                                prod(prd00@grid@cellsize)*sum(prd05[["Hp"]]),
                                prod(prd00@grid@cellsize)*sum(prd05[["Pb"]]),
                                prod(prd00@grid@cellsize)*sum(prd05[["Rm"]]),
                                prod(prd00@grid@cellsize)*sum(prd05[["Rh"]])),3)
area.est
 



