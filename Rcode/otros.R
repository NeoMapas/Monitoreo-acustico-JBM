##system(sprintf("wget 'https://docs.google.com/spreadsheets/d/1QKSdOTV2zJwbdKdXQwp77LSJm-PbnT8NYsKnIeGUd4M/pubhtml?gid=1689083821&exportFormat=csv&single=true' --output-document=~/CEBA/data/JardinBotanicoMaracaibo/MonitoreoAnfibiosPuntos.csv")) ## no funciona
##https://docs.google.com/spreadsheets/d/1QKSdOTV2zJwbdKdXQwp77LSJm-PbnT8NYsKnIeGUd4M/pubhtml?gid=667748059&single=true
##https://docs.google.com/spreadsheets/d/1QKSdOTV2zJwbdKdXQwp77LSJm-PbnT8NYsKnIeGUd4M/pubhtml?gid=1959322558&single=true



##plot(JBM,col="darkgreen",border="darkgreen")
##points(Puntos)

ZJBM <- shapefile("~/CEBA/data/JardinBotanicoMaracaibo/ZonificacionJardin.shp")
plot(ZJBM[ZJBM@data$id!=1,])
rnull <- raster(extent(ZJBM))
rnull <- disaggregate(rnull,5)
rzona <- rasterize(ZJBM[ZJBM@data$id!=1,],rnull)
rzona <- rzona>0
xys <- coordinates(rzona)

values(rnull) <- rep(0:3,ncell(rnull)/4)
plot(rnull)
points(Puntos)

plot(hclust(as.dist(pointDistance(Puntos,longlat=T)),method="single"))
require(plotrix)

plot(ZJBM[ZJBM@data$id %in% c(7,8,9),])
plot(ZJBM[ZJBM@data$id %in% c(7,8,11),])
points(Puntos)

JBM.xy <- spTransform(ZJBM,CRS("+proj=utm +zone=19n +datum=WGS84"))
pts <- spTransform(Puntos,CRS("+proj=utm +zone=19n +datum=WGS84"))


draw.arc(x=pts@coords[,1],
         y=pts@coords[,2],
         radius=pts@data$Incertidumbre,
         deg1=0,deg2=360,lty=2,col=c("grey77",1)[1+(pts@data$Anotador != "No muestreado")])

plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),],col="green",border="wheat")

ss <- pts$Código %in% Muestreo$Punto

points(pts[ss,],col=c("grey77",1)[1+(pts@data$Anotador[ss] != "No muestreado")],pch=3)
draw.arc(x=pts@coords[ss,1],
         y=pts@coords[ss,2],
         radius=pts@data$Incertidumbre[ss],
         deg1=0,deg2=360,lty=2,col=c("grey77",1)[1+(pts@data$Anotador[ss] != "No muestreado")])


plot(JBM.xy[JBM.xy@data$id %in% c(7,8,11),],col="green",border="wheat")
ss <- pts$Código %in% Grabaciones$Punto


points(pts[ss,],col=c("grey77",1)[1+(pts@data$Anotador[ss] != "No muestreado")],pch=3)
draw.arc(x=pts@coords[ss,1],
         y=pts@coords[ss,2],
         radius=pts@data$Incertidumbre[ss],
         deg1=0,deg2=360,lty=2,col=c("grey77",1)[1+(pts@data$Anotador[ss] != "No muestreado")])


table(Puntos$Código %in% Grabaciones$Punto)
