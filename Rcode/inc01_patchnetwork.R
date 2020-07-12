##rp0 <- bosque>40
 frags <- clump(bosque>40)

v0 <- table(values(frags))
x0 <- aggregate(coordinates(frags),list(patch=values(frags)),mean)
merge(v0,x0,by.y="patch")
ptchs <- merge(v0,x0,by.x="row.names",by.y="patch")

plot(bosque>40)
with(ptchs,symbols(x,y,circles=sqrt(Freq),inches=.1,add=T))

g0 <-  dist(ptchs[,c("x","y")])
 m0 <- as.matrix(g0)
diag(m0) <- NA
r1 <- matrix(rep(1:nrow(m0),ncol(m0)),ncol=ncol(m0))
c1 <- matrix(rep(1:ncol(m0),rep(nrow(m0),ncol(m0))),
             ncol=ncol(m0))

layout(matrix(1:4,ncol=2))

plot(bosque>40)

plot(bosque>40)
with(ptchs,symbols(x,y,circles=sqrt(Freq),inches=.1,add=T))

plot(bosque>40)
segments(ptchs[r1[m0<.025],"x"],ptchs[r1[m0<.025],"y"],
         ptchs[c1[m0<.025],"x"],ptchs[c1[m0<.025],"y"],col=2)

plot(bosque>40)
segments(ptchs[r1[m0<.01],"x"],ptchs[r1[m0<.01],"y"],
         ptchs[c1[m0<.01],"x"],ptchs[c1[m0<.01],"y"],col=4)

el <- cbind(r1[m0<.01],c1[m0<.01])
el <- el[!is.na(el[,1]) & !is.na(el[,2]),]
g0 <- graph.edgelist(el, directed=TRUE)
q0 <- cliques(g0)

##require(gdistance)

##rblock <- rpatch
##rblock[] <- 1

##if (!exists("tr1"))
##  tr1 <- transition(rblock, transitionFunction=mean, directions=8)
##image(transitionMatrix(tr1))
