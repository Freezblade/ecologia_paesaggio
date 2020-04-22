#INTERPOLATION


#exercise: usiamo il vecchio sript sul covid e andiamo a plottare la mappa di densità;
#carichiamo il vecchio script "Lezione spatial"
setwd("~/Desktop/lab")
load("Lezione spatial.R")
ls()
covid <- read.table("covid_agg.csv",header = T)
attach(covid)

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)

library(spatstat)
covids <- ppp(lon,lat,c(-180,180),c(-90,90))
d <- density(covids)
plot(d)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

###interpolazione

covid
#creiamo dei valori per interpolazione in base ad etichette per ogni paese

#usiamo il point pattern di ppp, e associamo alla colonna cases del dataset covid
#se fai attach non si deve scrivere covid$cases ma solo cases
marks(covids) <- cases

#creiamo la mappa con la funzione smooth
s <- Smooth(covids)

# Exercise: plot(s) with points and coastlines

plot(s,col=cl1,main="Covid cases estimate")
cl1 <- colorRampPalette(c('purple', 'pink', 'light pink')) (200) 

points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

#stima non della densità di punti ma di casi nel mondo;

dev.off()

#mappa finale, unico fragico con entrambi i plot

par(mfrow=c(2,1))

#densità 


library(spatstat)
covids <- ppp(lon,lat,c(-180,180),c(-90,90))
d <- density(covids)
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

#interpolazione

plot(s,col=cl1,main="Covid cases estimate")
cl1 <- colorRampPalette(c('purple', 'pink', 'light pink')) (200) 

points(covids)

library(rgdal)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines,add=T)

dev.off()

#carichiamo un nuovo set di dati messi nella nostra cartella lab;

load("Tesi(1).RData")
ls()
#per fissare i nostri dati 
attach(Tesi)

#visualizziamo le prime 6 righe della tabella
head(Tesi)

#richiamiamo la libraria spatstat
library(spatstat)

#facciamo il nostro point pattern

#per vedere il sommario dei nostri dati 
summary(Tesi)

tesip <- ppp(Longitude,Latitude,c(12.41,12.47),c(43.90,43.95))

dt <- density(tesip)
plot(dt)
cl2 <- colorRampPalette(c("pink","purple","light pink"))

plot(dt,col=cl2)

points(tesip)
