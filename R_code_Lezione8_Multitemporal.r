# ANALISI MULTITEMPORALE
#stima di deforestazione di una certa area

setwd("~/Desktop/lab")

#chiamiamo la libreria raster
library(raster)

#con questa funzione faremo leggere le immagini al programma;
defor1<- brick("defor1.jpg")

defor2 <- brick("defor2.jpg")

#vediamo le caratteristiche del nostro file
defor1

#names: defor1.1, defor1.2, defor1.3 
#1 NIR
#2 RED
#3 GREEN

#creiamo un plot RGB
plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

#vediamo la copertura della foresta pluviale

defor2

#names: defor2.1, defor2.2, defor2.3 

#1 NIR
#2 RED
#3 GREEN

#creiamo un plot RGB
plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

#vediamo nella seconda immagine l'antropizzazione della zona analizzata

#facciamo un multiframe per vedere le 2 immagini a confronto

par(mfrow=c(2,1))

plotRGB(defor1,r=1,g=2,b=3,stretch="Lin")

plotRGB(defor2,r=1,g=2,b=3,stretch="Lin")

#classifichiamo l'immagine per riuscire a distinguere le classi, classificazione non supervisionata non diciamo cosa è cosa;
#per la classificazione abbiamo bisogno della libreria RStoolbox
library(RStoolbox)

d1c <- unsuperClass(defor1,nClasses = 2)

plot(d1c$map)
 cl1 <- colorRampPalette(c("dark green","pink"))(100)
 plot(d1c$map,col=cl1)

 #classificazione della seconda immagine
 
d2c <- unsuperClass(defor2,nClasses = 2)
cl2 <- colorRampPalette(c("pink","dark green"))(100)
plot(d2c$map,col=cl2)

#dark green <- Foresta
#pink <-zona antropizzata

#mettiamoli a confronto con multiframe;

par(mfrow=c(2,1))

plot(d1c$map,col=cl1)
plot(d2c$map,col=cl2)

dev.off()

#frequenza delle 2 mappe, 

freq(d1c$map)

#value   count
# 1     307552
# 2     33740

totd1 <- 307552+33740

freq(d2c$map)

#value   count
# 1     164353
# 2     178373

totd2 <- 164353+178373

#percentuale di foresta = frequanza prima mappa per 100 diviso il totale

percent1 <- freq(d1c$map)*100/totd1

percent1

#percentuali
#foreste <- 90.1
#zone aperte <- 9.8


percent2 <- freq(d2c$map)*100/totd2

percent2

#percentuali:
#Foreste <- 52.04
#zone aperte <- 47,95

cover <- c("Agriculture","Forest")

before <- c(9.8,90.2)
after <- c(47.95,52.04)
output <- data.frame(cover,before,after)
View(output)
