#Landcover lezione 21/04

#settiamo la nostra directory
setwd("~/Desktop/lab")

#richiamiamo la libreria raster
library(raster)

#recuperiamo le immagini che sono di nostro interesse contenute nella nostra working directory
p224r63_2011 <- brick("p224r63_2011_masked.grd")

#chiamiamo la libreria RStoolbox
library(RStoolbox)

plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")

#unsuperClass, l'mmagine di partenza e numero di classi 

p224r63_2011c <- unsuperClass(p224r63_2011,nClasses = 4)

#visualizziamo ciò che abbiamo svolto, informazione sull'immagine; si unisce la mappa al modello 
p224r63_2011c

#plottiamo la nostra mappa, i colori interi ci mostrano le nostre 4 classi;
plot(p224r63_2011c$map)

#cambiamo i colori del nostro grafico così da avere una migliore interpretazione del grafico

clclass <- colorRampPalette(c('green',"red","blue","black"))(100)

plot(p224r63_2011c$map,col=clclass)
